;;; gptel-transient.el --- Integrations for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integrations with related packages for gptel.  To use these, run
;;
;; (require 'gptel-integrations)
;;
;; For MCP integration:
;; - Run M-x `gptel-mcp-connect' and M-x `gptel-mcp-disconnect', OR
;; - Use gptel's tools menu, M-x `gptel-tools', OR
;; - Access tools from `gptel-menu'

;;; Code:
(require 'gptel)
(require 'cl-lib)
(eval-when-compile (require 'transient))

;;;; MCP integration - requires the mcp package
(declare-function mcp-hub-get-all-tool "mcp-hub")
(declare-function mcp-hub-get-servers "mcp-hub")
(declare-function mcp-hub-start-all-server "mcp-hub")
(declare-function mcp-stop-server "mcp")
(declare-function mcp-hub "mcp-hub")
(declare-function mcp--status "mcp-hub")
(declare-function mcp--tools "mcp-hub")
(declare-function mcp--prompts "mcp-hub")
(declare-function mcp--resources "mcp-hub")
(declare-function mcp-get-prompt "mcp-hub")
(declare-function mcp-make-text-tool "mcp-hub")
(declare-function mcp-read-resource "mcp-hub")
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

(defun gptel-mcp-connect (&optional servers server-callback interactive)
  "Add gptel tools from MCP servers using the mcp package.

MCP servers are started if required.  SERVERS is a list of server
names (strings) to connect to.  If nil, all known servers are
considered.

If INTERACTIVE is non-nil (or called interactively), guide the user
through setting up mcp, and query for servers to retrieve tools from.

Call SERVER-CALLBACK after starting MCP servers."
  (interactive (list nil nil t))
  (if (locate-library "mcp-hub")
      (unless (require 'mcp-hub nil t)
        (user-error "Could not load `mcp-hub'!  Please install\
 or configure the mcp package"))
    (user-error "Could not find mcp!  Please install or configure the mcp package"))
  (if (null mcp-hub-servers)
      (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
    (setq servers
          (if servers
              (mapcar (lambda (s) (assoc s mcp-hub-servers)) servers)
            mcp-hub-servers))
    (let ((unregistered-servers ;Available servers minus servers already registered with gptel
           (cl-loop for server in servers
                    with registered-names =
                    (cl-loop for (cat . _tools) in gptel--known-tools
                             if (string-prefix-p "mcp-" cat)
                             collect (substring cat 4))
                    unless (member (car server) registered-names)
                    collect server)))
      (if unregistered-servers
          (let* ((servers
                  (if interactive
                      (let ((picks
                             (completing-read-multiple
                              "Add tools from MCP servers (separate with \",\"): "
                              (cons '("ALL") unregistered-servers) nil t)))
                        (if (member "ALL" picks)
                            unregistered-servers
                          (mapcar (lambda (s) (assoc s mcp-hub-servers)) picks)))
                    unregistered-servers))
                 (server-active-p
                  (lambda (server) (gethash (car server) mcp-server-connections)))
                 (inactive-servers (cl-remove-if server-active-p servers))
                 (add-all-tools
                  (lambda (&optional server-names)
                    "Register and add tools from servers.  Report failures."
                    (let ((tools (gptel-mcp--get-tools server-names))
                          (now-active (cl-remove-if-not server-active-p mcp-hub-servers)))
                      (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
                      (gptel-mcp--activate-tools tools)
                      ;; Update known MCP prompts and resources
                      (gptel-mcp--update-known-prompts server-names)
                      (gptel-mcp--update-known-resources server-names)
                      (if-let* ((failed (cl-set-difference inactive-servers now-active
                                                           :test #'equal)))
                          (progn
                            (message "Inactive-before: %S, Now-Active: %S" inactive-servers now-active)
                            (message (substitute-command-keys
                                      "%d/%d server%s failed to start: %s.  Run \\[mcp-hub] to investigate.")
                                     (length failed) (length inactive-servers)
                                     (if (= (length failed) 1) "" "s")
                                     (mapconcat #'car failed ", ")))
                        (let ((added (or server-names (mapcar #'car now-active))))
                          (message "Added %d tools from %d MCP server%s: %s"
                                   (length tools) (length added)
                                   (if (= (length added) 1) "" "s")
                                   (mapconcat #'identity added ", "))))
                      (when (functionp server-callback) (funcall server-callback))))))

            (if inactive-servers        ;start servers
                (mcp-hub-start-all-server
                 add-all-tools (mapcar #'car inactive-servers))
              (funcall add-all-tools (mapcar #'car servers))))
        (message "All MCP tools are already available to gptel!")
        ;; Still update prompts and resources even if tools are already available
        (gptel-mcp--update-known-prompts)
        (gptel-mcp--update-known-resources)
        (when (functionp server-callback) (funcall server-callback))))))

(defun gptel-mcp-disconnect (&optional servers interactive)
  "Unregister gptel tools provided by MCP servers using the mcp package.

SERVERS is a list of server names (strings) to disconnect from.

If INTERACTIVE is non-nil, query the user about which tools to remove."
  (interactive (list nil t))
  (if-let* ((names-alist
             (cl-loop
              for (category . _tools) in gptel--known-tools
              if (and (string-match "^mcp-\\(.*\\)" category)
                      (or (null servers) ;Consider all if nil
                          (member (match-string 1 category) servers)))
              collect (cons (match-string 1 category) category))))
      (let ((remove-fn (lambda (cat-names)
                         (setq gptel-tools ;Remove from gptel-tools
                               (cl-delete-if (lambda (tool) (member (gptel-tool-category tool)
                                                               cat-names))
                                             gptel-tools))
                         (mapc (lambda (category) ;Remove from registry
                                 (setf (alist-get category gptel--known-tools
                                                  nil t #'equal)
                                       nil))
                               cat-names))))
        (if interactive
            (when-let* ((server-names
                         (completing-read-multiple
                          "Remove MCP server tools for (separate with \",\"): "
                          (cons '("ALL" . nil) names-alist)
                          nil t)))
              (when (member "ALL" server-names)
                (setq server-names (mapcar #'car names-alist)))
              (funcall remove-fn        ;remove selected tool categories
                       (mapcar (lambda (s) (cdr (assoc s names-alist))) server-names))
              (if (y-or-n-p
                   (format "Removed MCP tools from %d server%s.  Also shut down MCP servers?"
                           (length server-names)
                           (if (= (length server-names) 1) "" "s")))
                  (progn (mapc #'mcp-stop-server server-names)
                         (message "Shut down MCP servers: %S" server-names))
                (message "Removed MCP tools for: %S" server-names)))
          (funcall remove-fn (mapcar #'cdr names-alist))))
    ;; No MCP tools, ask to shut down servers
    (if (cl-loop
         for v being the hash-values of mcp-server-connections
         never v)
        (when interactive (message "No MCP servers active!"))
      (when (or (not interactive)
                (y-or-n-p "No MCP tools in gptel!  Shut down all MCP servers? "))
        (dolist (server mcp-hub-servers)
          (when (gethash (car server) mcp-server-connections)
            (mcp-stop-server (car server))))))))

(defun gptel-mcp--get-tools (&optional server-names)
  "Return tools from running MCP servers.

SERVER-NAMES is a list of server names to add tools from.  Add tools
from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (let ((servers (mapcar (lambda (n) (gethash n mcp-server-connections))
                         server-names)))
    (cl-mapcan
     (lambda (name server)
       (when (and server (equal (mcp--status server) 'connected))
         (when-let* ((tools (mcp--tools server))
                     (tool-names (mapcar #'(lambda (tool) (plist-get tool :name)) tools)))
           (mapcar (lambda (tool-name)
                     (plist-put (mcp-make-text-tool name tool-name t)
                                :category (format "mcp-%s" name)))
                   tool-names))))
     server-names servers)))

(defun gptel-mcp--get-prompts (&optional server-names)
  "Return prompts from running MCP servers.

SERVER-NAMES is a list of server names to get prompts from.  Get prompts
from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (let ((servers (mapcar (lambda (n) (gethash n mcp-server-connections))
                         server-names)))
    (cl-mapcan
     (lambda (_name server)
       (when (and server (equal (mcp--status server) 'connected))
         (when-let* ((prompts (mcp--prompts server)))
           (mapcar (lambda (prompt)
                     (list :name (plist-get prompt :name)
                           :description (plist-get prompt :description)))
                   prompts))))
     server-names servers)))

(defun gptel-mcp--get-resources (&optional server-names)
  "Return resources from running MCP servers.

SERVER-NAMES is a list of server names to get resources from.  Get resources
from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (let ((servers (mapcar (lambda (n) (gethash n mcp-server-connections))
                         server-names)))
    (cl-mapcan
     (lambda (_name server)
       (when (and server (equal (mcp--status server) 'connected))
         (when-let* ((resources (mcp--resources server)))
           (mapcar (lambda (resource)
                     (list :uri (plist-get resource :uri)
                           :name (plist-get resource :name)
                           :description (plist-get resource :description)))
                   resources))))
     server-names servers)))

(defun gptel-mcp--update-known-prompts (&optional server-names)
  "Update `gptel--known-mcp-prompts' with prompts from MCP servers.

SERVER-NAMES is a list of server names to update prompts for.  Update
prompts from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (dolist (server-name server-names)
    (when-let* ((server (gethash server-name mcp-server-connections))
                ((equal (mcp--status server) 'connected))
                (prompts (mcp--prompts server)))
      (setf (alist-get server-name gptel--known-mcp-prompts nil nil #'equal)
            (mapcar (lambda (prompt)
                      (list :name (plist-get prompt :name)
                            :description (plist-get prompt :description)))
                    prompts)))))

(defun gptel-mcp--update-known-resources (&optional server-names)
  "Update `gptel--known-mcp-resources' with resources from MCP servers.

SERVER-NAMES is a list of server names to update resources for.  Update
resources from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (dolist (server-name server-names)
    (when-let* ((server (gethash server-name mcp-server-connections))
                ((equal (mcp--status server) 'connected))
                (resources (mcp--resources server)))
      (setf (alist-get server-name gptel--known-mcp-resources nil nil #'equal)
            (mapcar (lambda (resource)
                      (list :uri (plist-get resource :uri)
                            :name (plist-get resource :name)
                            :description (plist-get resource :description)))
                    resources)))))

(defun gptel-mcp--activate-tools (&optional tools)
  "Activate TOOLS or all MCP tools in current gptel session."
  (unless tools (setq tools (gptel-mcp--get-tools)))
  (dolist (tool tools)
    (cl-pushnew (gptel-get-tool (list (plist-get tool :category)
                                      (plist-get tool :name)))
                gptel-tools)))

(with-eval-after-load 'gptel-transient
  (transient-define-suffix gptel--suffix-mcp-connect ()
    "Register tools provided by MCP servers."
    :key "M+"
    :description "Add MCP server tools"
    :transient t
    (interactive)
    ;; gptel-tools stores its state in its scope slot.  Retain the scope but
    ;; update it with the newly selected tools.  Then set up gptel-tools.
    (condition-case err
        (gptel-mcp-connect
         nil (lambda () (when-let* ((transient--prefix)
                               ((eq (oref transient--prefix command)
                                    'gptel-tools)))
                     (let ((state (transient-scope 'gptel-tools)))
                       (plist-put state :tools
                                  (delete-dups
                                   (nconc (mapcar (lambda (tool)
                                                    (list (gptel-tool-category tool)
                                                          (gptel-tool-name tool)))
                                                  gptel-tools)
                                          (plist-get state :tools))))
                       (transient-setup 'gptel-tools nil nil :scope state))))
         t)
      (user-error (message "%s" (cadr err)))))

  (transient-define-suffix gptel--suffix-mcp-disconnect ()
    "Remove tools provided by MCP servers from gptel."
    :key "M-"
    :description (lambda () (if (cl-some (lambda (cat) (string-match-p "^mcp-" cat))
                                    (map-keys gptel--known-tools))
                           "Remove MCP server tools"
                         "Shut down MCP servers"))
    :transient t
    :inapt-if
    (lambda () (or (not (boundp 'mcp-hub-servers))
              (null mcp-hub-servers)
              (cl-loop
               for v being the hash-values of mcp-server-connections
               never v)))
    (interactive)
    (call-interactively #'gptel-mcp-disconnect)
    ;; gptel-tools stores its state in its scope slot.  Retain the scope but
    ;; remove tools from it that no longer exist, then set up gptel-tools
    (cl-loop with state = (transient-scope 'gptel-tools)
             with tools = (plist-get state :tools)
             for tool-spec in tools
             if (map-nested-elt gptel--known-tools tool-spec)
             collect tool-spec into valid-tools
             finally do (plist-put state :tools valid-tools)
             (transient-setup 'gptel-tools nil nil :scope state)))

  (transient-define-suffix gptel--suffix-mcp-prompts ()
    "Manage MCP prompts."
    :key "Mp"
    :description "Manage MCP prompts"
    :transient t
    (interactive)
    (gptel-mcp-prompts))

  (transient-define-suffix gptel--suffix-mcp-resources ()
    "Manage MCP resources."
    :key "Mr"
    :description "Add MCP resources to context"
    :transient t
    (interactive)
    (gptel-mcp-resources))

  (transient-remove-suffix 'gptel-tools '(0 2))
  (transient-append-suffix 'gptel-tools '(0 -1)
    [""
     (gptel--suffix-mcp-connect)
     (gptel--suffix-mcp-disconnect)
     (gptel--suffix-mcp-prompts)
     (gptel--suffix-mcp-resources)]))

(defun gptel-mcp-prompts ()
  "Select and send MCP prompts via popup selection."
  (interactive)
  (if (null gptel--known-mcp-prompts)
      (message "No MCP prompts available. Connect to MCP servers first.")
    (let ((prompt-choices
           (cl-loop for (server-name . prompts) in gptel--known-mcp-prompts
                    append (mapcar (lambda (prompt)
                                     (cons (format "%s: %s"
                                                   server-name
                                                   (plist-get prompt :name))
                                           (cons server-name
                                                 (plist-get prompt :name))))
                                   prompts))))
      (if (null prompt-choices)
          (message "No MCP prompts available from connected servers.")
        (let ((selection (completing-read
                          "Select MCP prompt to send: "
                          prompt-choices nil t)))
          (when-let* ((choice (assoc selection prompt-choices))
                      (server-name (cadr choice))
                      (prompt-name (cddr choice)))
            (gptel--mcp-send-prompt server-name prompt-name)))))))

(defun gptel--mcp-prompt-description (server-name prompt-name)
  "Get description for PROMPT-NAME from SERVER-NAME."
  (when-let* ((server-prompts (alist-get server-name gptel--known-mcp-prompts nil nil #'equal))
              (prompt-spec (cl-find-if (lambda (p) (equal (plist-get p :name) prompt-name))
                                       server-prompts)))
    (or (plist-get prompt-spec :description) "No description available")))

(defun gptel--mcp-send-prompt (server-name prompt-name)
  "Send MCP prompt PROMPT-NAME from SERVER-NAME using gptel-request."
  (when-let* ((server (gethash server-name mcp-server-connections))
              ((equal (mcp--status server) 'connected)))
    (condition-case err
        (let* ((prompt-data (mcp-get-prompt server prompt-name nil))
               (prompt-text (when prompt-data
                              (if-let* ((messages (plist-get prompt-data :messages))
                                        (first-message (and (vectorp messages) (> (length messages) 0) (aref messages 0)))
                                        (content (plist-get first-message :content)))
                                  (if (stringp content)
                                      content
                                    (plist-get content :text))
                                ;; Fallback: try to extract any text content
                                (or (plist-get prompt-data :text)
                                    (format "%s" prompt-data)))))
               (gptel-buffer (or (cl-find-if (lambda (buf)
                                               (with-current-buffer buf gptel-mode))
                                             (buffer-list))
                                 (current-buffer))))
          (if prompt-text
              (progn
                (gptel-request prompt-text
                  :buffer gptel-buffer
                  :position (with-current-buffer gptel-buffer (point-max)))
                (message "Sent MCP prompt: %s from %s" prompt-name server-name))
            (message "Failed to get prompt %s from server %s" prompt-name server-name)))
      (error (message "Error sending MCP prompt: %s" (error-message-string err))))))

(defun gptel-mcp-resources ()
  "Select and add MCP resources to gptel context."
  (interactive)
  (if (null gptel--known-mcp-resources)
      (message "No MCP resources available. Connect to MCP servers first.")
    (let ((resource-choices
           (cl-loop for (server-name . resources) in gptel--known-mcp-resources
                    append (mapcar (lambda (resource)
                                     (cons (format "%s: %s"
                                                   server-name
                                                   (plist-get resource :name))
                                           (cons server-name
                                                 (plist-get resource :uri))))
                                   resources))))
      (if (null resource-choices)
          (message "No MCP resources available from connected servers.")
        (let ((selections (completing-read-multiple
                          "Select MCP resources to add to context (separate with \",\"): "
                          resource-choices nil t)))
          (when selections
            (dolist (selection selections)
              (when-let* ((choice (assoc selection resource-choices))
                          (server-name (cadr choice))
                          (resource-uri (cddr choice)))
                (gptel--mcp-add-resource-to-context server-name resource-uri)))))))))

(defun gptel--mcp-add-resource-to-context (server-name resource-uri)
  "Add MCP resource from SERVER-NAME with RESOURCE-URI to gptel context."
  (when-let* ((server (gethash server-name mcp-server-connections))
              ((equal (mcp--status server) 'connected)))
    (condition-case err
        (let* ((resource-data (mcp-read-resource server resource-uri))
               (content (when resource-data
                          (if-let* ((contents (plist-get resource-data :contents))
                                    (first-content (and (vectorp contents) (> (length contents) 0) (aref contents 0))))
                              (plist-get first-content :text)
                            ;; Fallback: try to extract any text content
                            (or (plist-get resource-data :text)
                                (format "%s" resource-data))))))
          (if content
              (let ((context-entry (list (format "mcp://%s/%s" server-name resource-uri)
                                        :mcp-resource t
                                        :server server-name
                                        :uri resource-uri
                                        :content content)))
                (cl-pushnew context-entry gptel-context--alist :test #'equal)
                (message "Added MCP resource: %s from %s to context" resource-uri server-name))
            (message "Failed to get resource %s from server %s" resource-uri server-name)))
      (error (message "Error adding MCP resource: %s" (error-message-string err))))))

(provide 'gptel-integrations)
;;; gptel-integrations.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
