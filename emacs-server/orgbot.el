;;; -*- lexical-binding: t -*-

(require 'json)
(require 'request)

(defgroup my-orgbot nil "OrgBot configuration."
  :prefix "my-orgbot-"
  :group 'my)

(defcustom my-orgbot-token nil
  "OrgBot Telegram token."
  :type 'string
  :group 'my-orgbot)

(defcustom my-orgbot-users nil
  "Users OrgBot responds to."
  :type '(repeat string)
  :group 'my-orgbot)

(defun my-orgbot-capture (tree)
  "Capture TREE with no user interaction.

Returns t on success, nil on error."
  (condition-case nil
      (let ((org-capture-templates-contexts)
            (org-capture-templates
             `((t nil entry (file org-default-notes-file) ,tree
                  :immediate-finish t))))
        (org-capture nil t)
        t)
    (error)))

(defun my-orgbot-backlog-count ()
  "Count number of entries in the backlog."
  (with-current-buffer (find-file-noselect org-default-notes-file)
    (length (org-map-entries nil nil 'file))))

(defun my-orgbot-reply (command)
  "Reply to COMMAND."
  (pcase command
    ("/start")
    ((pred (string-prefix-p "/"))
     "Unknown command")
    (_
     (when-let ((buffer (get-file-buffer org-default-notes-file)))
       (with-current-buffer buffer
         (revert-buffer t t)))
     (if (my-orgbot-capture
          (if-let ((url (with-temp-buffer
                          (insert command)
                          (and (equal (bounds-of-thing-at-point 'url)
                                      (cons (point-min) (point-max)))
                               (buffer-string)))))
              (my-org-capture-current-link 'entry url)
            (my-org-capture-tree command)))
         (format "Added backlog item #%d" (my-orgbot-backlog-count))
       (format "Capture failed")))))

(defun my-orgbot-error (error_code description)
  "Signal an error with ERROR_CODE and DESCRIPTION."
  (when (or error_code description)
    (message "Org Bot Error: %s %s" error_code description))
  (signal 'error nil))

(defun my-orgbot-call-method (method params &optional success error)
  "Call METHOD of Telegram API.

PARAMS are query parameters passed in the URL.

SUCCESS is a callback called on the result of the call if it was
successful.

ERROR is a callback called when the call returned an error."
  (request (format "https://api.telegram.org/bot%s/%s" my-orgbot-token method)
    :params params
    :parser 'json-read
    :error (lambda (&rest args)
             (when error
               (funcall error))
             (-let (((&plist :data (&alist 'error_code 'description))
                     args))
               (my-orgbot-error error_code description)))
    :success (lambda (&rest args)
               (-let (((&plist
                        :data (&alist 'ok 'error_code 'description 'result))
                       args))
                 (cond ((not (eq ok t))
                        (my-orgbot-error error_code description))
                       (success
                        (funcall success result)))))))

(defvar my-orgbot-response nil
  "Response object to the currently polling update request.")

(defun my-orgbot-poll (&optional confirmed-id)
  "Poll for the next update to handle.

If CONFIRMED-ID is non-nil, send it to the server to confirm the
previous update."
  (setq my-orgbot-response
        (my-orgbot-call-method
         "getUpdates"
         `((limit . 1)
           (timeout . 60)
           ,@(when confirmed-id
               `((offset . ,(1+ confirmed-id)))))
         (lambda (result)
           (setq my-orgbot-response nil)
           (if (zerop (length result))
               (my-orgbot-poll)
             (-let (((&alist
                      'update_id
                      'message (&alist
                                'message_id
                                'from (&alist 'username)
                                'chat (&alist 'id chat_id)
                                'text))
                     (aref result 0)))
               (when-let ((reply (and (member username my-orgbot-users)
                                      (my-orgbot-reply text))))
                 (my-orgbot-call-method "sendMessage"
                                        `((chat_id . ,chat_id)
                                          (text . ,reply))))
               (my-orgbot-poll update_id))))
         (lambda ()
           (setq my-orgbot-response nil)))))

(defun my-orgbot-start ()
  "Start OrgBot server."
  (interactive)
  (when my-orgbot-response
    (user-error "OrgBot server is already running"))
  (my-orgbot-poll)
  (message "OrgBot is started"))

(defun my-orgbot-stop ()
  "Stop OrgBot server."
  (interactive)
  (unless my-orgbot-response
    (user-error "OrgBot server is not running"))
  (request-abort my-orgbot-response)
  (setq my-orgbot-response nil)
  (message "OrgBot is stopped"))
