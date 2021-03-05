;;; -*- lexical-binding: t -*-

(require 'json)
(require 'request)
(require 's)

;;; Customization Options

(defgroup my-orgbot nil "OrgBot configuration."
  :prefix "my-orgbot-"
  :group 'my)

(defcustom my-orgbot-heading-length-bound 100
  "Upper bound on length of captured headings."
  :type 'integer
  :group 'my-orgbot)

(defcustom my-orgbot-token nil
  "OrgBot Telegram token."
  :type 'string
  :group 'my-orgbot)

(defcustom my-orgbot-users nil
  "Users OrgBot responds to."
  :type '(repeat string)
  :group 'my-orgbot)

;;; Capture Command

(defun my-orgbot-url-p (url)
  "t if URL is a url."
  (with-temp-buffer
    (insert url)
    (equal (bounds-of-thing-at-point 'url)
           (cons (point-min) (point-max)))))

(defun my-orgbot-capture (text)
  "Capture TEXT with no user interaction.

Returns a textual response."
  (let* ((tree
          (let ((lines (s-lines text))
                (url))
            (if (my-orgbot-url-p (car lines))
                (setq url (pop lines))
              (when (> (length (car lines))
                       my-orgbot-heading-length-bound)
                (push (s-truncate my-orgbot-heading-length-bound
                                  (car lines))
                      lines))
              (let ((last-line (car (last lines))))
                (when (my-orgbot-url-p last-line)
                  (setq url last-line
                        lines (butlast lines)))))
            (let ((description
                   (with-temp-buffer
                     (insert (s-trim (s-join "\n" lines)))
                     (goto-char (point-min))
                     (forward-line)
                     (fill-region (point) (point-max))
                     (buffer-string))))
              (if url
                  (my-org-capture-current-link 'entry url description)
                (my-org-capture-tree description)))))
         (org-capture-templates-contexts)
         (org-capture-templates
          `((t nil entry (file org-default-notes-file) ,tree
               :immediate-finish t))))
    (condition-case nil
        (progn
          (when-let ((buffer (get-file-buffer org-default-notes-file)))
            (with-current-buffer buffer
              (revert-buffer t t)))
          (org-capture nil t)
          (->> tree
               (s-replace "%?" "")
               (s-replace "<" "&lt;")
               (s-replace "* NEW" "* <b>NEW</b>")
               (replace-regexp-in-string
                org-link-bracket-re
                (lambda (link)
                  (let ((url (match-string 1 link))
                        (description (match-string 2 link)))
                    (if description
                        (format "<a href=\"%s\">%s</a>" url description)
                      url))))))
      (error (format "Capture failed")))))

;;; Shopping Commands

(defun my-orgbot-buy (item)
  "Toggle ITEM in shopping list."
  (let* ((file-name (abbreviate-file-name
                     (expand-file-name "Shopping.org" org-directory)))
         (revert-without-query (list file-name))
         (reply))
    (with-current-buffer (find-file-noselect file-name)
      (goto-char (point-max))
      (while (and (not reply) (outline-previous-heading))
        (let* ((el (org-element-at-point))
               (title (org-element-property :title el))
               (todo-keyword (org-element-property :todo-keyword el)))
          (when (string-equal title item)
            (org-todo (if todo-keyword "" (car org-todo-keywords-1)))
            (save-buffer)
            (setq reply
                  (cons (format (if todo-keyword
                                    "Removed %S from the shopping list"
                                  "Added %S to the shopping list")
                                title)
                        (cdr (my-orgbot-shopping-list todo-keyword))))))))
    (or reply "No such item")))

(defun my-orgbot-shopping-list (with-keyword)
  "Get the shopping list.

If WITH-KEYWORD is non-nil, include items with a keyword,
otherwise include items without a keyword."
  (let* ((file-name (abbreviate-file-name
                     (expand-file-name "Shopping.org" org-directory)))
         (revert-without-query (list file-name))
         (shopping-list))
    (with-current-buffer (find-file-noselect file-name)
      (goto-char (point-max))
      (while (outline-previous-heading)
        (when-let* ((el (org-element-at-point))
                    (_ (eq (null (org-element-property :todo-keyword el))
                           (null with-keyword))))
          (push (org-element-property :title el) shopping-list))))
    (list (if with-keyword
              (s-join "\n" (--map (s-concat "* " it) shopping-list))
            "...")
          :keyboard (--map (s-concat "/buy " it) shopping-list))))

;;; Command Router

(defun my-orgbot-reply (command)
  "Reply to COMMAND."
  (pcase (s-split-up-to " " command 1)
    (`("/start"))
    (`("/buy" ,item) (my-orgbot-buy item))
    (`("/buy") (my-orgbot-shopping-list nil))
    (`("/shopping") (my-orgbot-shopping-list t))
    (`("/X") '("."))
    (`(,(pred (string-prefix-p "/")) . ,_) "Unknown command")
    (_ (my-orgbot-capture command))))

(defun my-orgbot-register-commands ()
  "Register my commands with the Telegram."
  (interactive)
  (my-orgbot-call-method
   "setMyCommands"
   `((commands
      . ,(json-serialize
          [((command . "shopping")
            (description . "get the shopping list"))
           ((command . "buy")
            (description . "toggle item in the shopping list"))])))))

;;; Telegram API

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

;;; Polling Loop

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
             (-let* (((&alist
                       'update_id
                       'message (&alist
                                 'message_id
                                 'from (&alist 'username)
                                 'chat (&alist 'id chat_id)
                                 'text))
                      (aref result 0))
                     (reply (if (member username my-orgbot-users)
                                (my-orgbot-reply text)
                              "I don't know you"))
                     ((text &keys :keyboard) (if (stringp reply)
                                                 (list reply)
                                               reply)))
               (my-orgbot-call-method
                "sendMessage"
                `((chat_id . ,chat_id)
                  (text . ,text)
                  (parse_mode . "HTML")
                  (disable_web_page_preview . "true")
                  (reply_markup
                   . ,(json-serialize
                       (if keyboard
                           `(one_time_keyboard t
                             resize_keyboard t
                             keyboard ,(vconcat
                                        (--map (vector `(text ,it))
                                               (cons "/X" keyboard))))
                         '(remove_keyboard t))))))
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
