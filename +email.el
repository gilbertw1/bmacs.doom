;;; ~/.doom.d/+email.el -*- lexical-binding: t; -*-

(after! mu4e
  (setq mu4e-get-mail-command "mbsync-update-fast"
        mu4e-bookmarks `(("maildir:/fastmail/INBOX OR maildir:/gmail/INBOX OR maildir:/stream/INBOX" "Inbox" ?i)
                         ("maildir:/fastmail/Drafts OR maildir:/gmail/[Gmail]/.Drafts OR maildir:/stream/[Gmail]/.Drafts" "Drafts" ?d)
                         ("flag:unread AND maildir:/fastmail/INBOX OR maildir:/gmail/INBOX OR maildir:/stream/INBOX" "Unread messages" ?u)
                         ("flag:flagged" "Starred messages" ?s)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("mime:image/*" "Messages with images" ?p)))

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; Change refile to an 'A'
  (setq mu4e-marks (assq-delete-all 'refile mu4e-marks))
  (push '(refile
          :char ("A" . "▶")
          :prompt "archive"
          :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
          :action (lambda (docid msg target) (mu4e~proc-move docid
                                                        (mu4e~mark-check-target target) "-N")))
        mu4e-marks)

  ;; Special handling for fastmail, don't add +T and move to trash
  ;; Gmail is configured to automatically add deleted emails to the trash
  (setq mu4e-marks (assq-delete-all 'delete mu4e-marks))
  (push '(delete
          :char ("D" . "❌")
          :prompt "Delete"
          :show-target (lambda (target) "delete")
          :action (lambda (docid msg target)
                    (let ((maildir (mu4e-message-field msg :maildir)))
                      (cond ((string-prefix-p "/fastmail" maildir)
                             (mu4e~proc-move docid (mu4e-get-trash-folder msg) "-N"))
                            ((or (string-prefix-p "/gmail" maildir) (string-prefix-p "/stream" maildir))
                             (message "GMAIL - %s" maildir)
                             (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
                            (true (mu4e~proc-remove docid))))))
        mu4e-marks)
  ;(shell-command (format "expunge-email-id %s" (mu4e-message-field msg :message-id)))

  (defun bmail-refile-folder-function (msg)
    "Set the refile folder for MSG."
    (let ((maildir (mu4e-message-field msg :maildir)))
      (cond
       ((string-prefix-p "/fastmail" maildir)
        "/fastmail/Archive")
       ((string-prefix-p "/gmail" maildir)
        "/gmail/[Gmail]/All Mail")
       ((string-prefix-p "/stream" maildir)
        "/stream/[Gmail]/All Mail")
       (t nil))))

  (defun bmail-trash-folder-function (msg)
    "Set the trash folder for MSG."
    (let ((maildir (mu4e-message-field msg :maildir)))
      (cond
       ((string-prefix-p "/fastmail" maildir)
        "/fastmail/Trash")
       ((string-prefix-p "/gmail" maildir)
        "/gmail/[Gmail]/.Trash")
       ((string-prefix-p "/stream" maildir)
        "/stream/[Gmail]/.Trash")
       (t nil))))

(defun bmacs/file-as-string (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

  (defun email-signature (address)
    (bmacs/file-as-string
     (expand-file-name (concat "~/.signatures/" address))))

  (setq mu4e-refile-folder #'bmail-refile-folder-function
        mu4e-trash-folder #'bmail-trash-folder-function
        smtpmail-debug-info t
        smtpmail-stream-type 'ssl
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-user "gilbertw1@fastmail.com"
        smtpmail-smtp-service 465)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "fastmail"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/fastmail" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder          . "/fastmail/Sent")
                    (mu4e-drafts-folder        . "/fastmail/Drafts")
                    (smtpmail-smtp-user        . "gilbertw1@fastmail.com")
                    (smtpmail-smtp-server      . "smtp.fastmail.com")
                    (smtpmail-stream-type      . ssl)
                    (smtpmail-smtp-service     . 465)
                    (user-mail-address         . "bryan@bryan.sh")
                    (user-full-name            . "Bryan Gilbert")
                    (mu4e-compose-signature    . (email-signature "bryan@bryan.sh"))))
          ,(make-mu4e-context
            :name "gmail"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder          . "/gmail/[Gmail]/.Sent Mail")
                    (mu4e-drafts-folder        . "/gmail/[Gmail]/.Drafts")
                    (smtpmail-smtp-user        . "gilbertw1@gmail.com")
                    (smtpmail-smtp-server      . "smtp.gmail.com")
                    (smtpmail-stream-type      . starttls)
                    (smtpmail-smtp-service     . 587)
                    (user-mail-address         . "gilbertw1@gmail.com")
                    (user-full-name            . "Bryan Gilbert")
                    (mu4e-compose-signature    . (email-signature "gilbertw1@gmail.com"))))
          ,(make-mu4e-context
            :name "stream"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/stream" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder          . "/stream/[Gmail]/.Sent Mail")
                    (mu4e-drafts-folder        . "/stream/[Gmail]/.Drafts")
                    (smtpmail-smtp-user        . "bryan@stre.am")
                    (smtpmail-smtp-server      . "smtp.gmail.com")
                    (smtpmail-stream-type      . starttls)
                    (smtpmail-smtp-service     . 587)
                    (user-mail-address         . "bryan@stream.live")
                    (user-full-name            . "Bryan Gilbert")
                    (mu4e-compose-signature    . (email-signature "bryan@stream.live")))))))
