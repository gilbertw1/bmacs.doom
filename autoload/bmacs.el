;; config/bmacs/autoload/bmacs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bmacs/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +bmacs/search-project (&optional arg)
  "Conduct a text search in files under the project root.
If prefix ARG is set, prompt for a project to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (if-let* ((projects (projectile-relevant-known-projects)))
                  (completing-read "Switch to project: " projects
                                   nil t nil nil (doom-project-root))
                (user-error "There are no known projects"))
            default-directory)))
    (call-interactively #'+ivy/project-search)))

;;;###autoload
(defun bmacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;;;###autoload
(defun bmacs/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

  When NEW-FILENAME is not specified, asks user for a new name.

  Also renames associated buffer (if any exists), invalidates
  projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;;;###autoload
(defun bmacs/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

  Also kills associated buffer (if any exists) and invalidates
  projectile cache when it's possible.

  When ASK-USER is non-nil, user will be asked to confirm file
  removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

;;;###autoload
(defun bmacs/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

  FILENAME is deleted using `bmacs/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'bmacs/delete-file filename t))

(defun bmacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
  current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))
                            (persp-contain-buffer-p buffer)))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;;;###autoload
(defun +bmacs/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))
;;;###autoload
(defun +bmacs/browse-project ()
  (interactive) (doom-project-browse (doom-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +bmacs/browse-templates ()
  (interactive) (doom-project-browse +file-templates-dir))
;;;###autoload
(defun +bmacs/find-in-templates ()
  (interactive) (doom-project-find-file +file-templates-dir))

;;;###autoload
(defun +bmacs/browse-emacsd ()
  (interactive) (doom-project-browse doom-emacs-dir))
;;;###autoload
(defun +bmacs/find-in-emacsd ()
  (interactive) (doom-project-find-file doom-emacs-dir))

;;;###autoload
(defun +bmacs/browse-notes ()
  (interactive) (doom-project-browse org-directory))
;;;###autoload
(defun +bmacs/find-in-notes ()
  (interactive) (doom-project-find-file org-directory))

;;;###autoload
(defun +bmacs/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +bmacs/man-or-woman ()
  "Invoke `man' if man is installed, otherwise use `woman'."
  (interactive)
  (call-interactively
   (if (executable-find "man")
       #'man
     #'woman)))

;;;###autoload
(defalias '+bmacs/newline #'newline)
;;;###autoload
(defun +bmacs/project-tasks ()
  "Invokes `+ivy/tasks' or `+helm/tasks', depending on which is available."
  (interactive)
  (cond ((featurep! :completion ivy) (+ivy/tasks))
        ((featurep! :completion helm) (+helm/tasks))))

;;;###autoload
(defun +bmacs/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun +bmacs/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

;;;###autoload
(defun +bmacs/yank-pop ()
  "Interactively select what text to insert from the kill ring."
  (interactive)
  (call-interactively
   (cond ((fboundp 'counsel-yank-pop)    #'counsel-yank-pop)
         ((fboundp 'helm-show-kill-ring) #'helm-show-kill-ring)
         ((error "No kill-ring search backend available. Enable ivy or helm!")))))

;;;###autoload
(defun +bmacs*newline-indent-and-continue-comments (_orig-fn)
  "Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support."
  (interactive)
  (cond ((sp-point-in-string) (newline))
        ((and (sp-point-in-comment)
              comment-line-break-function)
         (funcall comment-line-break-function))
        (t
         (newline nil t)
         (indent-according-to-mode))))

;;;###autoload
(defun doom--backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context (ignore-errors (sp-get-thing)))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (not (bolp))
                (not (sp-point-in-string))
                (save-excursion (>= (- (skip-chars-backward " \t")) tab-width)))
           (let ((movement (% (current-column) tab-width)))
             (when (= movement 0)
               (setq movement tab-width))
             (delete-char (- movement)))
           (unless (memq (char-before) (list ?\n ?\ ))
             (insert " ")))

          ;; Otherwise do a regular delete
          ((delete-char -1)))))


;;;###autoload
(defun +workspace/new-with-name (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive "iP")
  (unless name
    (setq name (read-string "Workspace Name: ")))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (+workspace-switch name t)
             (+workspace/display)))
    ((debug error) (+workspace-error (cadr e) t))))


;;;###autoload
(defun bmacs/delete-git-index-lock ()
  "Deletes index.lock file for git project if it exists"
  (interactive)
  (let ((git-index-lock-file (concat (magit-git-dir) "index.lock")))
    (when (file-exists-p git-index-lock-file)
      (delete-file git-index-lock-file))))

;;;###autoload
(defun bmacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
  current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))
                            (persp-contain-buffer-p buffer)))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;;;###autoload
(defun bmacs/workspace-switch-last ()
  "Switches to the last workspace"
  (interactive)
  (if (+workspace-exists-p +workspace--last)
      (+workspace-switch +workspace--last)
    (error "No previous workspace.")))
