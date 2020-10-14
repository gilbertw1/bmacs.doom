;;; ~/.doom.d/+indent.el -*- lexical-binding: t; -*-

(defvar bmacs|yank-indent-threshold 1000 "don't auto indent over 1000 lines")

(defvar bmacs|indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "modes to limit auto indentation on")

(defmacro bmacs|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
  The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defvar bmacs-indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "modes to limit auto indentation on")

(defun bmacs|yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) bmacs|yank-indent-threshold)
      (indent-region beg end nil)))

(bmacs|advise-commands
  "indent" (evil-paste-before evil-paste-after) around
  "If current mode is not one of bmacs-indent-sensitive-modes
  indent yanked text (with universal arg don't indent)."
  ad-do-it
  (evil-with-single-undo
    (if (and (not (equal '(4) (ad-get-arg 0)))
             (not (member major-mode bmacs-indent-sensitive-modes)) ;; TODO indent-sensitive-modes
             (derived-mode-p 'prog-mode))
        (let ((transient-mark-mode nil)
              (save-undo buffer-undo-list))
          (bmacs|yank-advised-indent-function (region-beginning)
                                                (region-end))))))

(setq-default tab-width 2
              c-basic-offset 4
              coffee-tab-width 2
              javascript-2-level 2
              js-2-level 2
              js2-basic-offset 2
              js-indent-level 2
              typescript-indent-level 2
              web-mode-markup-2-offset 2
              web-mode-css-2-offset 2
              web-mode-code-2-offset 2
              css-2-offset 2
              rust-indent-offset 4
              evil-shift-width 2)
