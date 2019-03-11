;;; ~/.doom.d/+evil.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;
;; Evil Commands ;;
;;;;;;;;;;;;;;;;;;;

(after! evil
  (evil-define-operator evil-delete-char-without-register (beg end type reg)
    "delete character without yanking unless in visual mode"
    :motion evil-forward-char
    (interactive "<R><y>")
    (if (evil-visual-state-p)
        (evil-delete beg end type reg)
      (evil-delete beg end type ?_)))

  (evil-define-operator evil-delete-backward-char-without-register (beg end type _)
    "delete backward character without yanking"
    :motion evil-backward-char
    (interactive "<R><y>")
    (evil-delete beg end type ?_))

  (evil-define-operator evil-delete-without-register (beg end type _ _2)
    (interactive "<R><y>")
    (evil-delete beg end type ?_))

  (evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
    (interactive "<R><y>")
    (let ((text (replace-regexp-in-string "\n" "" (filter-buffer-substring beg end))))
      (if (string-match-p "^\\s-*$" text)
          (evil-delete beg end type ?_)
        (evil-delete beg end type reg yank-handler))))

  (evil-define-operator evil-delete-line-without-register (beg end type _ yank-handler)
    (interactive "<R><y>")
    (evil-delete-line beg end type ?_ yank-handler))

  (evil-define-operator evil-change-without-register (beg end type _ yank-handler)
    (interactive "<R><y>")
    (evil-change beg end type ?_ yank-handler))

  (evil-define-operator evil-change-line-without-register (beg end type _ yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line
    (interactive "<R><y>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))

  (evil-define-command evil-paste-after-without-register (count &optional register yank-handler)
    "evil paste before without yanking"
    :suppress-operator t
    (interactive "P<x>")
    (if (evil-visual-state-p)
        (evil-visual-paste-without-register count register)
      (evil-paste-after count register yank-handler)))

  (evil-define-command evil-paste-before-without-register (count &optional register yank-handler)
    "evil paste before without yanking"
    :suppress-operator t
    (interactive "P<x>")
    (if (evil-visual-state-p)
        (evil-visual-paste-without-register count register)
      (evil-paste-before count register yank-handler)))

  (evil-define-command evil-visual-paste-without-register (count &optional register)
    "Paste over Visual selection."
    :suppress-operator t
    (interactive "P<x>")
    ;; evil-visual-paste is typically called from evil-paste-before or
    ;; evil-paste-after, but we have to mark that the paste was from
    ;; visual state
    (setq this-command 'evil-visual-paste)
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0)))
           (yank-handler (car-safe (get-text-property
                                    0 'yank-handler text)))
           new-kill
           paste-eob)
      (evil-with-undo
        (let* ((kill-ring (list (current-kill 0)))
               (kill-ring-yank-pointer kill-ring))
          (when (evil-visual-state-p)
            (evil-visual-rotate 'upper-left)
            ;; if we replace the last buffer line that does not end in a
            ;; newline, we use `evil-paste-after' because `evil-delete'
            ;; will move point to the line above
            (when (and (= evil-visual-end (point-max))
                       (/= (char-before (point-max)) ?\n))
              (setq paste-eob t))
            (evil-delete-without-register evil-visual-beginning evil-visual-end
                                          (evil-visual-type))
            (when (and (eq yank-handler #'evil-yank-line-handler)
                       (not (eq (evil-visual-type) 'line))
                       (not (= evil-visual-end (point-max))))
              (insert "\n"))
            (evil-normal-state)
            (setq new-kill (current-kill 0))
            (current-kill 1))
          (if paste-eob
              (evil-paste-after count register)
            (evil-paste-before count register)))
        (kill-new new-kill)
        ;; mark the last paste as visual-paste
        (setq evil-last-paste
              (list (nth 0 evil-last-paste)
                    (nth 1 evil-last-paste)
                    (nth 2 evil-last-paste)
                    (nth 3 evil-last-paste)
                    (nth 4 evil-last-paste)
                    t))))))


;; Evil MC
(after! evil-mc
  (dolist
      (commands '((evil-change-without-register . ((:default . evil-mc-execute-default-evil-change)))
                  (evil-change-line-without-register . ((:default . evil-mc-execute-default-evil-change-line)))
                  (evil-delete-without-register . ((:default . evil-mc-execute-default-evil-delete)))
                  (evil-delete-without-register-if-whitespace . ((:default . evil-mc-execute-default-evil-delete)))
                  (evil-delete-char-without-register . ((:default . evil-mc-execute-default-evil-delete)))
                  (evil-delete-backward-char-without-register . ((:default . evil-mc-execute-default-evil-delete)))
                  (evil-delete-line-without-register . ((:default . evil-mc-execute-default-evil-delete)))
                  (evil-paste-after-without-register . ((:default . evil-mc-execute-default-evil-paste)))
                  (evil-paste-before-without-register . ((:default . evil-mc-execute-default-evil-paste)))))
    (push commands evil-mc-custom-known-commands)))

; Evil little word
(after! evil
  (defun maybe-define-category (cat doc &optional table)
    (unless (category-docstring cat table) (define-category cat doc table)))

  (let (uc lc defs (table (standard-category-table)))
    (map-char-table
     #'(lambda (key value)
         (when (natnump value)
           (let (from to)
             (if (consp key)
                 (setq from (car key) to (cdr key))
               (setq from (setq to key)))
             (while (<= from to)
               (cond ((/= from (downcase from))
                      (add-to-list 'uc from))
                     ((/= from (upcase from))
                      (add-to-list 'lc from)))
               (setq from (1+ from))))))
     (standard-case-table))
    (setq defs `(("Uppercase" ?U ,uc)
                 ("Lowercase" ?u ,lc)
                 ("Underscore" ?_ (?_))))
    (dolist (elt defs)
      (maybe-define-category (cadr elt) (car elt) table)
      (dolist (ch (car (cddr elt)))
        (modify-category-entry ch (cadr elt) table))))

  (defgroup evil-little-word nil
    "CamelCase and snake_case word movement support."
    :prefix "evil-little-word-"
    :group 'evil)

  (defcustom evil-little-word-separating-categories
    (append evil-cjk-word-separating-categories '((?u . ?U) (?_ . ?u) (?_ . ?U)))
    "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
    :type '((character . character))
    :group 'evil-little-word)

  (defcustom evil-little-word-combining-categories
    (append evil-cjk-word-combining-categories '())
    "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
    :type '((character . character))
    :group 'evil-little-word)

  (defmacro evil-with-little-word (&rest body)
    (declare (indent defun) (debug t))
    `(let ((evil-cjk-word-separating-categories
            evil-little-word-separating-categories)
           (evil-cjk-word-combining-categories
            evil-little-word-combining-categories))
       ,@body))

  (defun forward-evil-little-word (&optional count)
    "Forward by little words."
    (evil-with-little-word (forward-evil-word count)))

  (evil-define-motion evil-forward-little-word-begin (count)
    "Move the cursor to the beginning of the COUNT-th next little word."
    :type exclusive
    (evil-with-little-word (evil-forward-word-begin count)))

  (evil-define-motion evil-forward-little-word-end (count)
    "Move the cursor to the end of the COUNT-th next little word."
    :type inclusive
    (evil-with-little-word (evil-forward-word-end count)))

  (evil-define-motion evil-backward-little-word-begin (count)
    "Move the cursor to the beginning of the COUNT-th previous little word."
    :type exclusive
    (evil-with-little-word (evil-backward-word-begin count)))

  (evil-define-motion evil-backward-little-word-end (count)
    "Move the cursor to the end of the COUNT-th previous little word."
    :type inclusive
    (evil-with-little-word (evil-backward-word-end count)))

  (evil-define-text-object evil-a-little-word (count &optional beg end type)
    "Select a little word."
    (evil-select-an-object 'evil-little-word beg end type count))

  (evil-define-text-object evil-inner-little-word (count &optional beg end type)
    "Select inner little word."
    (evil-select-inner-object 'evil-little-word beg end type count))

  (define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
  (define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
  (define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
  (define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
  (define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
  (define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word))

;; from default config

(defun +default|disable-delete-selection-mode ()
  (delete-selection-mode -1))
(add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
(add-hook 'evil-insert-state-exit-hook  #'+default|disable-delete-selection-mode)
