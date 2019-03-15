;;; private/gilbertw1/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Iosevka" :size 24))

;; disable double buffering to prevent stuttering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(load! "+evil.el")
(load! "+hydra.el")
(load! "+bindings.el")
(load! "+banner.el")
                                        ;
;; avy use all windows
(setq avy-all-windows t)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (progn
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil)))

(use-package undo-propose
  :commands (undo-propose))

;; open *.rest files in restclient mode
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Override smerge colors
(custom-set-faces
  '(smerge-refined-removed ((t (:inherit 'smerge-mine))))
  '(smerge-refined-added   ((t (:inherit 'smerge-other)))))

;; command to open markdown files with
(setq markdown-open-command "typora")

;; remove xref lookup backend from specific major modes
(add-hook! (scala-mode)
  (setq-local +lookup-definition-functions
              '(+lookup-dumb-jump-backend
                +lookup-project-search-backend
                +lookup-evil-goto-definition-backend)))

;; set specific company backends for scala mode
(set-company-backend! 'scala-mode
  '(company-dabbrev-code company-capf company-keywords company-files company-dabbrev))
