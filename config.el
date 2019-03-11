;;; private/gilbertw1/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Iosevka" :size 24))

(general-override-mode +1)

(load! "+evil.el")
(load! "+hydra.el")
(load! "+bindings.el")
(load! "+banner.el")

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (progn
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil)))

;; open *.rest files in restclient mode
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Override smerge colors
(custom-set-faces
  '(smerge-refined-removed ((t (:inherit 'smerge-mine))))
  '(smerge-refined-added   ((t (:inherit 'smerge-other)))))

(setq markdown-open-command "typora")
