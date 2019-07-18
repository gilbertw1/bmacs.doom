;;; private/gilbertw1/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Iosevka" :size 24))

;; disable double buffering to prevent stuttering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(load! "+evil.el")
(load! "+hydra.el")
(load! "+bindings.el")
(load! "+banner.el")
(load! "+email.el")
(load! "+indent.el")

;; avy use all windows
(setq avy-all-windows t)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (progn
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil)))

(use-package package-lint
  :commands (package-lint-current-buffer package-lint-buffer))

(use-package play-routes-mode
  :mode "/routes$")

;; open *.rest files in restclient mode
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; open routes.template file type in play routes mode
(add-to-list 'auto-mode-alist '("/routes.template'" . play-routes-mode))

;; Add slight spacing between edge and fringe
(add-to-list 'default-frame-alist '(internal-border-width . 2))

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
(set-company-backend! 'scala-mode 'company-dabbrev-code 'company-capf 'company-keywords 'company-files)

;; turn on company auto completion
(setq company-idle-delay 0.3)
(setq ensime-company-idle-delay 0.3)
(setq company-dabbrev-downcase 0)

(add-hook! 'prog-mode
  (setq doom-inhibit-indent-detection t))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward))

(set-company-backend! 'mu4e-compose-mode 'company-capf)

(setq +workspaces-on-switch-project-behavior t)
