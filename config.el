;;; private/gilbertw1/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Iosevka" :size 24))
(setq doom-theme 'doom-city-lights)

(setq org-directory "~/org")

;; disable double buffering to prevent stuttering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; always indent on tab
(setq-default tab-always-indent t)

(load! "+evil.el")
(load! "+hydra.el")
(load! "+bindings.el")
(load! "+banner.el")
(load! "+email.el")
(load! "+indent.el")
(load! "+dashdocs.el")
(load! "themes/doom-flatwhite-modified-theme.el")
(load! "+counsel-jq.el")

(load! "+better-jumper.el")

;; avy use all windows
(setq avy-all-windows t)

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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(smerge-refined-added ((t (:inherit 'smerge-other))))
 '(smerge-refined-removed ((t (:inherit 'smerge-mine)))))

;; Set indentation values
(setq-default tab-width 2
              c-basic-offset 4
              coffee-tab-width 2
              javascript-2-level 2
              js-2-level 2
              js2-basic-offset 2
              web-mode-markup-2-offset 2
              web-mode-css-2-offset 2
              web-mode-code-2-offset 2
              css-2-offset 2
              rust-indent-offset 4)

;; command to open markdown files with
(setq markdown-open-command "typora")

;; remove xref lookup backend from specific major modes
(add-hook! (scala-mode)
  (setq-local lsp-enable-indentation nil)
  (setq-local +lookup-definition-functions
              '(+lookup-dumb-jump-backend
                +lookup-project-search-backend
                +lookup-evil-goto-definition-backend)))

; Set default sbt command to compile
(setq sbt:default-command "compile")

; Never use the home directory as an sbt project
(defadvice! sbt-find-root-ignore-home (orig-fn name-or-pred &optional dir best-root)
  "Ignore home directory when selecting sbt root."
  :around #'sbt:find-root-impl
  (if (and dir (string= (expand-file-name dir) (expand-file-name "~/")))
      (sbt:find-root-impl name-or-pred (file-name-directory (directory-file-name dir)) best-root)
    (funcall orig-fn name-or-pred dir best-root)))

;; set specific company backends for scala mode
;(set-company-backend! 'scala-mode 'company-dabbrev-code 'company-capf 'company-keywords 'company-files)

;; turn on company auto completion
(setq company-idle-delay 0.3)
(setq ensime-company-idle-delay 0.3)
(setq company-dabbrev-downcase 0)

;; Evil snipe function across lines
(setq evil-snipe-scope 'visible)

(add-hook! 'prog-mode
  (setq doom-inhibit-indent-detection t))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward))

(set-company-backend! 'mu4e-compose-mode 'company-capf)

(setq +workspaces-on-switch-project-behavior t)

(defmacro +ivy-do-action! (action)
  "Returns an interactive lambda that sets the current ivy action and
immediately runs it on the current candidate (ending the ivy session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))

;; dumb-jump functions
(defun bmacs/jump-definition (&optional other-window)
  "Jump to the definition of the symbol at point using `dumb-jump'"
  (interactive)
  (evil--jumps-push)
  (if other-window
      (dumb-jump-go-other-window)
    (dumb-jump-go)))

(defun bmacs/jump-definition-other-window ()
  (interactive)
  (bmacs/jump-definition t))

;(setq-default ivy-read-action-function #'ivy-read-action-ivy)
(setq-default ivy-read-action-function #'ivy-hydra-read-action)
(put 'customize-themes 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282828" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   '("54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "eb94e44599a45c495ad472ad551a40b87cbc4bae9631211e7a78d72b102c61b1" "a4b9eeeabde73db909e6b080baf29d629507b44276e17c0c411ed5431faf87dd" "5c9a906b076fe3e829d030a404066d7949e2c6c89fc4a9b7f48c054333519ee7" "4b0b568d63b1c6f6dddb080b476cfba43a8bbc34187c3583165e8fb5bbfde3dc" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "8c75e2bdf8d1293c77a752dd210612cfb99334f7edd360a42a58a8497a078b35" "41039913efab185af1ec1b13ff4df36d6941994d5e3dee39791f30fcd94b42be" "de43de35da390617a5b3e39b6b27c107cc51271eb95cceb1f43d13d9647c911d" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "15ba8081651869ec689c9004288bed79003de5b4ee9c51a9d4a208d9e3439706" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "f1938227a38cfe55a12076dac514f03a1d9aa1a47957870b690cc80db5e7a1b3" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" default))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))

;; Override org mode colors
; (custom-set-faces
;  '(org-level-1              ((t :foreground "#51afef" :inherit nil :height 1.2)))
;  '(org-level-2              ((t :foreground "#DCAEEA" :inherit nil :height 1.1)))
;  '(org-level-3              ((t :foreground "#a9a1e1" :inherit nil :height 1.1)))
;  '(org-level-4              ((t :foreground "#ECBE7B" :inherit nil :height 1.1)))
;  '(org-level-5              ((t :foreground "#46D9FF" :inherit nil :height 1.1))))

;; Override flycheck warning face (specifically for tango theme)
; (custom-set-faces
;  '(flycheck-warning       ((t :underline (:color "#bd8400" :style wave) :inherit nil))))


(define-key! ivy-minibuffer-map
    "M-o" #'ivy-dispatching-done)

(add-hook! mu4e-header-mode
   (setq-local hl-line-sticky-flag t))

(add-hook! mu4e-loading-mode
   (setq-local hl-line-sticky-flag t))

(add-hook! mu4e-view-mode
   (setq-local hl-line-sticky-flag t))

;(setq-hook! '(mu4e-header-mode mu4e-loading-mode mu4e-view-mode) hl-line-sticky-flag t)

;Evil iedit hack
(with-eval-after-load 'evil-iedit-state
    (fset 'iedit-cleanup 'iedit-lib-cleanup))


; (with-eval-after-load 'persp-mode
;   (defadvice persp-delete-other-windows (before better-jumper activate)
;     (select-window (split-window))))

; (setq persp-reset-windows-on-nil-window-conf
;       (lambda ()
;         (message "RESETTING WINDOWS")
;         (delete-other-windows (split-window))))
