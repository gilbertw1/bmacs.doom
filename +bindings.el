;;; private/gilbertw1/+bindings.el -*- lexical-binding: t; -*-

(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key "SPC m"
      doom-localleader-alt-key "M-SPC m")

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

;; Global Keybindings
(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag
 ;; Essential
 "M-x"    #'execute-extended-command
 "M-;"    #'eval-expression
 "M-u"    #'universal-argument

 ;;; Evil-esque bindings
 ;;; Smarter newlines
 :i [remap newline] #'newline-and-indent  ; auto-indent on newline
 :i "C-j"           #'+bmacs/newline    ; default behavior
 ;; Smarter RET in normal mode
 :n "RET" (general-predicate-dispatch nil
            (and (bound-and-true-p flyspell-mode)
                 (+flyspell-correction-at-point-p))
            'flyspell-correct-word-generic)
 ;; Yank to EOL
 :n  "Y"  "y$"
 ;; without yank commands
 :n  "c"  #'evil-change-without-register
 :n  "C"  #'evil-change-line-without-register
 :n  "p"  #'evil-paste-after-without-register
 :n  "P"  #'evil-paste-before-without-register
 :n  "x"  #'evil-delete-char-without-register
 :n  "X"  #'evil-delete-backward-char-without-register
 :n  "d"  #'evil-delete-without-register-if-whitespace
 ;; evil little word motions
 :nov "w" #'evil-forward-little-word-begin
 :nov "b" #'evil-backward-little-word-begin
 :nov "e" #'evil-forward-little-word-end
 ;; simple motions
 :nv "H"  #'evil-first-non-blank
 :nv "L"  #'evil-last-non-blank
 ;; evil-commentary
 :n  "gc" #'evil-commentary
 ;; avy goto char
 :n "s"   #'avy-goto-char-2
 :n "S"   #'bmacs/evil-avy-goto-char-2-after-insert
 ;; evil-surround
 :v  "s"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit
 ;; expand-region
 :v  "v"  #'er/expand-region
 :v  "V"  #'er/contract-region
 ;; don't leave visual mode after shifting
 :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv
 ;; save buffer (but don't close window)
 :n "zz"  #'save-buffer
 :v "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
 :v "K" (concat ":m '<-2" (kbd "RET") "gv=gv")
 :n "K" #'evil-previous-line

 ;; Indent on tab
 :nvi "TAB"     #'indent-for-tab-command
 ;; Temporary escape into emacs mode
 :e [C-escape] #'evil-normal-state
 :n [C-escape] #'evil-emacs-state

 (:after vc-annotate
   :map vc-annotate-mode-map
   [remap quit-window] #'kill-this-buffer)

 :n "C-S-f"  #'toggle-frame-fullscreen

 ;; Works when I accidentally use tmux bindings
 (:desc "tmux emulation"
   :prefix "C-b"
   :desc "Window left"                     :nv "h" #'evil-window-left
   :desc "Window down"                     :nv "j" #'evil-window-down
   :desc "Window right"                    :nv "l" #'evil-window-right
   :desc "Window up"                       :nv "k" #'evil-window-up
   :desc "Delete current window"           :nv "x" #'delete-window
   :desc "Split window vertical"           :nv "/" #'split-window-right
   :desc "Split window horizontal"         :nv "-" #'split-window-below)
 ;; window management (prefix "C-w")
 (:map evil-window-map
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'other-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   "O"       #'doom/window-zoom
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)

 (:after evil-easymotion
   (:prefix ("g" . "easymotion")
     "l"    #'evilem-motion-forward-word-begin
     "h"    #'evilem-motion-backward-word-begin
     "L"    #'evilem-motion-forward-WORD-begin
     "H"    #'evilem-motion-backward-WORD-begin
     "("    #'evilem-motion-forward-sentance-begin
     ")"    #'evilem-motion-backward-sentance-begin
     "n"    #'evilem-motion-search-next
     "N"    #'evilem-motion-search-previous
     "SPC"  #'avy-goto-char-timer
     "g"    #'evil-goto-first-line))

 (:map help-map
   "'"   #'doom/what-face
   "."   #'helpful-at-point ; replaces `display-local-help'
   "a"   #'apropos ; replaces `apropos-command'
   "A"   #'doom/describe-autodefs
   "B"   #'doom/open-bug-report
   "d"   #'doom/describe-module ; replaces `apropos-documentation' b/c `apropos' covers this
   "D"   #'doom/open-manual
   "E"   #'doom/open-vanilla-sandbox
   "F"   #'describe-face ; replaces `Info-got-emacs-command-node' b/c redundant w/ helpful
   "h"   #'+lookup/documentation ; replaces `view-hello-file' b/c annoying
   "L"   #'global-command-log-mode ; replaces `describe-language-environment' b/c remapped to C-l
   "C-l" #'describe-language-environment
   "M"   #'doom/describe-active-minor-mode
   "C-m" #'info-emacs-manual
   "n"   #'doom/open-news ; replaces `view-emacs-news' b/c it's on C-n too
   "O"   #'+lookup/online
   "p"   #'doom/describe-package ; replaces `finder-by-keyword'
   "P"   #'find-library ; replaces `describe-package' b/c redundant w/ `doom/describe-package'
   "t"   #'doom/toggle-profiler ; replaces `help-with-tutorial' b/c not useful for evil users
   "r" nil ; replaces `info-emacs-manual' b/c it's on C-m now
   (:prefix ("r" . "reload")
     "r"   #'doom/reload
     "t"   #'doom/reload-theme
     "p"   #'doom/reload-packages
     "f"   #'doom/reload-font
     "P"   #'doom/reload-project)
   "V"   #'doom/version ; replaces `finder-by-keyword'
   "W"   #'+bmacs/man-or-woman))


;; Leader
(map! :leader
      :desc "Eval expression"       ";"    #'eval-expression
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture
      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Blink cursor line"     "DEL"  #'+nav-flash/blink-cursor
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump
      :desc "Open last buffer"      "TAB"  #'bmacs/alternate-buffer

      :desc "Universal argument"      "u"  #'universal-argument
      :desc "Switch workspace buffer" ","  #'persp-switch-to-buffer
      :desc "Switch buffer"           "<"  #'switch-to-buffer

      :desc "Search from here"      "?"    #'counsel-rg
      :desc "Find word in project"  "*"    #'counsel-projectile-rg-region-or-symbol
      :desc "Toggle last popup"     "~"    #'+popup/toggle
      :desc "Find file"             "."    #'find-file
      :desc "Ace window"            "W"    #'ace-window

      :desc "Resume last search"    "'"    #'ivy-resume
      :desc "help"                  "h"    help-map

      (:prefix ("/" . "search")
        :desc "Jump to symbol across buffers" "I" #'imenu-anywhere
        :desc "Search buffer"                 "b" #'swiper
        :desc "Search current directory"      "d" #'+bmacs/search-from-cwd
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Search in project"             "/" #'+ivy/project-search
        :desc "Search project"                "p" #'+ivy/project-search)

      (:prefix ("b" . "buffer")
        :desc "Select entire buffer"           "a"   #'mark-whole-buffer
        :desc "Switch workspace buffer"        "b"   #'persp-switch-to-buffer
        :desc "Switch buffer"                  "B"   #'ivy-switch-buffer
        :desc "Kill buffer"                    "d"   #'kill-this-buffer
        :desc "Kill other buffers"             "D"   #'doom/kill-other-buffers
        :desc "Kill a buffer"                  "k"   #'kill-buffer
        :desc "New empty buffer"               "n"   #'evil-buffer-new
        :desc "Scratch buffer"                 "s"   #'doom/open-scratch-buffer
        :desc "Bury buffer"                    "z"   #'bury-buffer)

      (:prefix ("c" . "code")
        :desc "Build"                       "b"   #'+eval/build
        :desc "Jump to definition"          "d"   #'+lookup/definition
        :desc "Jump to references"          "D"   #'+lookup/references
        :desc "Evaluate buffer/region"      "e"   #'+eval/buffer-or-region
        :desc "Evaluate & replace region"   "E"   #'+eval:replace-region
        :desc "Format buffer/region"        "f"   #'+format/region-or-buffer
        :desc "Dash install docset"         "i"   #'counsel-dash-install-docset
        :desc "Open REPL"                   "r"   #'+eval/open-repl-other-window
                                        ;:desc "Counsel dash"                "d"   #'counsel-dash
        :desc "Delete trailing newlines"    "W"   #'doom/delete-trailing-newlines
        :desc "List errors"                 "x"   #'flycheck-list-errors)

      (:prefix ("f" . "file")
        :desc "Find directory"                 "a" #'dired
        :desc "Counsel bookmark"               "b" #'counsel-bookmark
        :desc "Copy file"                      "c" #'copy-file
        :desc "Copy current file"              "C" #'bmacs/copy-file
        :desc "Delete file"                    "d" #'bmacs/delete-file-confirm
        :desc "Delete current file"            "D" #'doom/delete-this-file
        :desc "Open project editorconfig"      "e" #'editorconfig-find-current-editorconfig
        :desc "Find file in emacs.d"           "E" #'+bmacs/find-in-emacsd
        :desc "Open file from here"            "f" #'find-file
        :desc "Sudo open file from here"       "F" #'doom/sudo-find-file
        :desc "Remote"                         "i" (λ! (counsel-find-file "/"))
        :desc "Find file in private config"    "p" #'doom/find-file-in-private-config
        :desc "Browse private config"          "P" #'doom/open-private-config
        :desc "Recent files"                   "r" #'counsel-recentf
        :desc "Rename current file"            "R" #'bmacs/rename-current-buffer-file
        :desc "Save buffer"                    "s" #'save-buffer
        :desc "Save all buffers"               "S" #'evil-write-all
        :desc "Neotree toggle"                 "t" #'treemacs/toggle
        :desc "Sudo find file"                 "u" #'doom/sudo-find-file
        :desc "Sudo edit this file"            "U" #'doom/sudo-this-file
        :desc "Cleanup this tramp conn"        "x" #'tramp-cleanup-this-connection
        :desc "Cleanup all tramp conns"        "X" (λ! (tramp-cleanup-all-buffers) (tramp-cleanup-all-connections))
        :desc "Yank filename"                  "y" #'+bmacs/yank-buffer-filename)

      (:prefix ("g" . "git")
        :desc "Jump to next hunk"            "]"   #'git-gutter:next-hunk
        :desc "Jump to previous hunk"        "["   #'git-gutter:previous-hunk
        :desc "Git status"                   "s"   #'magit-status
        :desc "Git blame"                    "b"   #'magit-blame
        :desc "Git log current file"         "C"   #'magit-file-popup
        :desc "Magit fetch"                  "F"   #'magit-fetch
        :desc "Git time machine"             "t"   #'git-timemachine
                                        ;:desc "Git log all"                  "l"   #'magit-log-all
        :desc "Magit buffer log"             "L"   #'magit-log
        :desc "Smerge Hydra"                 "m"   #'bmacs-hydra-smerge/body
        :desc "Show and copy git link"       "y"   #'git-link
                                        ;:desc "Open git link"                "o"   #'bmacs/git-browse
        :desc "Delete git index lock"        "X"   #'bmacs/delete-git-index-lock
        (:prefix ("g" . "gist")
          :desc "Gist from buffer"           :nv "b" #'gist-buffer
          :desc "Private gist from buffer"   :nv "B" #'gist-buffer-private
          :desc "Gist from region"           :nv "r" #'gist-region
          :desc "Private gist from region"   :nv "R" #'gist-region-private
          :desc "List gists"                 :nv "l" #'gist-list)
        (:prefix ("c" . "create")
          :desc "Initialize repo"           "r"   #'magit-init
          :desc "Clone repo"                "R"   #'+magit/clone
          :desc "Commit"                    "c"   #'magit-commit-create
          :desc "Issue"                     "i"   #'forge-create-issue
          :desc "Pull request"              "p"   #'forge-create-pullreq)
        (:prefix ("f" . "find")
          :desc "Find file"                 "f"   #'magit-find-file
          :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
          :desc "Find commit"               "c"   #'magit-show-commit
          :desc "Find issue"                "i"   #'forge-visit-issue
          :desc "Find pull request"         "p"   #'forge-visit-pullreq)
        (:prefix ("l" . "list")
          :desc "List gists"                "g"   #'+gist:list
          :desc "List repositories"         "r"   #'magit-list-repositories
          :desc "List submodules"           "s"   #'magit-list-submodules
          :desc "List issues"               "i"   #'forge-list-issues
          :desc "List pull requests"        "p"   #'forge-list-pullreqs
          :desc "List notifications"        "n"   #'forge-list-notifications)
        (:prefix ("o" . "open in browser")
          :desc "Browse region or line"     "."   #'+vc/git-browse-region-or-line
          :desc "Browse remote"             "r"   #'forge-browse-remote
          :desc "Browse commit"             "c"   #'forge-browse-commit
          :desc "Browse an issue"           "i"   #'forge-browse-issue
          :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
          :desc "Browse issues"             "I"   #'forge-browse-issues
          :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs))

      (:prefix ("i" . "insert")
        :desc "Insert from clipboard"         "y"   #'+bmacs/yank-pop
        :desc "Insert from evil register"     "r"   #'evil-ex-registers
        :desc "New snippet"                   "n" #'yas-new-snippet
        :desc "Insert snippet"                "i" #'yas-insert-snippet
        :desc "Jump to mode snippet"          "/" #'yas-visit-snippet-file
        :desc "Jump to snippet"               "s" #'+snippets/find-file
        :desc "Browse snippets"               "S" #'+snippets/browse
        :desc "Reload snippets"               "r" #'yas-reload-all)

      (:prefix ("l" . "workspace")
        :desc "New workspace"                 "n"   #'+workspace/new-with-name
        :desc "Switch workspace"              "l"   #'+workspace/switch-to
        :desc "Delete session"                "x"   #'+workspace/kill-session
        :desc "Delete this workspace"         "d"   #'+workspace/delete
        :desc "Rename workspace"              "r"   #'+workspace/rename
        :desc "Switch to last workspace"      "TAB" #'bmacs/workspace-switch-last)

      (:prefix ("n" . "notes")
        :desc "Open deft"           "d"  #'deft
        :desc "Find file in notes"  "n"  #'+bmacs/find-in-notes
        :desc "Browse notes"        "N"  #'+bmacs/browse-notes
        :desc "Org capture"         "x"  #'org-capture)

      (:prefix ("o" . "open")
        :desc "Org agenda"         "a"  #'org-agenda
        :desc "Default browser"    "b"  #'browse-url-of-file
        :desc "Debugger"           "d"  #'+debug/open
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        :desc "Project sidebar"              "p" #'+treemacs/toggle
        :desc "Find file in project sidebar" "P" #'+treemacs/find-file
        :desc "Imenu sidebar" "i" #'imenu-list-smart-toggle
        :desc "Terminal"          "t" #'+term/open
        :desc "Terminal in popup" "T" #'+term/open-popup-in-project
        :desc "Terminal"          "t" #'+vterm/open
        :desc "Terminal in popup" "T" #'+vterm/open-popup-in-project
        :desc "Eshell"            "e" #'+eshell/open
        :desc "Eshell in popup"   "E" #'+eshell/open-popup)

      (:prefix ("p" . "project")
        :desc "Browse project"               "." #'+bmacs/browse-project
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Find file in project"         "/" #'projectile-find-file
        :desc "Project specific bookmarks"    :nv "b" #'counsel-projectile-bookmark
        :desc "Compile project"              "c" #'projectile-compile-project
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Recent project files"         "r" #'projectile-recentf
        :desc "List project tasks"           "t" #'+bmacs/project-tasks
        :desc "Kill project buffers"          "k" #'projectile-kill-buffers
        :desc "Invalidate cache"             "x" #'projectile-invalidate-cache)

      (:prefix ("q" . "session")
        :desc "Quit Emacs"                   "q" #'evil-quit-all
        :desc "Save and quit Emacs"          "Q" #'evil-save-and-quit
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      (:prefix ("s" . "search")
        :desc "IEdit mode"                    :nv "e" #'evil-iedit-state/iedit-mode
        :desc "Swiper search"                 :nv "s" #'swiper)

      (:prefix ("t" . "toggle")
        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "Flycheck"                     "f" #'flycheck-mode
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Indent guides"                "i" #'highlight-indentation-mode
        :desc "Indent guides (column)"       "I" #'highlight-indentation-current-column-mode
        :desc "Impatient mode"               "h" #'+impatient-mode/toggle
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "org-tree-slide mode"          "p" #'+org-present/start)

      (:prefix ("w" . "window")
        :desc "Split window vertical"        :nv "/" #'split-window-right
        :desc "Split window horizontal"      :nv "-" #'split-window-below
        :desc "Balance windows"              :nv "=" #'balance-windows
        :desc "Delete current window"        :nv "d" #'+workspace/close-window-or-workspace
        :desc "Ace delete window"            :nv "D" #'ace-delete-window
        :desc "Window left"                  :nv "h" #'evil-window-left
        :desc "Window down"                  :nv "j" #'evil-window-down
        :desc "Window right"                 :nv "l" #'evil-window-right
        :desc "Window up"                    :nv "k" #'evil-window-up
        :desc "Move window left"             :nv "H" #'evil-window-move-far-left
        :desc "Move window down"             :nv "J" #'evil-window-move-very-bottom
        :desc "Move window right"            :nv "L" #'evil-window-move-far-right
        :desc "Move window up"               :nv "K" #'evil-window-move-very-top
        :desc "Ace swap window"              :nv "s" #'ace-swap-window
        :desc "Winner undo"                  :nv "u" #'winner-undo
        :desc "Winner redo"                  :nv "U" #'winner-redo
        :desc "Ace window"                   :nv "W" #'ace-window
        :desc "Manage Windows"               :nv "w" #'bmacs-hydra-window/body
        :desc "Toggle maximize window"       :nv "m" #'doom/window-zoom))

;; Modules
(map! (:after evil-mc
        (:map evil-mc-key-map
          "C-S-j"          #'evil-mc-make-cursor-move-next-line
          "C-S-k"          #'evil-mc-make-cursor-move-prev-line))

      (:after realgud
        (:map realgud:shortkey-mode-map
          :n "j" #'evil-next-line
          :n "k" #'evil-previous-line
          :n "h" #'evil-backward-char
          :n "l" #'evil-forward-char
          :n "c" #'realgud:cmd-continue
          :m "n" #'realgud:cmd-next
          :m "b" #'realgud:cmd-break
          :m "B" #'realgud:cmd-clear))

      ;; Lookup
      :nv "K"  #'+lookup/documentation
      :nv "gd" #'+lookup/definition
      :nv "gD" #'+lookup/references
      :nv "gf" #'+lookup/file

      ;; Snippets
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create
      (:map yas-keymap
        "C-e"         #'+snippets/goto-end-of-field
        "C-a"         #'+snippets/goto-start-of-field
        [M-right]     #'+snippets/goto-end-of-field
        [M-left]      #'+snippets/goto-start-of-field
        [M-backspace] #'+snippets/delete-to-start-of-field
        [backspace]   #'+snippets/delete-backward-char
        [delete]      #'+snippets/delete-forward-char-or-field)

      ;; flycheck
      :m "]e" #'next-error
      :m "[e" #'previous-error
      (:map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; workspaces
      :n "gt"    #'+workspace/switch-right
      :n "gT"    #'+workspace/switch-left
      :g "M-t"   #'+workspace/new
      :g "M-T"   #'+workspace/display)

;; Completion
(map! :i "C-@"      #'+company/complete
      :i "C-SPC"    #'+company/complete
      (:prefix "C-x"
        :i "C-l"    #'+company/whole-lines
        :i "C-k"    #'+company/dict-or-keywords
        :i "C-f"    #'company-files
        :i "C-]"    #'company-etags
        :i "s"      #'company-ispell
        :i "C-s"    #'company-yasnippet
        :i "C-o"    #'company-capf
        :i "C-n"    #'+company/dabbrev
        :i "C-p"    #'+company/dabbrev-code-previous)
      (:after company
        (:map company-active-map
          "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
          "C-n"     #'company-select-next
          "C-p"     #'company-select-previous
          "C-j"     #'company-select-next
          "C-k"     #'company-select-previous
          "C-h"     #'company-show-doc-buffer
          "C-u"     #'company-previous-page
          "C-d"     #'company-next-page
          "C-s"     #'company-filter-candidates
          "C-S-s"   #'counsel-company
          "C-SPC"   #'company-complete-common
          "TAB"     #'company-complete-common-or-cycle
          [backtab] #'company-select-previous)
        (:map company-search-map  ; applies to `company-filter-map' too
          "C-n"     #'company-select-next-or-abort
          "C-p"     #'company-select-previous-or-abort
          "C-j"     #'company-select-next-or-abort
          "C-k"     #'company-select-previous-or-abort
          "C-s"     (λ! (company-search-abort) (company-filter-candidates))
          "ESC"     #'company-search-abort)
        ;; TAB auto-completion in term buffers
        :map comint-mode-map "TAB" #'company-complete)

      (:map (help-mode-map helpful-mode-map)
        :n "Q" #'ivy-resume)
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC"  #'ivy-call-and-recenter  ; preview file
        "C-y"    #'yank
        "C-h"    (kbd "DEL")
        "C-k"    #'ivy-previous-line
        "C-j"    #'ivy-next-line
        "C-w"    #'ivy-backward-kill-word
        "C-u"    #'ivy-kill-line
        "C-b"    #'backward-word
        "C-f"    #'forward-word
        "C-o"    #'ivy-dispatching-done
        "C-l"    #'ivy-alt-done
        "C-v"    #'yank)
      (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
        [C-return] (+ivy-do-action! #'+ivy-git-grep-other-window-action))
      (:after swiper
        :map swiper-map
        [backtab] #'+ivy/wgrep-occur))

;; UI
(map! :m "]t" #'hl-todo-next
      :m "[t" #'hl-todo-previous

      (:after neotree
        :map neotree-mode-map
        :n "g"     nil
        :n "TAB"   #'neotree-quick-look
        :n "RET"   #'neotree-enter
        :n "DEL"   #'evil-window-prev
        :n "c"     #'neotree-create-node
        :n "r"     #'neotree-rename-node
        :n "d"     #'neotree-delete-node
        :n "j"     #'neotree-next-line
        :n "k"     #'neotree-previous-line
        :n "n"     #'neotree-next-line
        :n "p"     #'neotree-previous-line
        :n "h"     #'+neotree/collapse-or-up
        :n "l"     #'+neotree/expand-or-open
        :n "J"     #'neotree-select-next-sibling-node
        :n "K"     #'neotree-select-previous-sibling-node
        :n "H"     #'neotree-select-up-node
        :n "L"     #'neotree-select-down-node
        :n "G"     #'evil-goto-line
        :n "gg"    #'evil-goto-first-line
        :n "v"     #'neotree-enter-vertical-split
        :n "s"     #'neotree-enter-horizontal-split
        :n "q"     #'neotree-hide
        :n "R"     #'neotree-refresh)

      :n "C-`"   #'+popup/toggle
      :n "C-~"   #'+popup/raise
      :g "C-x p" #'+popup/other

      :m "]d"    #'git-gutter:next-hunk
      :m "[d"    #'git-gutter:previous-hunk)

;; Editor
(map!
 :nv "C-SPC" #'+fold/toggle
 :n "gQ"    #'+format:region

 ;; evil-mc
 (:prefix "gz"
   :nv "d" #'evil-mc-make-and-goto-next-match
   :nv "D" #'evil-mc-make-and-goto-prev-match
   :nv "j" #'evil-mc-make-cursor-move-next-line
   :nv "k" #'evil-mc-make-cursor-move-prev-line
   :nv "m" #'evil-mc-make-all-cursors
   :nv "n" #'evil-mc-make-and-goto-next-cursor
   :nv "N" #'evil-mc-make-and-goto-last-cursor
   :nv "p" #'evil-mc-make-and-goto-prev-cursor
   :nv "P" #'evil-mc-make-and-goto-first-cursor
   :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
   :nv "u" #'evil-mc-undo-all-cursors
   :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here)
 (:after evil-mc
   :map evil-mc-key-map
   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
 ;; evil-multiedit
 :v  "R"     #'evil-multiedit-match-all
 :n  "M-d"   #'evil-multiedit-match-symbol-and-next
 :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   #'evil-multiedit-match-and-next
 :v  "M-D"   #'evil-multiedit-match-and-prev
 :nv "C-M-d" #'evil-multiedit-restore
 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 :n "!" #'rotate-text)

;;; :emacs
(map! (:after git-timemachine
        :map git-timemachine-mode-map
        :n "C-p" #'git-timemachine-show-previous-revision
        :n "C-n" #'git-timemachine-show-next-revision
        :n "[["  #'git-timemachine-show-previous-revision
        :n "]]"  #'git-timemachine-show-next-revision
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame))

;; tools
(map! (:after evil-magit
        ;; fix conflicts with private bindings
        :map (magit-status-mode-map magit-revision-mode-map)
        "C-j" nil
        "C-k" nil)
      (:map transient-map
        "q" #'transient-quit-one)

      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url))

;;; :lang
(map! (:after markdown-mode
        :map markdown-mode-map
        ;; fix conflicts with private bindings
        [backspace] nil)

      (:after sbt
        (:map sbt-mode-map
          :nv "C-d" #'evil-scroll-down)) )


;; Universal motion repeating keys

(defvar +bmacs-repeat-keys (cons ";" ",")
  "The keys to use for repeating motions.

This is a cons cell whose CAR is the key for repeating a motion forward, and
whose CDR is for repeating backward. They should both be kbd-able strings.")

(when +bmacs-repeat-keys
  (defmacro do-repeat! (command next-func prev-func)
    "Makes ; and , the universal repeat-keys in evil-mode. These keys can be
customized by changing `+bmacs-repeat-forward-key' and
`+bmacs-repeat-backward-key'."
    (let ((fn-sym (intern (format "+bmacs*repeat-%s" (doom-unquote command)))))
      `(progn
         (defun ,fn-sym (&rest _)
           (define-key! :states 'motion
             (car +bmacs-repeat-keys) #',next-func
             (cdr +bmacs-repeat-keys) #',prev-func))
         (advice-add #',command :before #',fn-sym))))

  ;; n/N
  (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (setq evil-snipe-repeat-keys nil
        evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
  (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse)

  ;; */#
  (do-repeat! evil-visualstar/begin-search-forward
              evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
              evil-ex-search-previous evil-ex-search-next))

;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
;; NOTE SPC u replaces C-u as the universal argument.
(map! :gi "C-u" #'doom/backward-kill-to-bol-and-indent
      :gi "C-w" #'backward-kill-word
      ;; Vimmish ex motion keys
      :gi "C-b" #'backward-word
      :gi "C-f" #'forward-word)

(after! view
  (define-key view-mode-map [escape] #'View-quit-all))
(after! man
  (evil-define-key* 'normal Man-mode-map "q" #'kill-this-buffer))

;; Minibuffer
(define-key! evil-ex-completion-map
  "C-a" #'move-beginning-of-line
  "C-b" #'backward-word
  "C-s" (if (featurep! :completion ivy)
            #'counsel-minibuffer-history
          #'helm-minibuffer-history))

; (define-key! :keymaps +bmacs-minibuffer-maps
;   [escape] #'abort-recursive-edit
;   "C-v"    #'yank
;   "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
;   "C-a"    #'move-beginning-of-line
;   "C-b"    #'backward-word
;   "C-r"    #'evil-paste-from-register
;   ;; Scrolling lines
;   "C-j"    #'next-line
;   "C-k"    #'previous-line
;   "C-S-j"  #'scroll-up-command
;   "C-S-k"  #'scroll-down-command)
