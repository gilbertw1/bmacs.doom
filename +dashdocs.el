
 (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
 (add-hook 'scala-mode-hook (lambda () (setq-local counsel-dash-docsets '("Scala" "Akka" "Play_Scala" "Java"))))
 (add-hook 'java-mode-hook (lambda () (setq-local counsel-dash-docsets '("Java" "Play_Java"))))
 (add-hook 'rust-mode-hook (lambda () (setq-local counsel-dash-docsets '("Rust"))))
 (add-hook 'clojure-mode-hook (lambda () (setq-local counsel-dash-docsets '("Clojure"))))
 (add-hook 'haskell-mode-hook (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
 (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
 (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
 (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
 (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
 (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
 (add-hook 'html-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "Javascript"))))
 (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python 3"))))

(defun gilbertw1/browse-url-new-window (url &optional ignored)
  (call-process "firefox-developer-edition" nil 0 nil "-new-window" url))

(after! dash-docs
  (setq dash-docs-browser-func #'gilbertw1/browse-url-new-window))