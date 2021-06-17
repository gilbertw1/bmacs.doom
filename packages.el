;; -*- no-byte-compile: t; -*-
;;; private/gilbertw1/packages.el

(package! paredit :recipe (:depth full))

(package! org-mu4e :disable t)
(package! magit-todos :disable t)
(package! doom-snippets :disable t)
;(package! ivy-hydra :disable t)

(package! package-lint)
(package! play-routes-mode)
(package! avy)
(package! ace-link)
(package! evil-args)
(package! git-link)

;; Use default snippets
(package! doom-snippets :ignore t)
(package! yasnippet-snippets)

(unpin! doom-themes)
(unpin! persp-mode)
