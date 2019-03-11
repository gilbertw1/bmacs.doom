;;; ~/.doom.d/banner.el -*- lexical-binding: t; -*-

(defun doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'font-lock-comment-face) " ")
            (insert "\n"))
          '("$$$$$$$\\  $$\\      $$\\  $$$$$$\\   $$$$$$\\   $$$$$$   "
          "$$  __$$\\ $$$\\    $$$ |$$  __$$\\ $$  __$$\\ $$  __$$\\ "
          "$$ |  $$ |$$$$\\  $$$$ |$$ /  $$ |$$ /  \\__|$$ /  \\__|"
          "$$$$$$$\\ |$$\\$$\\$$ $$ |$$$$$$$$ |$$ |      \\$$$$$$\\  "
          "$$  __$$\\ $$ \\$$$  $$ |$$  __$$ |$$ |       \\____$$\\ "
          "$$ |  $$ |$$ |\\$  /$$ |$$ |  $$ |$$ |  $$\\ $$\\   $$ |"
          "$$$$$$$  |$$ | \\_/ $$ |$$ |  $$ |\\$$$$$$  |\\$$$$$$  |"
          "\\_______/ \\__|     \\__|\\__|  \\__| \\______/  \\______/ "
          "                                                     "
          "                        EMACS                        "))
    (when (and (stringp +doom-dashboard-banner-file)
               (display-graphic-p)
               (file-exists-p! +doom-dashboard-banner-file +doom-dashboard-banner-dir))
      (let* ((image (create-image (expand-file-name +doom-dashboard-banner-file
                                                    +doom-dashboard-banner-dir)
                                  'png nil))
             (size (image-size image nil))
             (margin (+ 1 (/ (- +doom-dashboard--width (car size)) 2))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (when (> margin 0)
          (save-excursion
            (goto-char point)
            (insert (make-string (truncate margin) ? )))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) ?\n)))))
