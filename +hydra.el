;;; ~/.doom.d/+hydra.el -*- lexical-binding: t; -*-

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra bmacs-hydra-window ()
  "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_/_ vertical   	_b_uffer			_H_ X←
_j_ ↓        	_-_ horizontal	_f_ind files	_J_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1				_K_ X↑
_l_ →        	_Z_ reset      	_s_wap				_L_ X→
_F_ollow		  _D_lt Other   	_S_ave				_m_aximize
_SPC_ cancel	_o_nly this   	_d_elete			_=_ Balance
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("b" counsel-projectile-switch-to-buffer)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("/" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("-" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("o" delete-other-windows)
  ("m" ace-delete-other-windows)
  ("=" balance-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))

(defhydra bmacs-hydra-smerge (:hint nil
                                    :pre (smerge-mode 1)
                                    ;; Disable `smerge-mode' when quitting hydra if
                                    ;; no merge conflicts remain.
                                    :post (smerge-auto-leave))
  "
                                                      ╭────────┐
    Movement   Keep           Diff              Other │ smerge │
    ╭─────────────────────────────────────────────────┴────────╯
       ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
       ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
       ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
       ^_j_ ↓^     [_a_] all        [_H_] hightlight
       ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
       ^_G_^                                            │ [_q_] quit"
  ("g" (progn (goto-char (point-min)) (smerge-next)))
  ("G" (progn (goto-char (point-max)) (smerge-prev)))
  ("C-j" smerge-next)
  ("C-k" smerge-prev)
  ("j" next-line)
  ("k" previous-line)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("H" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("R" smerge-kill-current)
  ("q" nil :color blue))
