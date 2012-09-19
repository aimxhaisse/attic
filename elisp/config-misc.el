;; s. rannou <mxs@sbrk.org>
;;
;; Random configs

(setq make-backup-files nil)		;; no *~
(menu-bar-mode -1)                      ;; no topmenu
(setq-default truncate-lines t)         ;; truncate long lines
(setq column-number-mode t)             ;; column number
(display-time-mode t)			;; time
(set-language-environment "UTF-8")
(setq debug-on-error t)

; tmux
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])

(provide 'config-misc)
