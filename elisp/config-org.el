;; author: s. rannou <mxs@sbrk.org>
;;
;; Config for org-mode

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-agenda-files '("~/organizer.org"))
(setq org-log-done t)

;; fontify
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (dot . t)
   (js . t)))

;; when exporting in html do it with nice class names
(setq org-export-htmlize-output-type 'css)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(provide 'config-org)
