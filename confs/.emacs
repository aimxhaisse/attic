;; s. rannou <mxs@sbrk.org>
;;
;; My dotemacs is a bootstraper for:
;;
;; * config options that are in ~/elisp/*.el and are always loaded
;; * various modes, each is a directory in ~/elisp ; some are always loaded
;;
;; I prefix all my functions with my- not to pollute the world

(defvar my-config-dir "~/elisp")

(defun my-load-paths ()
  "append to the load-path all folders in my-config-dir"
  (let ((default-directory my-config-dir))
    (and (add-to-list 'load-path my-config-dir)
     (normal-top-level-add-subdirs-to-load-path))))

(my-load-paths)

(require 'config-org)
(require 'config-misc)
(require 'config-httpd)
(require 'config-slime)
(require 'my-blog)
(require 'go-mode-load)
