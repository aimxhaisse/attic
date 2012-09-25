;; author: s. rannou <mxs@sbrk.org>
;;
;; my blog (work in progress)

(require 'xmlgen)
(require 'htmlfontify)

(defvar *input-directory* "~/blog/pages/")
(defvar *output-directory* "~/blog/html/")
(defvar *static-directory* "~/blog/static/")

(defun my-publish ()
  "publishes items from *input-directory* to *output-directory*"
  (my-collect-items *input-directory*))

(defun my-collect-items (input)
  "collects items from input"
  (my-log (concat "collecting items from " input))
  (my-get-items input))

(defun my-log (msg)
  "appends msg to the debug buffer"
  (let* ((buffer (get-buffer-create "my-blog-debug")))
    (set-buffer buffer)
    (insert (concat msg "\n"))
    t))

(defun my-make-item (type path &optional entries)
  "creates a new item"
  (my-log (concat "creating item " path))
  (list :type type
	:path path
	:entries entries))

(defun my-get-items (in)
  "returns items to be processed"
  (let* ((input (file-name-as-directory in))
	 (entries (directory-files input))
	 (items nil))
    (loop for name in entries do
	  (let* ((path (concat input name)))
	    (my-log (concat "hej " name))
	    (unless (string-prefix-p "." name)
	      (my-log (concat "ok, we add this entry: " path))
	      (if (file-directory-p path)
		  (push (my-make-item 'dir path (my-get-items path)) res)
		(push (my-make-item 'file path) res)))))))

(provide 'my-blog)
