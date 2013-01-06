;; author: s. rannou <mxs@sbrk.org>
;;
;; my blog (work in progress)

(require 'org-publish)
(require 'xmlgen)
(require 'htmlfontify)
(require 'org-atom)
(require 'atom-syndication)

(defun my-blog-header ()
  "returns an htmlized string for the header of the blog"
  (xmlgen '(div :id "header"
		(ul :id "menu"
		    (li :class "menu-item"
			(a :href "/" "home"))
		    (li :class "menu-item"
			(a :href "/quotes.html" "quotes"))
		    (li :class "menu-item"
			(a :href "/bookmarks.html" "bookmarks"))
		    (li :class "menu-item"
			(a :href "/sitemap.html" "sitemap")))
                (a :href "/"
		   (img :id "dave" :src "/img/header.png")))))

(defun my-blog-footer ()
  "returns an htmlized string for the footer of the blog"
  (xmlgen '(div :id "footer"
		(hr)
		(a :href "http://sbrk.org" "powered by sbrk.org"))))

(defun my-blog-styles (styles)
  "returns an htmlized string of css files to include"
  (if styles
      (concat (xmlgen
	       `(link :href ,(concat "/css/" (car styles))
		      :rel "stylesheet"
		      :style "text/css"))
	      (my-blog-styles (cdr styles))) nil))

(defun my-blog-now-rfc-822 ()
  "generates a string representing now following the RFC 822"
  (format-time-string "%a, %d %b %Y %H:%I:%S %Z" (current-time) t))

(defun my-blog-make-rss (items)
  "generates the rss feed using 'items'"
  (let* ((rss-description (or (plist-get project-plist :rss-description) "RSS Feed"))
	 (rss-link (plist-get project-plist :publishing-url))
	 (rss-title (plist-get project-plist :rss-title))
	 (feed items))
    (setq feed (cons `(title ,rss-title) feed))
    (setq feed (cons `(link ,rss-link) feed))
    (setq feed (cons `(ttl ,(* 3600 24)) feed))
    (setq feed (cons `(pubDate ,(my-blog-now-rfc-822)) feed))
    (setq feed (cons `(description rss-description) feed))
    (setq feed (cons 'channel feed))
    (xmlgen `(rss :version "2.0" ,feed))))

(defun my-blog-sitemap ()
  "generates an RSS feed for my blog using entries from :rss-directory"
  (let* ((rss-dir (plist-get project-plist :rss-directory))
	 (dir (plist-get project-plist :base-directory))
	 (rss-filename (or (plist-get project-plist :rss-filename) "sitemap.xml"))
	 (target (format "%s/%s" (plist-get project-plist :publishing-directory) rss-filename))
	 (files (directory-files rss-dir))
	 (items ()))
    (with-current-buffer (find-file target)
      (erase-buffer)
      (while files
	(let* ((file (car files))
	       (link (file-relative-name file dir))
	       (title (org-publish-find-title file)))
	  (setq items (cons `(item (title ,title) (link ,link)) items)))
	(setq files (cdr files)))
      (insert (my-blog-make-rss items))
      (save-buffer)
      (kill-buffer))))

(setq org-publish-project-alist
      '(("blog-notes"
	 :base-directory "~/blog/org"
	 :rss-directory "~/blog/html/blog"
	 :rss-filename "blog.xml"
	 :rss-title "Pom Pom Galli"
         :style-include-default nil
         :publishing-directory "~/blog/html"
         :table-of-contents nil
         :section-numbers nil
         :timestamp t
         :author "Sébastien Rannou"
         :base-extension "org"
         :recursive t 
         :publishing-function org-publish-org-to-html
         :htmlized-source t
         :html-preamble my-blog-header
	 :completion-function my-blog-sitemap
         :html-postamble my-blog-footer
         :headline-levels 4
	 ; required for the sitemap
	 :auto-index t
	 :sitemap-function my-blog-sitemap
	 :publishing-url "http://bender.sbrk.org/")
	("blog-static"
	 :base-directory "~/blog/org/"
	 :publishing-directory "~/blog/html/"
	 :base-extension "css\\|js\\|png"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("blog"
	 :components ("blog-notes" "blog-static"))))

(setq org-export-html-style (my-blog-styles '("kaneda.css")))
(define-key org-mode-map "\C-cp" (lambda ()
				   (interactive)
				   (org-publish-project "blog" t)))

(provide 'my-blog)
