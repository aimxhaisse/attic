;; author: s. rannou <mxs@sbrk.org>
;;
;; my blog

(require 'org-publish)
(require 'org-atom)
(require 'xmlgen)
(require 'htmlfontify)
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
			(a :href "/blog.html" "blog")))
                (a :href "/"
		   (img :id "dave" :src "/img/header.png")))))

(defun my-blog-footer ()
  "returns an htmlized string for the footer of the blog"
  (concat
   (xmlgen '(i :id "bottom-note" "This page was last updated on %d."))
   (xmlgen '(div :id "footer"
		 (hr)
		 (a :href "http://sbrk.org" "powered by sbrk.org")))))

(defun my-blog-styles (styles)
  "returns an htmlized string of css files to include"
  (if styles
      (concat (xmlgen
	       `(link :href ,(concat "/css/" (car styles))
		      :rel "stylesheet"
		      :style "text/css"))
	      (my-blog-styles (cdr styles))) nil))

;; blog pages are excluded from the blog sitemap
(setq my-blog-settings `("blog-pages"
	 :base-directory "~/blog/org/"
         :style-include-default nil
         :publishing-directory "~/blog/html/"
         :author "Sébastien Rannou"
	 :base-extension "org"
         :htmlized-source t
         :html-preamble my-blog-header
	 :html-postamble ,(my-blog-footer)
	 :table-of-contents nil
         :headline-levels 4
	 :section-numbers nil
	 :publishing-url "http://mxs.sbrk.org/"))

;; blog entries and the blog sitemap
(setq my-blog-sitemap (cons "blog-sitemap"
			    (append (list :auto-sitemap t
					  :base-directory "~/blog/org/posts/"
					  :sitemap-sort-files 'anti-chronologically
					  :sitemap-sort-folders 'last
					  :sitemap-filename "blog.html"
					  :sitemap-file-entry-format "%t (%d)"
					  :sitemap-title "Blog Entries")
				    (cdr my-blog-settings))))

(setq org-publish-project-alist
      `(,my-blog-sitemap
	,my-blog-settings
	("blog-static"
	 :base-directory "~/blog/org/"
	 :publishing-directory "~/blog/html/"
	 :base-extension "css\\|js\\|png\\|jpg"
	 :recursive t
	 :section-numbers nil
	 :publishing-function org-publish-attachment)
	("blog"
	 :components ("blog-pages" "blog-sitemap" "blog-static"))))

(setq org-export-html-style (my-blog-styles '("kaneda.css")))
(define-key org-mode-map "\C-cp" (lambda ()
				   (interactive)
				   (org-publish-project "blog" t)))

(provide 'my-blog)
