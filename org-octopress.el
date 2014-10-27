;;; org-octopress.el --- Compose octopress articles using org-mode.

;; Copyright (C) 2013 Yoshinari Nomura.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Keywords: org, jekyll, octopress, blog
;; Version: 0.1
;; Package-Requires: ((org "8.0") (orglue "0.1") (ctable "0.1.1"))
;;; Commentary:

;; Basic settings:
;;
;; (setq org-octopress-directory-top       "~/octopress/source")
;; (setq org-octopress-directory-posts     "~/octopress/source/_posts")
;; (setq org-octopress-directory-org-top   "~/octopress/source")
;; (setq org-octopress-directory-org-posts "~/octopress/source/blog")
;; (setq org-octopress-setup-file          "~/lib/org-sty/setupfile.org")
;;
;; M-x org-octopress
;;
;; Note:
;;  In octopress/_config.yml, you must set the permelink attribute:
;;    permalink: /blog/:year-:month-:day-:title.html
;;

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'orglue-publish)
(require 'ox-jekyll)
(require 'ctable)

;;; Publishing

;; Assumed directory tree:
;;
;; + octopress
;;   + source
;;     + blog   <- (1) You compose YYYY-MM-DD-title.org
;;     + _posts <- (2) ox-jekyll.el exports to YYYY-MM-DD-title.html (w/ YAML)
;;   + public
;;     + blog   <- (3) Jekyll exports to YYYY-MM-DD-title.html (w/o YAML).
;;

(defvar org-octopress-directory-top       "~/octopress/source")
(defvar org-octopress-directory-posts     "~/octopress/source/_posts")
(defvar org-octopress-directory-org-top   "~/octopress/source")
(defvar org-octopress-directory-org-posts "~/octopress/source/blog")
(defvar org-octopress-setup-file          "~/sys/lib/org-sty/octopress.org")
(defvar org-octopress-component           nil)

(add-hook 'orglue-before-export-dispatch-hook 'org-octopress-setup-publish-project)

(defun org-octopress-setup-publish-project ()
  (let* ((oct-top   org-octopress-directory-top)
         (org-top   org-octopress-directory-org-top)
         (oct-posts org-octopress-directory-posts)
         (org-posts org-octopress-directory-org-posts))
    (orglue-update-publish-project-alist
     'org-publish-project-alist
     `(
       ("octopress" :components ("octopress-posts" "octopress-org"))
       ("octopress-posts"
        :base-directory ,org-posts
        :publishing-directory ,oct-posts
        :base-extension "org"
        :recursive nil
        :exclude "/[^0-9][^/]+\\.org$" ;; XXXX
        :publishing-function org-jekyll-publish-to-html
        )
       ("octopress-org"
        :base-directory ,org-top
        :publishing-directory ,oct-top
        :base-extension "org"
        :exclude "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]-.*\\.org$"
        :recursive t
        :publishing-function org-jekyll-publish-to-html
        )
       ("octopress-static"
        :base-directory ,org-top
        :publishing-directory ,oct-top
        :base-extension ".*"
        :exclude "\\.org$"
        :recursive t
        :publishing-function org-publish-attachment
        )
       ))))

(defvar org-octopress-summary-buffer nil
  "Main buffer, showing summary table")

(defun org-octopress-refresh ()
  "Refresh \"Octopress\" buffer."
  (interactive)
  (when org-octopress-summary-buffer
    (kill-buffer org-octopress-summary-buffer)
    (org-octopress)))

;;; Summary Mode

;; keymap
(defvar org-octopress-summary-mode-map  nil
  "Keymap for `org-octopress-summary-mode'.")

(defvar org-octopress-summary-mode-hook nil)

(defun org-octopress--merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))

(unless org-octopress-summary-mode-map
  (setq org-octopress-summary-mode-map (make-sparse-keymap))
  (define-key org-octopress-summary-mode-map "w" 'org-octopress-new-post)
  (define-key org-octopress-summary-mode-map "d" 'org-octopress-delete-post)
  (define-key org-octopress-summary-mode-map "r" 'org-octopress-refresh)
  (setq org-octopress-summary-mode-map
        (org-octopress--merge-keymap org-octopress-summary-mode-map ctbl:table-mode-map)))

;; new post
(defun org-octopress-new-post (&optional title date)
  "New post."
  (interactive "sPermalink Text: ")
  (let ((date (or date (org-read-date)))
        (post-path (expand-file-name
                    (org-octopress--new-post-file-name title date)
                    org-octopress-directory-org-posts)))
    (find-file post-path)
    (save-excursion
      (org-jekyll-insert-export-options-template title date org-octopress-setup-file nil "true"))
    (save-buffer)
    (org-octopress-refresh)
    (find-file post-path)
    (search-forward "TITLE: " nil t)))

;; delete post
(defun org-octopress-delete-post ()
  "Delete existing post."
  (interactive)
  (let ((org-post-path (nth 4 (ctbl:cp-get-selected-data-row org-octopress-component))))
    (delete-file org-post-path)
    (let ((html-post-path
         (concat (substring
                  (replace-regexp-in-string
                   (regexp-quote (expand-file-name org-octopress-directory-org-posts))
                   (expand-file-name org-octopress-directory-posts)
                   org-post-path) 0 -4) ".html")))
    (ignore-errors (delete-file html-post-path))))
  (org-octopress-refresh))

;; summary 
(defun org-octopress-summary-mode ()
  "Major mode for listing and controlling org-mode based blog articles.

\\{org-octopress-summary-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map org-octopress-summary-mode-map)
  (setq major-mode 'org-octopress-summary-mode
        mode-name  "Org-Octopress")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'org-octopress-summary-mode-hook))

(defun org-octopress--summary-header (&optional title)
  (concat
   (format "%s\n" (or title "Octopress"))
   (mapconcat
    'identity
    (org-octopress--summary-command-help
     (cl-remove-duplicates
      (mapcar 'cdr (cdr org-octopress-summary-mode-map)))
     org-octopress-summary-mode-map)
    "\n")
   "\n\n\n"))

(defun org-octopress--summary-command-help (symbols &optional keymap)
  (let (symbol keysym keystr docstr summary-list)
    (while (setq symbol (car symbols))
      (setq keysym (where-is-internal symbol (or keymap (current-local-map)) nil)
            keystr (if keysym (mapconcat 'key-description keysym ",") "No keybind")
            docstr (documentation symbol))
      (if docstr
          (setq summary-list (cons (format "%10s ... %s (%s)"
                                           keystr
                                           (car (split-string docstr "\n"))
                                           symbol)
                                   summary-list)))
      (setq symbols (cdr symbols)))
    summary-list))

(defun org-octopress--summary-table (contents keymap)
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (ctbl:create-table-component-region
     :param param
     :width  nil
     :height nil
     :keymap keymap
     :model
     (make-ctbl:model
      :data contents
      :sort-state '(-1 2)
      :column-model
      (list (make-ctbl:cmodel
             :title "Date"
             :sorter 'ctbl:sort-string-lessp
             :min-width 10
             :align 'left)
            (make-ctbl:cmodel
             :title "Category"
             :align 'left
             :sorter 'ctbl:sort-string-lessp)
            (make-ctbl:cmodel
             :title "Title"
             :align 'left
             :min-width 40
             :max-width 140)
            (make-ctbl:cmodel
             :title "Published"
             :align 'left
             :min-width 4
             :max-width 5)
            )))))

(defun org-octopress--scan-post ()
  (mapcar
   (lambda (filename)
     (org-jekyll-property
      '(:date
        :jekyll-categories
        :title
        :jekyll-published
        :input-file)
      filename))
   (directory-files
    (expand-file-name
     org-octopress-directory-org-posts) t "^[0-9].*\\.org$")))

;; startup
(defun org-octopress (&optional title)
  "Org-mode and Octopress."
  (interactive)
  (setq org-octopress-summary-buffer (get-buffer-create "Octopress"))
  (switch-to-buffer org-octopress-summary-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (org-octopress--summary-header title))
  (save-excursion
    (setq org-octopress-component (org-octopress--summary-table
                                   (org-octopress--scan-post) org-octopress-summary-mode-map)))
  (ctbl:cp-add-click-hook
   org-octopress-component
   (lambda ()
     (find-file (nth 4 (ctbl:cp-get-selected-data-row org-octopress-component)))))
  (org-octopress-summary-mode)
  (ctbl:navi-goto-cell
   (ctbl:find-first-cell (ctbl:component-dest org-octopress-component)))
  )

;;; Helpers

(defun org-octopress--sanitize-title (title)
  (replace-regexp-in-string "[\t ]+" "-" (downcase title)))

(defun org-octopress--new-post-file-name (title &optional date)
  (let ((time (if (stringp date) (org-read-date nil t date) date)))
    (format
     (format-time-string "%Y-%m-%d-%%s.org" time)
     (org-octopress--sanitize-title title))))

(provide 'org-octopress)

;;; org-octopress.el ends here
