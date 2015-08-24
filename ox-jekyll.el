;;; ox-jekyll.el --- Export Jekyll articles using org-mode.

;; Copyright (C) 2013  Yoshinari Nomura

;; Author: Yoshinari Nomura <nom@quickhack.net>
;; Author: Justin Gordon <justin.gordon@gmail.com>
;; Keywords: org, jekyll
;; Version: 0.1

;;; Commentary:

;; This library implements a Jekyll-style html backend for
;; Org exporter, based on `html' back-end.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-jkl-export-as-html' (temporary buffer) and
;; `org-jkl-export-to-html' ("html" file with YAML front matter).
;;
;; For publishing, `org-jekyll-publish-to-html' is available.
;; For composing, `org-jekyll-insert-export-options-template' is available.

;;; Code:

;;; Dependencies

(require 'ox-html)

;;; User Configurable Variables

(defgroup org-export-jekyll nil
  "Options for exporting Org mode files to jekyll HTML."
  :tag "Org Jekyll"
  :group 'org-export
  :version "24.2")

(defcustom org-jekyll-include-yaml-front-matter t
  "If true, then include yaml-front-matter when exporting to html.

If false, then you should include the yaml front matter like this at the top of the file:

#+BEGIN_HTML
---
layout: post
title: \"Upgrading Octopress\"
date: 2013-09-15 22:08
comments: true
categories: [octopress, rubymine]
tags: tech news
keywords: Octopress
description: Instructions on Upgrading Octopress
---
#+END_HTML"
  :group 'org-export-jekyll
  :type 'boolean)


(defcustom org-jekyll-layout "post"
  "Default layout used in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-categories ""
  "Default space-separated categories in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-tags ""
  "Default space-separated tags in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-published "true"
  "Default publish status in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-comments ""
  "Default comments (disqus) flag in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-use-src-plugin nil
   "If t, org-jekyll exporter eagerly uses plugins instead of
org-mode's original HTML stuff. For example:

   #+BEGIN_SRC ruby
     puts \"Hello world\"
   #+END_SRC

makes:

  {% codeblock ruby %}
  puts \"Hello world\"
  {% endcodeblock %}"
  :group 'org-export-jekyll-use-src-plugin
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'jekyll 'html
  :export-block '("HTML" "JEKYLL")
  :menu-entry
  '(?j "Jekyll: export to HTML with YAML front matter."
       ((?H "As HTML buffer" org-jekyll-export-as-html)
        (?h "As HTML file" org-jekyll-export-to-html)))
  :translate-alist
  '((template . org-jekyll-template) ;; add YAML front matter.
    (src-block . org-jekyll-src-block)
    (inner-template . org-jekyll-inner-template)) ;; force body-only
  :options-alist
  '((:jekyll-layout "JEKYLL_LAYOUT" nil org-jekyll-layout)
    (:jekyll-categories "JEKYLL_CATEGORIES" nil org-jekyll-categories)
    (:jekyll-tags "JEKYLL_TAGS" nil org-jekyll-tags)
    (:jekyll-published "JEKYLL_PUBLISHED" nil org-jekyll-published)
    (:jekyll-comments "JEKYLL_COMMENTS" nil org-jekyll-comments)))


;;; Internal Filters


(defun org-jekyll-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into jekyll code template format
if `org-jekyll-use-src-plugin` is t. Otherwise, perform as
`org-html-src-block`. CONTENTS holds the contents of the item.
INFO is a plist used as a communication channel."
  (if org-jekyll-use-src-plugin
      (let ((language (org-element-property :language src-block))
            (value (org-remove-indentation
                    (org-element-property :value src-block))))
        (format "{%% codeblock lang:%s %%}\n%s{%% endcodeblock %%}"
                language value))
    (org-export-with-backend 'html src-block contents info)))


;;; Template

(defun org-jekyll-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (if org-jekyll-include-yaml-front-matter
      (concat
       (org-jekyll--yaml-front-matter info)
       contents)
    contents
    ))

(defun org-jekyll-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; PREVIEW mark on the top of article.
   (unless (equal "true" (plist-get info :jekyll-published))
     "<span style=\"background: red;\">PREVIEW</span>")
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

;;; YAML Front Matter

(defun org-jekyll--get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (format "%s" (or property default ""))))

(defun org-jekyll--yaml-front-matter (info)
  (let ((title
         (org-jekyll--get-option info :title))
        (date
         (org-jekyll--get-option info :date))
        (layout
         (org-jekyll--get-option info :jekyll-layout org-jekyll-layout))
        (categories
         (org-jekyll--get-option info :jekyll-categories org-jekyll-categories))
        (tags
         (org-jekyll--get-option info :jekyll-tags org-jekyll-tags))
        (published
         (org-jekyll--get-option info :jekyll-published org-jekyll-published))
        (comments
         (org-jekyll--get-option info :jekyll-comments))
        (convert-to-yaml-list
         (lambda (arg)
           (mapconcat #'(lambda (text)(concat "\n- " text)) (split-string arg) " "))))
    (unless (equal published "true")
      (setq title (concat "[PREVIEW] " title)))
    (concat
     "---"
     "\ntitle: \""    title
     "\"\ndate: "     date
     "\nlayout: "     layout
     "\ncategories: " (funcall convert-to-yaml-list  categories)
     "\ntags: "       (funcall convert-to-yaml-list tags)
     "\npublished: "  published
     "\ncomments: "   comments
     "\n---\n")))

;;; Filename and Date Helper

(defun org-jekyll-date-from-filename (&optional filename)
  (let ((fn (file-name-nondirectory (or filename (buffer-file-name)))))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      nil)))

(defun org-jekyll-property-list (&optional filename)
  (let ((backend 'jekyll) plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename)))
      (setq plist (org-export-backend-options backend))
      plist)))

(defun org-jekyll-property (keys &optional filename)
  (let ((plist (org-jekyll-property-list filename)))
    (mapcar (lambda (key)
              (let ((value (plist-get plist key)))
                (setq value (if (listp value) (car value) value))
                (if (stringp value)
                    (substring-no-properties value))))
            keys)))

(defun org-jekyll-date-from-property (&optional filename)
  (let ((plist (org-jekyll-property filename)))
    (org-read-date
     nil nil
     (org-export-data-with-backend (plist-get plist :date) 'jekyll plist))))

(defun org-jekyll-create-filename ()
  (let ((date (org-jekyll-date-from-property))
        (file (file-name-nondirectory (buffer-file-name)))
        (dir  (file-name-directory (buffer-file-name))))
    (expand-file-name
     (replace-regexp-in-string "^[0-9]+-[0-9]+-[0-9]+" date file)
     dir)))

;;; End-User functions

;;;###autoload
(defun org-jekyll-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML buffer adding some YAML front matter."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Jekyll HTML Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (funcall org-html-display-buffer-mode)
              (org-export-add-to-stack (current-buffer) 'jekyll)))
        `(org-export-as 'jekyll ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                   'jekyll "*Org Jekyll HTML Export*"
                   nil subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-jekyll-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file adding some YAML front matter."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'jekyll))
          (let ((org-export-coding-system org-html-coding-system))
            `(expand-file-name
              (org-export-to-file
               'jekyll ,file nil ,subtreep ,visible-only ,body-only ',ext-plist))))
      (let ((org-export-coding-system org-html-coding-system))
        (org-export-to-file
         'jekyll file nil subtreep visible-only body-only ext-plist)))))

;;;###autoload
(defun org-jekyll-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'jekyll filename ".html" plist pub-dir))

;;;###autoload
(defun org-jekyll-insert-export-options-template
  (&optional title date setupfile categories tags published layout)
  "Insert a settings template for Jekyll exporter."
  (interactive)
  (let ((layout     (or layout org-jekyll-layout))
        (published  (or published org-jekyll-published))
        (tags       (or tags org-jekyll-tags))
        (categories (or categories org-jekyll-categories)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: "             title
                       "\n#+DATE: "              date
                       "\n#+SETUPFILE: "         setupfile
                       "\n#+JEKYLL_LAYOUT: "     layout
                       "\n#+JEKYLL_CATEGORIES: " categories
                       "\n#+JEKYLL_TAGS: "       tags
                       "\n#+JEKYLL_PUBLISHED: "  published
                       "\n\n* \n\n{{{more}}}"))))))

;;; provide

(provide 'ox-jekyll)

;;; ox-jekyll.el ends here
