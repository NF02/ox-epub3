;;; ox-epub3.el --- Export Org-mode to EPUB 3.3 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicola Ferru Aka NFVblog

;; Author: Nicola Ferru Aka NFVblog
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0"))
;; Keywords: epub, org-mode, export, ebook
;; URL: https://github.com/NF02/ox-epub3

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package exports Org-mode files to EPUB 3.3 format compliant
;; with W3C specifications.  It generates valid EPUB files that pass
;; epubcheck validation.
;;
;; Usage:
;;   M-x org-epub3-export-as-epub
;;   C-c C-e E e
;;
;; Configuration via #+EPUB3_* keywords or setq variables.

;;; Code:

(require 'ox-html)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(require 'ox-epub3-kernel)
(require 'ox-epub3-struct)
(require 'ox-epub3-latex)
(require 'ox-epub3-style)

;;; --- Customization ---

(defgroup ox-epub3 nil
  "Export Org-mode to EPUB 3."
  :prefix "org-epub3-"
  :group 'org-export)

(defcustom org-epub3-zip-command "zip"
  "Command to create ZIP archives."
  :type 'string
  :group 'ox-epub3)

(defcustom org-epub3-default-language "it"
  "Default language for EPUB metadata (BCP 47)."
  :type 'string
  :group 'ox-epub3)

(defcustom org-epub3-page-progression "ltr"
  "Page progression direction: ltr, rtl, or default."
  :type '(choice (const "ltr") (const "rtl") (const "default"))
  :group 'ox-epub3)

(defcustom org-epub3-toc-depth 3
  "Maximum depth for table of contents."
  :type 'integer
  :group 'ox-epub3)

(defcustom org-epub3-layout "reflowable"
  "Layout type: reflowable or pre-paginated."
  :type '(choice (const "reflowable") (const "pre-paginated"))
  :group 'ox-epub3)

(defcustom org-epub3-identifier-prefix "urn:uuid:"
  "Identifier prefix.  UUID will be appended."
  :type 'string
  :group 'ox-epub3)

(defcustom org-epub3-use-ncx t
  "If non-nil, include NCX for EPUB 2 backward compatibility."
  :type 'boolean
  :group 'ox-epub3)

(defcustom org-epub3-accessibility-summary
  "This publication conforms to EPUB Accessibility 1.1."
  "Accessibility summary text."
  :type 'string
  :group 'ox-epub3)

(defcustom org-epub3-cover-image nil
  "Path to cover image file."
  :type '(choice (const nil) file)
  :group 'ox-epub3)

;;; --- Image Handling ---

(defun org-epub3--collect-images (tree)
  "Collect all image file paths referenced in TREE.
Returns a list of absolute file paths."
  (let ((images nil))
    (org-element-map tree 'link
      (lambda (link)
        (when (equal (org-element-property :type link) "file")
          (let ((path (org-element-property :path link)))
            (when (and path
                       (string-match-p "\\`\\.?\\.?/\\|^[^:]+\\.[a-z]+\\'" path)
                       (file-exists-p path)
                       (string-match-p
                        "\\`\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)\\'"
                        (downcase (file-name-extension path))))
              (push path images))))))
    (nreverse images)))

(defun org-epub3--register-image (img-file img-dir manifest)
  "Register IMG-FILE in MANIFEST, copying it to IMG-DIR if needed.
Returns updated MANIFEST."
  (let* ((fname (file-name-nondirectory img-file))
         (ext (file-name-extension fname))
         (id (concat "img-" (replace-regexp-in-string "[^a-zA-Z0-9]" "-" fname)))
         (href (concat "EPUB/images/" fname))
         (mime (org-epub3--mime ext)))
    (when (file-exists-p img-file)
      (make-directory img-dir t)
      (copy-file img-file (concat img-dir "/" fname) t))
    (cons (list :id id :href href :type mime) manifest)))

;;; --- Chapter Extraction ---

(defun org-epub3--chapter-body (hl buf)
  "Extract the raw Org text of headline HL from BUF, including sub-headlines."
  (let ((begin (org-element-property :begin hl))
        (end (org-element-property :end hl)))
    (when (and begin end)
      (with-current-buffer buf
        (let ((raw (buffer-substring-no-properties begin end)))
          (string-trim-right raw))))))

;;; --- Main Export ---

(defun org-epub3--export-to-file (epub-file)
  "Export current Org buffer to EPUB-FILE."
  (let ((org-epub3--uuid (concat org-epub3-identifier-prefix (org-epub3--uuid)))
        (org-epub3--chapter-counter 0)
        (org-epub3--manifest-items nil)
        (org-epub3--spine-items nil)
        (org-epub3--nav-items nil)
        (org-epub3--ncx-points nil)
        (org-epub3--chapters nil)
        (org-epub3--has-svg nil)
        (temp-dir (make-temp-file "ox-epub3-" t)))
    (unwind-protect
        (let* ((info (org-export-get-environment 'epub3))
               (raw-title (plist-get info :title))
               (title (if raw-title
                          (org-epub3--to-string raw-title) "Untitled"))
               (raw-author (plist-get info :author))
               (author (when raw-author
                         (org-epub3--to-string raw-author)))
               (raw-email (plist-get info :email))
               (email (when (and raw-email (stringp raw-email))
                        (org-epub3--to-string raw-email)))
               (cover (plist-get info :epub3-cover-image))
               (tree (org-element-parse-buffer))
               (oebps (concat temp-dir "/OEBPS"))
               (text (concat oebps "/EPUB/text"))
               (img  (concat oebps "/EPUB/images"))
               (css  (concat oebps "/EPUB/styles")))
          ;; Create directories
          (make-directory text t)
          (make-directory css t)

          ;; mimetype, container.xml
          (org-epub3--generate-mimetype temp-dir)
          (org-epub3--generate-container temp-dir)

          ;; CSS
          (org-epub3--write-file (concat css "/style.css")
                                 (org-epub3--default-css))

          ;; Copy extra CSS
          (dolist (f (plist-get info :epub3-style))
            (when (file-exists-p f)
              (copy-file f (concat css "/" (file-name-nondirectory f)) t)))

          ;; Cover
          (when (and cover (file-exists-p cover))
            (let ((fname (file-name-nondirectory cover)))
              (make-directory img t)
              (copy-file cover (concat img "/" fname) t)
              (org-epub3--write-file
               (concat text "/cover.xhtml")
               (org-epub3--xhtml
                "Copertina"
                (concat "<div class=\"cover\">\n"
                        "  <img src=\"../images/" (org-epub3--esc fname)
                        "\" alt=\"Copertina\"/>\n"
                        "  <h1>" (org-epub3--esc title) "</h1>\n"
                        (when author
                          (concat "  <div class=\"author\">"
                                  (org-epub3--esc author) "</div>\n"))
                        (when email
                          (concat "  <div class=\"email\">"
                                  (org-epub3--esc email) "</div>\n"))
                        "</div>\n")))
              (push (list :id "cover" :href "EPUB/text/cover.xhtml"
                          :type "application/xhtml+xml")
                    org-epub3--manifest-items)
              (push (list :idref "cover" :linear "yes")
                    org-epub3--spine-items)))

          ;; Title page
          (org-epub3--write-file
           (concat text "/titlepage.xhtml")
           (org-epub3--xhtml
            title
            (concat "<div class=\"title-page\">\n"
                    "  <h1>" (org-epub3--esc title) "</h1>\n"
                    (when author
                      (concat "  <div class=\"author\">"
                              (org-epub3--esc author) "</div>\n"))
                    (when email
                      (concat "  <div class=\"email\">"
                              (org-epub3--esc email) "</div>\n"))
                    "</div>\n")))
          (push (list :id "titlepage" :href "EPUB/text/titlepage.xhtml"
                      :type "application/xhtml+xml")
                org-epub3--manifest-items)
          (push (list :idref "titlepage" :linear "yes")
                org-epub3--spine-items)

          ;; Collect images from document tree, copy and register in manifest
          (let ((raw-images (org-epub3--collect-images tree))
                (seen nil))
            (dolist (img-file raw-images)
              (unless (member img-file seen)
                (push img-file seen)
                (when (file-exists-p img-file)
                  (setq org-epub3--manifest-items
                        (org-epub3--register-image
                         img-file img org-epub3--manifest-items))))))

          ;; Extract level-1 headlines as chapters
          (let ((headlines (org-element-map tree 'headline
                             (lambda (hl)
                               (when (= (org-element-property :level hl) 1) hl))))
                (buf (current-buffer)))
            (dolist (hl headlines)
              (cl-incf org-epub3--chapter-counter)
              (let* ((ch-num org-epub3--chapter-counter)
                     (ch-id (format "ch%03d" ch-num))
                     (ch-file (format "ch%03d.xhtml" ch-num))
                     (ch-href (concat "EPUB/text/" ch-file))
                     (ch-title (org-epub3--to-string
                                (org-element-property :raw-value hl)))
                     (ch-raw (org-epub3--chapter-body hl buf))
                     (org-epub3--has-svg nil)
                     (ch-html (org-export-string-as
                               (or ch-raw "") 'epub3 t
                               '(:html-doctype "html5"
                                 :html-preamble nil
                                 :html-postamble nil
                                 :html-head nil
                                 :html-head-extra nil
                                 :html-toc nil
                                 :html-validation-link nil
                                 :html-link-home ""
                                 :html-link-up ""
                                 :with-toc nil)))
                     (ch-has-svg org-epub3--has-svg)
                     (ch-contents (if (string-match "<body[^>]*>\\(\\(?:.\\|\n\\)*\\)</body>" ch-html)
                                     (match-string 1 ch-html)
                                   ch-html)))
                ;; Write chapter XHTML
                (org-epub3--write-file
                 (concat text "/" ch-file)
                 (org-epub3--xhtml
                  ch-title
                  (concat (format "<h1>%s</h1>\n" (org-epub3--esc ch-title))
                          ch-contents)))
                ;; Manifest
                (push (list :id ch-id :href ch-href
                            :type "application/xhtml+xml"
                            :props (when ch-has-svg "svg"))
                      org-epub3--manifest-items)
                ;; Spine
                (push (list :idref ch-id :linear "yes")
                      org-epub3--spine-items)
                ;; Nav
                (push (list :label ch-title :href ch-href)
                      org-epub3--nav-items)
                ;; NCX
                (push (list :id ch-id :label ch-title :href ch-file)
                      org-epub3--ncx-points)
                ;; Track
                (push (list :id ch-id :href ch-href)
                      org-epub3--chapters))))

          ;; Generate OPF, nav, NCX
          (org-epub3--generate-nav oebps info)
          (when org-epub3-use-ncx
            (org-epub3--generate-ncx oebps info))
          (org-epub3--generate-opf oebps info)

          ;; Create ZIP
          (let ((default-directory temp-dir))
            (call-process org-epub3-zip-command nil nil nil
                          "-Xu0" epub-file "mimetype")
            (call-process org-epub3-zip-command nil nil nil
                          "-Xur9" epub-file "META-INF" "OEBPS"))

          (message "EPUB exported: %s" epub-file))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; --- Interactive ---

;;;###autoload
(defun org-epub3-export-as-epub (&optional _async _subtreep _visible-only _body-only)
  "Export current Org buffer to an EPUB 3.3 file."
  (interactive)
  (let ((epub-file (concat (file-name-sans-extension
                            (or (buffer-file-name) (buffer-name)))
                           ".epub")))
    (org-epub3--export-to-file epub-file)
    (when (file-exists-p epub-file)
      (dired-jump nil epub-file))))

;;;###autoload
(defun org-epub3-export-to-epub-and-open (&optional _async _subtreep _visible-only _body-only)
  "Export current Org buffer to EPUB and open in default reader."
  (interactive)
  (let ((epub-file (concat (file-name-sans-extension
                            (or (buffer-file-name) (buffer-name)))
                           ".epub")))
    (org-epub3--export-to-file epub-file)
    (when (file-exists-p epub-file)
      (browse-url epub-file))))

;;; --- Link Transcoding ---

(defun org-epub3--link (link contents info)
  "Transcode LINK element to XHTML.
CONTENTS is the transcoded content.  INFO is the export plist.
Rewrites image paths to use ../images/ for EPUB structure."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (alt (let ((desc (org-element-property :description link)))
               (when desc (org-epub3--to-string desc)))))
    (pcase type
      ("file"
       (if (member (downcase (file-name-extension path))
                   '("jpg" "jpeg" "png" "gif" "svg" "webp"))
           (let* ((fname (file-name-nondirectory path))
                  (href (concat "../images/" fname)))
             (concat "<img src=\"" (org-epub3--esc href) "\""
                     (when alt
                       (concat " alt=\"" (org-epub3--esc alt) "\""))
                     "/>"))
         ;; Non-image file links: use default HTML transcoder
         (org-html-link link contents info)))
      (_
       (org-html-link link contents info)))))

;;; --- Backend Definition ---

(org-export-define-derived-backend 'epub3 'html
  :menu-entry
  '(?E "Export to EPUB 3"
       ((?e "As EPUB file" org-epub3-export-as-epub)
        (?o "As EPUB and open" org-epub3-export-to-epub-and-open)))
  :options-alist
  '((:epub3-cover-image "EPUB3_COVER_IMAGE" nil org-epub3-cover-image)
    (:epub3-style "EPUB3_STYLE" nil nil)
    (:epub3-layout "EPUB3_LAYOUT" nil org-epub3-layout)
    (:epub3-toc-depth "EPUB3_TOC_DEPTH" nil org-epub3-toc-depth parse-int)
    (:epub3-page-progression "EPUB3_PAGE_PROGRESSION" nil org-epub3-page-progression))
  :translate-alist
  '((template . org-epub3--template)
    (link . org-epub3--link)
    (latex-fragment . org-epub3--latex-fragment)
    (latex-environment . org-epub3--latex-environment))
  :filters-alist
  '((:filter-parse-tree . org-epub3--filter-tree)))

(defun org-epub3--filter-tree (tree _backend _info)
  "Filter TREE for epub3 export.  Pass through unchanged."
  tree)

(defun org-epub3--template (contents _info)
  "Template function.  CONTENTS is raw HTML."
  contents)

(provide 'ox-epub3)
;;; ox-epub3.el ends here
