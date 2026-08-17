;;; ox-epub3.el --- Export Org-mode to EPUB 3.3 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicola Ferru Aka NFVblog

;; Author: Nicola Ferru Aka NFVblog
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0"))
;; Keywords: epub, org-mode, export, ebook
;; URL: https://github.com/user/ox-epub3

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

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

;;; --- Internal Variables ---

(defvar org-epub3--uuid nil
  "Generated UUID for current export.")
(defvar org-epub3--chapter-counter 0
  "Chapter counter for current export.")
(defvar org-epub3--manifest-items nil
  "Manifest items accumulated during export.")
(defvar org-epub3--spine-items nil
  "Spine items accumulated during export.")
(defvar org-epub3--nav-items nil
  "Navigation items for toc.xhtml.")
(defvar org-epub3--ncx-points nil
  "NCX navigation points.")
(defvar org-epub3--chapters nil
  "List of generated chapter plists.")
(defvar org-epub3--images nil
  "Image files to copy into EPUB.")

;;; --- Helpers ---

(defun org-epub3--uuid ()
  "Generate a UUID v4."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (+ 16384 (random 16384))
          (+ 32768 (random 16384))
          (random 65536) (random 65536) (random 65536)))

(defun org-epub3--iso-now ()
  "Return current time as ISO 8601 UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun org-epub3--to-string (data)
  "Convert DATA to a plain string, stripping text properties."
  (cond
   ((stringp data) (substring-no-properties data))
   ((symbolp data) (symbol-name data))
   ((numberp data) (number-to-string data))
   ((null data) "")
   ((listp data) (org-epub3--to-string (car data)))
   (t (substring-no-properties (format "%s" data)))))

(defun org-epub3--esc (text)
  "Escape TEXT for XML."
  (let ((s (org-epub3--to-string text)))
    (when (and s (not (string-empty-p s)))
      (setq s (replace-regexp-in-string "&" "&amp;" s))
      (setq s (replace-regexp-in-string "<" "&lt;" s))
      (setq s (replace-regexp-in-string ">" "&gt;" s))
      (setq s (replace-regexp-in-string "\"" "&quot;" s))
      s)))

(defun org-epub3--mime (ext)
  "Return MIME type for EXT."
  (pcase (downcase ext)
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("svg" "image/svg+xml")
    ("webp" "image/webp")
    ("ttf" "font/ttf")
    ("otf" "font/otf")
    ("woff" "font/woff")
    ("woff2" "font/woff2")
    ("css" "text/css")
    ((or "xhtml" "html" "htm") "application/xhtml+xml")
    (_ "application/octet-stream")))

;;; --- XHTML Wrapper ---

(defun org-epub3--xhtml (title body &optional lang css-path)
  "Wrap BODY in XHTML5 document with TITLE, LANG and CSS-PATH."
  (let ((lang (or lang org-epub3-default-language))
        (css-path (or css-path "../styles/style.css")))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<!DOCTYPE html>\n"
     "<html xmlns=\"http://www.w3.org/1999/xhtml\"\n"
     "      xmlns:epub=\"http://www.idpf.org/2007/ops\"\n"
     "      xml:lang=\"" (org-epub3--esc lang) "\">\n"
     "<head>\n"
     "  <title>" (org-epub3--esc (or title "")) "</title>\n"
     "  <link rel=\"stylesheet\" type=\"text/css\" href=\"" css-path "\"/>\n"
     "</head>\n"
     "<body>\n"
     body
     "</body>\n"
     "</html>\n")))

;;; --- Default CSS ---

(defun org-epub3--default-css ()
  "Default CSS for EPUB content."
  (concat
   "body {\n"
   "  font-family: Georgia, \"Times New Roman\", serif;\n"
   "  line-height: 1.6;\n"
   "  margin: 1em;\n"
   "  color: #333;\n"
   "}\n"
   "h1 { font-size: 1.8em; margin: 1.5em 0 0.8em; }\n"
   "h2 { font-size: 1.5em; margin: 1.3em 0 0.7em; }\n"
   "h3 { font-size: 1.3em; margin: 1.2em 0 0.6em; }\n"
   "p { margin: 0.8em 0; text-align: justify; }\n"
   "img { max-width: 100%; height: auto; }\n"
   "blockquote {\n"
   "  margin: 1em 2em;\n"
   "  padding: 0.5em 1em;\n"
   "  border-left: 3px solid #ccc;\n"
   "  font-style: italic;\n"
   "  color: #555;\n"
   "}\n"
   "pre, code {\n"
   "  font-family: monospace;\n"
   "  background: #f5f5f5;\n"
   "}\n"
   "pre {\n"
   "  padding: 1em;\n"
   "  overflow-x: auto;\n"
   "  white-space: pre-wrap;\n"
   "  word-wrap: break-word;\n"
   "}\n"
   "code { padding: 0.1em 0.3em; border-radius: 2px; }\n"
   "table { border-collapse: collapse; width: 100%%; margin: 1em 0; }\n"
   "th, td { border: 1px solid #ddd; padding: 0.5em; text-align: left; }\n"
   "th { background: #f0f0f0; font-weight: bold; }\n"
   "ul, ol { margin: 0.8em 0; padding-left: 2em; }\n"
   "li { margin: 0.3em 0; }\n"
   "a { color: #0066cc; text-decoration: none; }\n"
   ".title-page { text-align: center; margin-top: 25vh; }\n"
   ".title-page h1 { font-size: 2.5em; }\n"
   ".title-page .author { font-size: 1.3em; color: #666; margin-top: 0.5em; }\n"
   ".title-page .email { font-size: 1em; color: #888; }\n"
   ".cover { text-align: center; }\n"
   ".cover img { max-width: 100%; max-height: 60vh; }\n"
   ".cover h1 { font-size: 2em; margin: 0.5em 0 0.3em; }\n"
   ".cover .author { font-size: 1.3em; color: #666; margin-bottom: 0.2em; }\n"
   ".cover .email { font-size: 1em; color: #888; }\n"))

;;; --- EPUB Structure Generation ---

(defun org-epub3--write-file (path content)
  "Write CONTENT to PATH."
  (with-temp-file path (insert content)))

(defun org-epub3--generate-mimetype (temp-dir)
  "Generate mimetype file in TEMP-DIR."
  (org-epub3--write-file (concat temp-dir "/mimetype") "application/epub+zip"))

(defun org-epub3--generate-container (temp-dir)
  "Generate META-INF/container.xml in TEMP-DIR."
  (let ((meta (concat temp-dir "/META-INF")))
    (make-directory meta t)
    (org-epub3--write-file
     (concat meta "/container.xml")
     (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<container version=\"1.0\"\n"
             "           xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">\n"
             "  <rootfiles>\n"
             "    <rootfile full-path=\"OEBPS/content.opf\"\n"
             "              media-type=\"application/oebps-package+xml\"/>\n"
             "  </rootfiles>\n"
             "</container>\n"))))

(defun org-epub3--generate-nav (temp-dir info)
  "Generate toc.xhtml navigation document in TEMP-DIR using INFO."
  (let* ((title (or (plist-get info :title) "Indice"))
         (lang (or (plist-get info :language) org-epub3-default-language))
         (nav-items (nreverse org-epub3--nav-items)))
    (org-epub3--write-file
     (concat temp-dir "/toc.xhtml")
     (org-epub3--xhtml
      (concat (org-epub3--esc title) " - Indice")
      (concat
       "  <nav id=\"toc\" epub:type=\"toc\">\n"
       "    <h1>" (org-epub3--esc title) "</h1>\n"
       "    <ol>\n"
       (mapconcat
        (lambda (item)
          (let ((label (plist-get item :label))
                (href (plist-get item :href))
                (children (plist-get item :children)))
            (concat
             "      <li><a href=\"" (org-epub3--esc href) "\">"
             (org-epub3--esc label) "</a>"
             (when children
               (concat "\n        <ol>\n"
                       (mapconcat
                        (lambda (c)
                          (concat "          <li><a href=\""
                                  (org-epub3--esc (plist-get c :href))
                                  "\">" (org-epub3--esc (plist-get c :label))
                                  "</a></li>"))
                        children "\n")
                       "\n        </ol>"))
             "</li>")))
        nav-items "\n")
       "\n    </ol>\n"
       "  </nav>\n")
      lang "EPUB/styles/style.css"))))

(defun org-epub3--generate-ncx (temp-dir info)
  "Generate toc.ncx in TEMP-DIR using INFO."
  (let ((title (or (plist-get info :title) ""))
        (points (nreverse org-epub3--ncx-points)))
    (org-epub3--write-file
     (concat temp-dir "/toc.ncx")
     (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\"\n"
             "  \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">\n"
              "<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">\n"
             "<head>\n"
             "  <meta name=\"dtb:uid\" content=\""
             (org-epub3--esc org-epub3--uuid) "\"/>\n"
             "  <meta name=\"dtb:depth\" content=\""
             (number-to-string org-epub3-toc-depth) "\"/>\n"
             "  <meta name=\"dtb:totalPageCount\" content=\"0\"/>\n"
             "  <meta name=\"dtb:maxPageNumber\" content=\"0\"/>\n"
             "</head>\n"
             "<docTitle><text>" (org-epub3--esc title) "</text></docTitle>\n"
             "<navMap>\n"
             (mapconcat
              (lambda (p)
                (concat "  <navPoint id=\"" (plist-get p :id) "\">\n"
                        "    <navLabel><text>"
                        (org-epub3--esc (plist-get p :label))
                        "</text></navLabel>\n"
                         "    <content src=\""
                         (org-epub3--esc
                          (concat "EPUB/text/" (plist-get p :href)))
                         "\"/>\n"
                        "  </navPoint>"))
              points "\n")
             "\n</navMap>\n"
             "</ncx>\n"))))

(defun org-epub3--generate-opf (temp-dir info)
  "Generate content.opf in TEMP-DIR using INFO."
  (let* ((title (or (plist-get info :title) "Untitled"))
         (author (plist-get info :author))
         (lang (or (plist-get info :language) org-epub3-default-language))
         (date (or (plist-get info :date)
                   (format-time-string "%Y-%m-%d" (current-time) t)))
         (description (plist-get info :description))
         (cover (plist-get info :epub3-cover-image))
         (manifest (nreverse org-epub3--manifest-items))
         (spine (nreverse org-epub3--spine-items)))
    (org-epub3--write-file
     (concat temp-dir "/content.opf")
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<package xmlns=\"http://www.idpf.org/2007/opf\"\n"
      "         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n"
      "         unique-identifier=\"pub-id\"\n"
      "         version=\"3.0\">\n\n"
      "<metadata>\n"
      "  <dc:identifier id=\"pub-id\">" (org-epub3--esc org-epub3--uuid) "</dc:identifier>\n"
      "  <dc:title id=\"t1\">" (org-epub3--esc title) "</dc:title>\n"
      "  <dc:language>" (org-epub3--esc lang) "</dc:language>\n"
      "  <dc:date>" (org-epub3--esc date) "</dc:date>\n"
      (when author
        (concat "  <dc:creator id=\"creator1\">"
                (org-epub3--esc author) "</dc:creator>\n"))
      (when description
        (concat "  <dc:description>"
                (org-epub3--esc description) "</dc:description>\n"))
      "  <meta property=\"dcterms:modified\">"
      (org-epub3--iso-now) "</meta>\n"
      "  <meta refines=\"#t1\" property=\"title-type\">main</meta>\n"
      (when author
        "  <meta refines=\"#creator1\" property=\"role\" scheme=\"marc:relators\">aut</meta>\n")
      "  <meta property=\"schema:accessMode\">textual</meta>\n"
      "  <meta property=\"schema:accessModeSufficient\">textual</meta>\n"
      "  <meta property=\"schema:accessFeature\">alternativeText</meta>\n"
      "  <meta property=\"schema:accessFeature\">readingOrder</meta>\n"
      "  <meta property=\"schema:accessHazard\">none</meta>\n"
      "  <meta property=\"schema:accessibilitySummary\">"
      (org-epub3--esc org-epub3-accessibility-summary) "</meta>\n"
      "  <meta property=\"rendition:layout\">"
      (org-epub3--esc org-epub3-layout) "</meta>\n"
      "</metadata>\n\n"
      "<manifest>\n"
      "  <item id=\"nav\" href=\"toc.xhtml\" media-type=\"application/xhtml+xml\" properties=\"nav\"/>\n"
      (when org-epub3-use-ncx
        "  <item id=\"ncx\" href=\"toc.ncx\" media-type=\"application/x-dtbncx+xml\"/>\n")
      "  <item id=\"style\" href=\"EPUB/styles/style.css\" media-type=\"text/css\"/>\n"
      (when (and cover
                 (file-exists-p (concat temp-dir "/EPUB/images/"
                                        (file-name-nondirectory cover))))
        (concat "  <item id=\"cover-image\" href=\"EPUB/images/"
                (org-epub3--esc (file-name-nondirectory cover))
                "\" media-type=\"" (org-epub3--mime (file-name-extension cover))
                "\" properties=\"cover-image\"/>\n"))
      (mapconcat
       (lambda (item)
         (concat "  <item id=\"" (plist-get item :id) "\" href=\""
                 (org-epub3--esc (plist-get item :href))
                 "\" media-type=\"" (org-epub3--esc (plist-get item :type))
                 "\""
                 (when (plist-get item :props)
                   (concat " properties=\"" (org-epub3--esc (plist-get item :props)) "\""))
                 "/>"))
       manifest "\n")
      "\n</manifest>\n\n"
      "<spine"
      (when org-epub3-use-ncx " toc=\"ncx\"")
      (concat " page-progression-direction=\""
              (org-epub3--esc org-epub3-page-progression) "\"")
      ">\n"
      (mapconcat
       (lambda (s)
         (concat "  <itemref idref=\"" (plist-get s :idref)
                 "\" linear=\"" (or (plist-get s :linear) "yes") "\"/>"))
       spine "\n")
      "\n</spine>\n"
      "</package>\n"))))

;;; --- Chapter Extraction via org-html ---

(defun org-epub3--org-to-html ()
  "Export current buffer to HTML fragment."
  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t)
        (org-html-head-include-default-style nil)
        (org-html-head-include-scripts nil)
        (org-html-use-infojs nil)
        (org-html-preamble nil)
        (org-html-postamble nil)
        (org-html-link-home "")
        (org-html-link-up "")
        (org-html-validation-link nil))
    (org-export-as 'html nil nil t '(:html-doctype "html5"))))

(defun org-epub3--headline-to-xhtml (hl info)
  "Export headline element HL to XHTML body fragment using INFO."
  (let* ((level (org-element-property :level hl))
         (title (org-export-data (org-element-property :title hl) info))
         (todo (and (plist-get info :with-todo-keywords)
                    (org-element-property :todo-keyword hl)))
         (tags (and (plist-get info :with-tags)
                    (org-element-property :raw-value hl)
                    (org-element-property :tags hl))))
    (concat
     (format "<h%d>%s%s%s</h%d>\n"
             level
             (if todo (concat "<span class=\"todo-keyword\">"
                              (org-epub3--esc todo) "</span> ") "")
             (org-epub3--esc title)
             (if tags
                 (concat " "
                         (mapconcat (lambda (tag)
                                      (concat "<span class=\"tag\">"
                                              (org-epub3--esc tag) "</span>"))
                                    tags " "))
               "")
             level)
     (org-export-data (org-element-property :contents hl) info))))

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
        (org-epub3--images nil)
        (temp-dir (make-temp-file "ox-epub3-" t)))
    (unwind-protect
        (let* (          (info (org-export-get-environment 'epub3))
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

          ;; Extract headlines as chapters
          (let ((headlines (org-element-map tree 'headline
                             (lambda (hl) hl)))
                (buf (current-buffer)))
            (dolist (hl headlines)
              (cl-incf org-epub3--chapter-counter)
              (let* ((ch-num org-epub3--chapter-counter)
                     (ch-id (format "ch%03d" ch-num))
                     (ch-file (format "ch%03d.xhtml" ch-num))
                     (ch-href (concat "EPUB/text/" ch-file))
                     (ch-title (org-epub3--to-string
                                (org-element-property :raw-value hl)))
                      (ch-begin (org-element-property :contents-begin hl))
                      (ch-end (org-element-property :contents-end hl))
                      (ch-raw (when (and ch-begin ch-end)
                                (with-current-buffer buf
                                  (buffer-substring-no-properties ch-begin ch-end))))
                      (ch-html (org-export-string-as
                                (or ch-raw "") 'html t
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
                            :type "application/xhtml+xml")
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

          ;; Copy images found in document
          (org-element-map tree 'link
            (lambda (link)
              (when (eq (org-element-property :type link) 'file)
                (let ((path (org-element-property :path link)))
                  (when (and path
                             (string-match-p "\\`\\.?\\.?/\\|^[^:]+\\.[a-z]+\\'" path)
                             (file-exists-p path)
                             (string-match-p
                              "\\`\\.\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)\\'"
                              (downcase (file-name-extension path))))
                    (push path org-epub3--images))))))
          ;; Deduplicate and copy images
          (let ((seen nil))
            (dolist (img-file (nreverse org-epub3--images))
              (unless (member img-file seen)
                (push img-file seen)
                (when (file-exists-p img-file)
                  (make-directory img t)
                  (copy-file img-file
                             (concat img "/" (file-name-nondirectory img-file))
                             t)))))

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
  '((template . org-epub3--template))
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
