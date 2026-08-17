;;; ox-epub3-struct.el --- EPUB 3 structure generation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicola Ferru Aka NFVblog

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ox-epub3-kernel)

;;; --- File Writing ---

(defun org-epub3--write-file (path content)
  "Write CONTENT to PATH."
  (with-temp-file path (insert content)))

;;; --- Mimetype ---

(defun org-epub3--generate-mimetype (temp-dir)
  "Generate mimetype file in TEMP-DIR."
  (org-epub3--write-file (concat temp-dir "/mimetype") "application/epub+zip"))

;;; --- Container ---

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

;;; --- Navigation (toc.xhtml) ---

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

;;; --- NCX (EPUB 2 backward compat) ---

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

;;; --- OPF Package ---

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

(provide 'ox-epub3-struct)
;;; ox-epub3-struct.el ends here
