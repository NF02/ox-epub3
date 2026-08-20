;;; ox-epub3-style.el --- EPUB 3 CSS and XHTML wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicola Ferru Aka NFVblog

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ox-epub3-kernel)

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
   ".cover .email { font-size: 1em; color: #888; }\n"
   ".math { display: inline; vertical-align: middle; }\n"
   ".math svg { display: inline; vertical-align: middle; }\n"
   ".equation { text-align: center; margin: 1em 0; }\n"
   ".equation svg { display: block; margin: 0 auto; }\n"
    ".latex-fragment, .latex-environment {\n"
    "  font-family: monospace;\n"
    "  background: #f5f5f5;\n"
    "  padding: 0.2em 0.4em;\n"
    "  border-radius: 3px;\n"
    "  font-style: italic;\n"
    "}\n"
    ".footnote-ref { font-size: 0.8em; vertical-align: super; }\n"
    ".footnote-definition { font-size: 0.9em; margin: 0.5em 0 0.5em 2em; }\n"
    ".footnote-backref { text-decoration: none; margin-right: 0.3em; }\n"
    ;; htmlize syntax highlighting for e-ink (bold/italic, no colors)
    ;; Covers: emacs-lisp, c, lisp, fortran, python, cobol
    ".org-keyword { font-weight: bold; }\n"
    ".org-string { font-style: italic; }\n"
    ".org-comment { font-style: italic; }\n"
    ".org-function-name { font-weight: bold; }\n"
    ".org-type { font-style: italic; }\n"
    ".org-constant { font-weight: bold; }\n"
    ".org-builtin { font-weight: bold; font-style: italic; }\n"
    ".org-variable-name { font-style: italic; }\n"
    ".org-negation-char { font-weight: bold; }\n"
    ".org-preprocessor { font-weight: bold; }\n"
    ".org-doc-string { font-style: italic; }\n"
    ".org-decorator { font-style: italic; font-weight: bold; }\n"
    ".org-warning { font-weight: bold; text-decoration: underline; }\n"
    ".org-label { font-style: italic; }\n"
    ".org-sh-quoted-exec { font-weight: bold; font-style: italic; }\n"))

(provide 'ox-epub3-style)
;;; ox-epub3-style.el ends here
