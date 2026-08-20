;;; ox-epub3-kernel.el --- EPUB 3 core helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicola Ferru Aka NFVblog

;; This file is not part of GNU Emacs.

;;; Code:

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
(defvar org-epub3--id-map nil
  "Alist mapping (ID . CHAPTER-FILE) for internal cross-references.")

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

(provide 'ox-epub3-kernel)
;;; ox-epub3-kernel.el ends here
