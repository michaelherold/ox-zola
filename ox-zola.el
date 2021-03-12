;;; ox-zola.el --- This is the description           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Michael Herold

;; Author: Michael Herold <opensource@michaeljherold.com>
;; Package-Requires: ((emacs "24.4") (org "9.0"))
;; Keywords: Org, commonmark, tools

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'ffap)                                   ; For `ffap-url-regexp'
(require 'ox-commonmark)

(defun org-zola-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola-flavored Markdown buffer.

If the current buffer is narrowed, only export the narrowed part.

If a region is active, export that region.

When optional argument ASYNC is non-nil, the export happens asynchronously. The
resulting buffer should be accessible through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the subtree at point, first
extracting information from the headline properties.

When optional argument VISIBLE-ONLY is non-nil, skips the contents of hidden
elements.

Exports to a buffer names \"*Org Zola Export*\", which will be displayed
when `org-export-show-temporary-export-buffer' is non-nil."
  (interactive)

  (org-export-to-buffer 'zola "*Org Zola Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-zola--get-anchor (element info)
  "Return an Org ELEMENT's anchor tag name.

INFO is a property list used as a communication channel.

When the ELEMENT has a CUSTOM_ID property, use that as the anchor name.
Otherwise, derive the title string from the INFO and slugify it."
  (let ((ret (org-element-property :CUSTOM_ID element)))
    (unless ret
      (let ((title (org-export-data-with-backend (org-element-property :title element) 'md info)))
        (setq ret (org-zola--slug title))))
    ret))

(defun org-zola--headline (headline contents info)
  "Transcode HEADLINE element into Zola-flavored Markdown format.

CONTENTS is the headline contents. INFO is a property list used as a
communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level-offset (string-to-number (plist-get info :zola-level-offset)))
           (level (org-export-get-relative-level headline info))
           (level-effective (+ level-offset level))
           (title (org-export-data (org-element-property :title headline) info))
           (anchor (format "{#%s}" (org-zola--get-anchor headline info)))
           (headline (concat title " " anchor "\n"))
           (headline-title (concat "\n" (make-string level-effective ?#) " " headline "\n"))
           (content-str (or (org-string-nw-p contents) "")))
      (format "%s%s" headline-title content-str))))

(defun org-zola--slug (str)
  "Convert string STR to a slug and return it."

  (let* ((str (downcase str))
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         (str (replace-regexp-in-string "&" " and " str))
         (str (replace-regexp-in-string "\\." " dot " str))
         (str (replace-regexp-in-string "\\+" " plus " str))
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         (str (replace-regexp-in-string "[()]" "" str))
         (str (replace-regexp-in-string " " "-" str))
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

(when (version< emacs-version "25.0")
  (advice-add 'org-zola--slug :before
              (lambda (str)
                (let* ((multibyte-punctuations-str "：")
                       (str (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str)))
                  str))))

(org-export-define-derived-backend 'zola 'commonmark
  :menu-entry
  '(?Z "Export to Zola-compatible Markdown"
       ((?t "File to temporary buffer"
            (lambda (async subtree visible-only _body-only)
              (org-zola-export-to-md async subtree visible-only)))))
  :translate-alist '((headline . org-zola--headline))
  :options-alist '((:zola-level-offset "ZOLA_LEVEL_OFFSET" nil "0")))

(provide 'ox-zola)
;;; ox-zola.el ends here
