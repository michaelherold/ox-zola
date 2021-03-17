;;; ox-commonmark.el --- CommonMark Backend for the Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Michael Herold

;; Author: Michael Herold <opensource@michaeljherold.com>
;; Package-Requires: ((emacs "24.5") (org "9.1.5"))

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

;; This library implements a CommonMark (https://commonmark.org/) backend for
;; the Org exporter. In particular, it targets the Rust pulldown-cmark crate
;; (https://github.com/raphlinus/pulldown-cmark) for compatibility with the Zola
;; static site generator.
;;
;; It takes significant inspiration from Kaushal Modi's Hugo exporter
;; (https://ox-hugo.scripter.co).

;;; Code:

(require 'ffap)                                   ; For `ffap-url-regexp'
(require 'ox-md)
(require 'ox-publish)

;;; Variables

;;; User-configurable variables

(defgroup org-export-commonmark nil
  "Options for exporting Org mode files to CommonMark."
  :tag "Org Export CommonMark"
  :group 'org-export
  :version "25.2")

(defcustom org-commonmark-bullet-list-marker ?-
  "The marker for bullet list items.

May be either a hyphen, plus sign, or an asterisk, per the specification."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-center-block-class "org-center"
  "The class set on the container element for a center block."
  :group 'org-export-commonmark
  :type 'string)

(defcustom org-commonmark-center-block-style-tag t
  "Toggle whether center blocks will emit a style tag."
  :group 'org-export-commonmark
  :type 'boolean)

(defcustom org-commonmark-emphasis-indicator ?_
  "The indicator for emphasis runs.

May be either an asterisk or underscore, per the specification.

This type of element correspond to an italic Org element."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-fenced-code-block-delimeter ?`
  "The delimeter for fenced code blocks.

May be either a backtick or a tilde, per the specification."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-ordered-list-marker ?.
  "The marker for ordered list items.

May be either a period or a right parenthesis, per the specification."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-strong-emphasis-indicator ?*
  "The indicator for strong emphasis runs.

May be either an asterisk or underscore, per the specification.

This type of element correspond to a bold Org element."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-thematic-break-delimeter ?-
  "The delimeter for thematic breaks.

May be either an asterisk, hyphen, or underscore, per the specification."
  :group 'org-export-commonmark
  :type 'character)

(defcustom org-commonmark-verse-block-class "org-verse"
  "The class set on the container element for a verse block."
  :group 'org-export-commonmark
  :type 'string)

(defun org-commonmark-export-as-commonmark (&optional async subtreep visible-only)
  "Export current buffer to a CommonMark buffer.

If the current buffer is narrowed, only export the narrowed part.

If a region is active, export that region.

When optional argument ASYNC is non-nil, the export happens asynchronously. The
resulting buffer should be accessible through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the subtree at point, first
extracting information from the headline properties.

When optional argument VISIBLE-ONLY is non-nil, skips the contents of hidden
elements.

Exports to a buffer names \"*Org CommonMark Export*\", which will be displayed
when `org-export-show-temporary-export-buffer' is non-nil."
  (interactive)

  (org-export-to-buffer 'commonmark "*Org CommonMark Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-commonmark--bold (_bold contents _info)
  "Transcode BOLD element into CommonMark strong emphasis format.
CONTENTS is the text within bold markup."
  (let* ((delimeter (make-string 2 org-commonmark-strong-emphasis-indicator)))
    (format "%s%s%s" delimeter contents delimeter)))

(defun org-commonmark--center-block (center-block contents _info)
  "Transcode CENTER-BLOCK element into a centered paragraph.
CONTENTS is the text within the comment."
  (let ((class org-commonmark-center-block-class))
    (concat (when org-commonmark-center-block-style-tag
              (format "<style>.%s { margin-left: auto; margin-right: auto; text-align: center; }</style>\n" class))
            (format "<div class=\"%s\">\n%s</div>\n" class contents))))

(defun org-commonmark--fenced-code-block (content language)
  "Wrap CONTENT in a fenced code block in a given LANGUAGE.

See https://spec.commonmark.org/0.29/#fenced-code-blocks for the specification."

  (let* ((backticks (make-string 3 org-commonmark-fenced-code-block-delimeter)))
    (format "%s%s\n%s%s" backticks language content backticks)))

(defun org-commonmark--example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element into CommonMark fenced code block format.
INFO is a property list holding contextual information."

  (let* ((example (org-export-format-code-default example-block info)))
    (org-commonmark--fenced-code-block example "txt")))

(defalias 'org-commonmark--fixed-width #'org-commonmark--example-block
  "Transcode a FIXED-WIDTH element into a CommonMark fenced code block.")

(defun org-commonmark--get-anchor (element info)
  "Return an Org ELEMENT's anchor tag name.

INFO is a property list used as a communication channel.

When the ELEMENT has a CUSTOM_ID property, use that as the anchor name.
Otherwise, derive the title string from the INFO and slugify it."
  (let ((ret (org-element-property :CUSTOM_ID element)))
    (unless ret
      (let ((title (org-export-data-with-backend (org-export-get-alt-title element info) 'md info)))
        (setq ret (org-commonmark--slug title))))
    ret))

(defun org-commonmark--headline-meta (headline info)
  "Extract title and anchor information from a HEADLINE element.

INFO is a property list used as a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info))
        (anchor (org-commonmark--get-anchor headline info)))
    (list 'title title 'anchor anchor)))

(defun org-commonmark--horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a HORIZONTAL-RULE element into CommonMark thematic break format.

Uses the custom setting `org-commonmark-thematic-break-delimeter' as the
character for the thematic break."
  (make-string 3 org-commonmark-thematic-break-delimeter))

(defun org-commonmark--inner-template (contents info)
  "Return the body of document string after CommonMark conversion.
CONTENTS is the transcoded contents string.  INFO is a property list
holding export options."
  (let* ((depth (plist-get info :with-toc))
         (headlines (and depth (org-export-collect-headlines info depth)))
         (toc-tail (if headlines "\n\n" ""))
         (toc-string ""))

    (when headlines
      (setq toc-string (format "# %s\n" (org-export-translate "Table of Contents" :utf-8 info)))
      (dolist (headline headlines)
        (setq toc-string
              (concat toc-string (org-commonmark--toc-entry headline info) "\n"))))
    (org-trim (concat toc-string toc-tail contents))))

(defun org-commonmark--italic (_italic contents _info)
  "Transcode ITALIC element into CommonMark emphasis format.
CONTENTS is the text within italic markup."
  (let* ((delimeter (make-string 1 org-commonmark-emphasis-indicator)))
    (format "%s%s%s" delimeter contents delimeter)))

(defun org-commonmark--item (item contents info)
  "Transcode ITEM element into CommonMark list item format.
CONTENTS is the item contents. INFO is a property list used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (marker (if (not (eq type 'ordered)) (make-string 1 org-commonmark-bullet-list-marker)
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           (make-string 1 org-commonmark-ordered-list-marker)))))
    (concat marker
            (make-string (- 4 (length marker)) ? )
            (let ((tag (org-element-property :tag item))
                  (delimeter (make-string 2 org-commonmark-strong-emphasis-indicator)))
              (and tag (format "%s%s:%s " delimeter (org-export-data tag info) delimeter)))
            (and contents
                 (org-trim (replace-regexp-in-string "^" "    " contents))))))

(defun org-commonmark--line-break (_line-break _contents info)
  "Transcode a LINE-BREAK element into a CommonMark hard line break.
INFO is a property list holding contextual information."
  "\\\n")

(defun org-commonmark--slug (str)
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
  (advice-add 'org-commonmark--slug :before
              (lambda (str)
                (let* ((multibyte-punctuations-str "：")
                       (str (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str)))
                  str))))

(defun org-commonmark--src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element into a CommonMark fenced code block.
INFO is a property list holding contextual information."

  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info)))
    (org-commonmark--fenced-code-block code lang)))

(defun org-commonmark--toc-entry (headline info)
  "Format a HEADLINE as an entry in the Table of Contents.

INFO is a property list used as a communication channel."
  (let* ((meta (org-commonmark--headline-meta headline info))
         (title (plist-get meta 'title))
         (anchor (plist-get meta 'anchor))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (bullet (make-string 1 org-commonmark-bullet-list-marker)))
    (concat indent bullet " [" title "]" "(#" anchor ")")))

(defun org-commonmark--verse-block (_verse-block contents info)
  "Transcode a VERSE-BLOCK element into a CommonMark paragraph.
CONTENTS is the contents of the verse block.  INFO is a property list holding
contextual information.

Wraps the verse in a paragraph with the CSS class specified in
`org-commonmark-verse-block-class', when set."
  (let* ((ret contents)
         (br (org-html-close-tag "br" nil info))
         (quoted-br (regexp-quote br))
         (ret (replace-regexp-in-string
               (format "\\(?:%s\\)?[ \t]*\n" quoted-br)
               (concat br "\n")
               ret))
         (ret (replace-regexp-in-string
               "^[[:blank:]]+"
               (lambda (match)
                 (let (out) (dotimes (_ (length match) out) (setq out (concat "&nbsp;" out)))))
               ret))
         (ret (replace-regexp-in-string
               (format "%s\n\\'" quoted-br)
               "\n"
               ret))
         (ret (concat
               "<p"
               (when org-commonmark-verse-block-class
                 (format " class=\"%s\"" org-commonmark-verse-block-class))
               (format ">\n%s</p>" ret))))
    ret))

(org-export-define-derived-backend 'commonmark 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  ;; :menu-entry
  ;; '(?C "Export to CommonMark"
  ;;      ((?C "To temporary buffer"
  ;;           (lambda (async subtree visible-only _body-only)
  ;;             (org-commonmark-export-as-commonmark async subtree visible-only)))))
  :translate-alist '((bold . org-commonmark--bold)
                     (center-block . org-commonmark--center-block)
                     (example-block . org-commonmark--example-block)
                     (fixed-width . org-commonmark--fixed-width)
                     (horizontal-rule . org-commonmark--horizontal-rule)
                     (inner-template . org-commonmark--inner-template)
                     (italic . org-commonmark--italic)
                     (item . org-commonmark--item)
                     (line-break . org-commonmark--line-break)
                     (src-block . org-commonmark--src-block)
                     (verse-block . org-commonmark--verse-block)))

(provide 'ox-commonmark)
;;; ox-commonmark.el ends here
