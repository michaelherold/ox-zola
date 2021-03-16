;;; ox-commonmark.el --- CommonMark Backend for the Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Michael Herold

;; Author: Michael Herold <opensource@michaeljherold.com>
;; Package-Requires: ((emacs "24.5"))

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

(defun org-commonmark--horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a HORIZONTAL-RULE element into CommonMark thematic break format.

Uses the custom setting `org-commonmark-thematic-break-delimeter' as the
character for the thematic break."
  (make-string 3 org-commonmark-thematic-break-delimeter))

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

(defun org-commonmark--src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element into a CommonMark fenced code block.
INFO is a property list holding contextual information."

  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info)))
    (org-commonmark--fenced-code-block code lang)))

(org-export-define-derived-backend 'commonmark 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  ;; :menu-entry
  ;; '(?C "Export to CommonMark"
  ;;      ((?C "To temporary buffer"
  ;;           (lambda (async subtree visible-only _body-only)
  ;;             (org-commonmark-export-as-commonmark async subtree visible-only)))))
  :translate-alist '((bold . org-commonmark--bold)
                     (example-block . org-commonmark--example-block)
                     (fixed-width . org-commonmark--fixed-width)
                     (horizontal-rule . org-commonmark--horizontal-rule)
                     (italic . org-commonmark--italic)
                     (item . org-commonmark--item)
                     (src-block . org-commonmark--src-block)))

(provide 'ox-commonmark)
;;; ox-commonmark.el ends here
