;;; test-helper.el --- Helpers for ox-zola tests -*- lexical-binding: t -*-

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
;;; Code:

(require 'undercover)

(setq undercover-force-coverage t)

(undercover "*.el"
            (:exclude "*-test.el")
            (:report-format 'simplecov)
            (:report-file nil))

(require 'ox-commonmark)

(defmacro ox-zola-tests--with-temp-buffer (text body-test)
  "Create a temporary `org-mode' buffer to test.

TEXT is the text to place in the buffer. BODY-TEST is an assertion to run
against the TEXT."
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line -1)
     ,body-test))

(defmacro ox-commonmark-tests--render-content (text)
  "Render content and return the rendered version.

TEXT is the text to place in a temporary `org-mode' buffer. BODY-TEST is an
assertion to run against the TEXT. BUFFER-NAME is the name of the buffer to read
from."

  `(ox-zola-tests--with-temp-buffer
    ,text
    (progn
      (org-commonmark-export-as-commonmark)
      (save-excursion
        (set-buffer "*Org CommonMark Export*")
        (buffer-string)))))

;;; test-helper.el ends here
