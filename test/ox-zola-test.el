;;; ox-zola-test.el --- Tests for ox-zola            -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests test exporting features for `ox-zola'.

;;; Code:

(ert-deftest headline-test ()
  (should (string-match-p "^## Test Headline {#test-headline}"
                          (ox-zola-tests--render-content "* Test Article
:PROPERTIES:
:EXPORT_FILE_NAME: test-article
:END:

** Test Headline"))))

;;; ox-zola-test.el ends here
