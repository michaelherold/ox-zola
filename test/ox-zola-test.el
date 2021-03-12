;;; ox-zola-test.el --- Tests for ox-zola            -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests test exporting features for `ox-zola'.

;;; Code:

(ert-deftest headline-test ()
  (should (string-match-p "^## Test Headline {#test-headline}"
                          (ox-zola-tests--render-content "
* Test Article
:PROPERTIES:
:EXPORT_FILE_NAME: test-article
:END:

** Test Headline"))))

(ert-deftest slug-test ()
  (should (equal "somebody-set-up-us-the-bomb" (org-zola--slug "Somebody Set Up Us The Bomb!")))
  (should (equal "turner-and-hooch" (org-zola--slug "Turner & Hooch")))
  (should (equal "at-and-t" (org-zola--slug "AT&T")))
  (should (equal "me-myself-and-irene" (org-zola--slug "Me, Myself, and Irene")))
  (should (equal "alien-resurrection" (org-zola--slug "Alien: Resurrection")))
  (should (equal "alien-resurrection" (org-zola--slug "Alien： Resurrection"))) ; Emacs < 25.0
  (should (equal "left-side-right-side" (org-zola--slug " Left Side Right Side ")))
  (should (equal "500--days-of-summer" (org-zola--slug "(500) Days of Summer")))
  )

;;; ox-zola-test.el ends here
