;;; ox-commonmark-test.el --- Tests for ox-commonmark

;;; Commentary:

;; These tests test various features of `ox-commonmark' for properly exporting
;; different Org mode elements.

;;; Code:


(ert-deftest bold-test ()
  (should (string-match-p (regexp-quote "**bold**")
                          (ox-commonmark-tests--render-content
                           "*bold*")))
  (should (string-match-p (regexp-quote "__bold__")
                          (let ((org-commonmark-strong-emphasis-indicator ?_))
                            (ox-commonmark-tests--render-content
                             "*bold*")))))

(ert-deftest center-block-test ()
  (should (string-match-p (regexp-quote "<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>
<div class=\"org-center\">
Lorem ipsum dolor sit amet
</div>")
                          (ox-commonmark-tests--render-content "
#+begin_center
Lorem ipsum dolor sit amet
#+end_center")))
  (should (not (string-match-p (regexp-quote "<style>")
                               (let ((org-commonmark-center-block-style-tag nil))
                                 (ox-commonmark-tests--render-content "
#+begin_center
This will not have a style tag.
#+end_center")))))
  (should (string-match-p (regexp-quote "<div class=\"my-center\">")
                          (let ((org-commonmark-center-block-class "my-center"))
                            (ox-commonmark-tests--render-content "
#+begin_center
This will have a custom div class.
#+end_center")))))

(ert-deftest example-block-test ()
  (should (string-match-p (regexp-quote "```txt
This is plaintext output.
```")
                          (ox-commonmark-tests--render-content
                           "#+begin_example
This is plaintext output.
#+end_example
"))))

(ert-deftest fixed-width-test ()
  (should (string-match-p (regexp-quote "```txt
This is fixed-width
content
across multiple lines.
```")
                          (ox-commonmark-tests--render-content
                           ": This is fixed-width
: content
: across multiple lines.
"))))

(ert-deftest horizontal-rule-test ()
  (should (string-match-p (regexp-quote "---")
                          (ox-commonmark-tests--render-content
                           "-----")))
  (should (string-match-p (regexp-quote "***")
                          (let ((org-commonmark-thematic-break-delimeter ?*))
                            (ox-commonmark-tests--render-content
                             "-----"))))
  (should (string-match-p (regexp-quote "___")
                          (let ((org-commonmark-thematic-break-delimeter ?_))
                            (ox-commonmark-tests--render-content
                             "-----")))))

(ert-deftest italic-test ()
  (should (string-match-p (regexp-quote "_emphasis_")
                          (ox-commonmark-tests--render-content
                           "/emphasis/")))
  (should (string-match-p (regexp-quote "*emphasis*")
                          (let ((org-commonmark-emphasis-indicator ?*))
                            (ox-commonmark-tests--render-content
                             "/emphasis/")))))

(ert-deftest item-test ()
  (should (string-match-p (regexp-quote "-   First
-   Second
-   Third")
          (ox-commonmark-tests--render-content "- First
- Second
- Third")))
  (should (string-match-p (regexp-quote "+   First
+   Second
+   Third")
          (let ((org-commonmark-bullet-list-marker ?+))
            (ox-commonmark-tests--render-content "- First
- Second
- Third"))))
  (should (string-match-p (regexp-quote "*   First
*   Second
*   Third")
          (let ((org-commonmark-bullet-list-marker ?*))
            (ox-commonmark-tests--render-content "- First
- Second
- Third"))))
  (should (string-match-p (regexp-quote "1.  First
2.  Second
3.  Third")
                          (ox-commonmark-tests--render-content "1. First
2. Second
3. Third")))
  (should (string-match-p (regexp-quote "1)  First
2)  Second
3)  Third")
          (let ((org-commonmark-ordered-list-marker ?\)))
            (ox-commonmark-tests--render-content "1. First
2. Second
3. Third"))))
  (should (string-match-p (regexp-quote "-   An item
    -   Nested item
    -   Second nested
-   Unnested item")
          (let ((ox-commonmark-bullet-list-marker ?+))
            (ox-commonmark-tests--render-content "- An item
  - Nested item
  - Second nested
- Unnested item"))))
  (should (string-match-p (regexp-quote "-   **Term:** Definition")
                          (ox-commonmark-tests--render-content "- Term :: Definition")))
  (should (string-match-p (regexp-quote "+   __Term:__ Definition")
                          (let ((org-commonmark-bullet-list-marker ?+)
                                (org-commonmark-strong-emphasis-indicator ?_))
                            (ox-commonmark-tests--render-content "- Term :: Definition")))))

(ert-deftest line-break-test ()
  (should (string-match-p (regexp-quote "This is a \\
test of a line break")
                          (ox-commonmark-tests--render-content "
This is a \\\\
test of a line break"))))

(ert-deftest slug-test ()
  (should (equal "somebody-set-up-us-the-bomb" (org-commonmark--slug "Somebody Set Up Us The Bomb!")))
  (should (equal "turner-and-hooch" (org-commonmark--slug "Turner & Hooch")))
  (should (equal "at-and-t" (org-commonmark--slug "AT&T")))
  (should (equal "me-myself-and-irene" (org-commonmark--slug "Me, Myself, and Irene")))
  (should (equal "alien-resurrection" (org-commonmark--slug "Alien: Resurrection")))
  (should (equal "alien-resurrection" (org-commonmark--slug "Alien： Resurrection"))) ; Emacs < 25.0
  (should (equal "left-side-right-side" (org-commonmark--slug " Left Side Right Side ")))
  (should (equal "500--days-of-summer" (org-commonmark--slug "(500) Days of Summer")))
  )

(ert-deftest src-block-test ()
  (should (string-match-p (regexp-quote "```ruby
def foo
  'bar'
end
```")
                          (ox-commonmark-tests--render-content
                           "#+begin_src ruby
def foo
  'bar'
end
#+end_src
")))
  (should (string-match-p (regexp-quote "~~~ruby
def foo
  'bar'
end
~~~")
                          (let ((org-commonmark-fenced-code-block-delimeter ?~))
                            (ox-commonmark-tests--render-content
                             "#+begin_src ruby
def foo
  'bar'
end
#+end_src
")))))

;;; ox-commonmark-test.el ends here
