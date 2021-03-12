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
