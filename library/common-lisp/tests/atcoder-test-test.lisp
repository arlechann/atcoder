(in-package :cl-user)

(defpackage :atcoder-test-test
  (:use :cl :rove))
(in-package :atcoder-test-test)

(defmacro with-mocked-function ((name fn) &body body)
  `(let ((old (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name) ,fn)
            ,@body)
       (setf (symbol-function ',name) old))))

(deftest atcoder-test-normalize-whitespaces
  (ok (equal "a b c"
             (atcoder.test::normalize-whitespaces
              (format nil " ~% a~C b ~C~% c  " #\Tab #\Return))))
  (ok (equal ""
             (atcoder.test::normalize-whitespaces
              (format nil " ~%~C~C " #\Tab #\Return)))))

(deftest atcoder-test-case-output
  (let ((pass-out (with-output-to-string (*standard-output*)
                    (atcoder.test:test-case (lambda () (format t "1  2~%")) "" "1 2")))
        (fail-out (with-output-to-string (*standard-output*)
                    (atcoder.test:test-case (lambda () (format t "3")) "" "4"))))
    (ok (search "Pass" pass-out))
    (ok (search "Failed" fail-out))
    (ok (search "expect:" fail-out))))

(deftest atcoder-test-parse-samples
  (let* ((html
           (concatenate
            'string
            (format nil "<h3>入力例 1</h3><pre>1 2~A~A3 4~A~A</pre>" #\CR #\LF #\CR #\LF)
            "<h3>出力例 1</h3><pre>5</pre>"
            "<h3>Sample Input 2</h3><pre>a&lt;b&amp;c</pre>"
            "<h3>Sample Output 2</h3><pre>ok&quot;yes&#39;</pre>"))
         (cases (atcoder.test::parse-atcoder-samples-from-html html)))
    (ok (equal (list (list (format nil "1 2~A3 4" #\NewLine) "5")
                     '("a<b&c" "ok\"yes'"))
               cases))))

(deftest atcoder-test-fetch-url-mocked
  (let ((called nil)
        (result ""))
    (with-mocked-function
        (atcoder.test::resolve-dexador-get-function
         (lambda ()
           (lambda (url &key headers)
             (setf called (list url headers))
             result)))
      (setf result "plain")
      (ok (equal "plain" (atcoder.test::fetch-url "http://example.com")))
      (ok (equal '("http://example.com" nil) called))

      (setf result "cookie")
      (ok (equal "cookie"
                 (atcoder.test::fetch-url "http://example.com" :cookie "abc")))
      (ok (equal "http://example.com" (first called)))
      (ok (equal '(("Cookie" . "REVEL_SESSION=abc")) (second called)))

      (setf result "cookie2")
      (ok (equal "cookie2"
                 (atcoder.test::fetch-url "http://example.com"
                                          :cookie "REVEL_SESSION=xyz")))
      (ok (equal '(("Cookie" . "REVEL_SESSION=xyz")) (second called))))))

(deftest atcoder-test-fetch-atcoder-samples-uses-global-cookie
  (let ((html "<h3>入力例 1</h3><pre>2 3</pre><h3>出力例 1</h3><pre>5</pre>")
        (captured-cookie nil)
        (old-cookie atcoder.test:*atcoder-session-cookie*))
    (unwind-protect
         (progn
           (setf atcoder.test:*atcoder-session-cookie* "token")
           (with-mocked-function
               (atcoder.test::fetch-url
                (lambda (_url &key cookie)
                  (declare (ignore _url))
                  (setf captured-cookie cookie)
                  html))
             (ok (equal '(("2 3" "5"))
                        (atcoder.test:fetch-atcoder-samples "dummy-url")))
             (ok (equal "token" captured-cookie))))
      (setf atcoder.test:*atcoder-session-cookie* old-cookie))))

(deftest atcoder-test-test-url-mocked
  (let ((printed (with-output-to-string (*standard-output*)
                   (with-mocked-function
                       (atcoder.test:fetch-atcoder-samples
                        (lambda (_url &key cookie)
                          (declare (ignore _url cookie))
                          '(("1 2" "3")
                            ("10 20" "30"))))
                     (ok (= 2 (atcoder.test:test-url
                               (lambda ()
                                 (let ((a (read)) (b (read)))
                                   (format t "~A~%" (+ a b))))
                               "dummy")))))))
    (ok (search "[Sample 1]" printed))
    (ok (search "[Sample 2]" printed))
    (ok (>= (count #\P printed) 2))))
