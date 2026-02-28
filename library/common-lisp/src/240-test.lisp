;;; test
;;;
(defpackage :atcoder.test
  (:use :cl :utility)
  (:export :*atcoder-session-cookie*
           :test
           :test-case
           :test-url
           :fetch-atcoder-samples))
(in-package :atcoder.test)

(defparameter *atcoder-session-cookie* nil
  "AtCoder session cookie string for authenticated fetch.
Either full cookie pair (e.g. \"REVEL_SESSION=...\") or just cookie value.")

(defun normalize-whitespaces (string)
  (with-output-to-string (out)
    (loop with pending-space = nil
          and wrote-char-p = nil
          for ch across string
          do (if (find ch '(#\Space #\Tab #\Newline #\Return))
                 (setf pending-space t)
                 (progn
                   (when (and pending-space wrote-char-p)
                     (write-char #\Space out))
                   (write-char ch out)
                   (setf wrote-char-p t)
                   (setf pending-space nil))))))

(defun test-case (fn input expect)
  (let ((output (with-output-to-string (*standard-output*)
                  (with-input-from-string (*standard-input* input)
                    (funcall fn)))))
    (if (string= (normalize-whitespaces output)
                 (normalize-whitespaces expect))
        (format t "Pass~%")
        (format t "Failed~%expect: ~A~%but actual: ~A~%" expect output))))

(defun atcoder-html-unescape (string)
  (labels ((replace-all (s from to)
             (with-output-to-string (out)
               (loop with start = 0
                     and from-len = (length from)
                     for pos = (search from s :start2 start)
                     if pos
                       do (write-string s out :start start :end pos)
                          (write-string to out)
                          (setf start (+ pos from-len))
                     else
                       do (write-string s out :start start)
                          (return)))))
    (let ((out string))
      (setf out (replace-all out "&lt;" "<"))
      (setf out (replace-all out "&gt;" ">"))
      (setf out (replace-all out "&amp;" "&"))
      (setf out (replace-all out "&quot;" "\""))
      (setf out (replace-all out "&#39;" "'"))
      out)))

(defun atcoder-strip-tags (string)
  (with-output-to-string (out)
    (loop with in-tag = nil
          for ch across string
          do (cond ((char= ch #\<) (setf in-tag t))
                   ((char= ch #\>) (setf in-tag nil))
                   ((not in-tag) (write-char ch out))))))

(defun atcoder-clean-pre-text (text)
  (let* ((lines (split-string text :separator '(#\Newline)))
         (filtered (loop for line in lines
                         unless (string-prefix-p "```" (trim-spaces line))
                           collect line)))
    (trim-spaces (atcoder-html-unescape (strjoin filtered)))))

(defun detect-sample-kind (heading)
  (let ((h (string-downcase (trim-spaces heading))))
    (cond ((or (search "入力例" h)
               (search "sample input" h))
           :input)
          ((or (search "出力例" h)
               (search "sample output" h))
           :output)
          (t nil))))

(defun parse-atcoder-samples-from-html (html)
  (let ((pos 0)
        (pending-input nil)
        (cases nil))
    (loop
      for h3-open = (search "<h3" html :start2 pos)
      while h3-open
      do (let* ((h3-tag-end (search ">" html :start2 h3-open))
                (h3-close (and h3-tag-end (search "</h3>" html :start2 (1+ h3-tag-end)))))
           (if (and h3-tag-end h3-close)
               (let* ((heading-raw (subseq html (1+ h3-tag-end) h3-close))
                      (kind (detect-sample-kind (atcoder-strip-tags heading-raw)))
                      (pre-open (search "<pre" html :start2 h3-close))
                      (pre-tag-end (and pre-open (search ">" html :start2 pre-open)))
                      (pre-close (and pre-tag-end (search "</pre>" html :start2 (1+ pre-tag-end)))))
                 (if (and kind pre-open pre-tag-end pre-close)
                     (let ((text (atcoder-clean-pre-text (subseq html (1+ pre-tag-end) pre-close))))
                       (cond ((eq kind :input)
                              (setf pending-input text))
                             ((and (eq kind :output) pending-input)
                              (push (list pending-input text) cases)
                              (setf pending-input nil)))
                       (setf pos (+ pre-close (length "</pre>"))))
                     (setf pos (+ h3-close (length "</h3>")))))
               (setf pos (1+ h3-open)))))
    (nreverse cases)))

(defun normalize-atcoder-cookie (cookie)
  (cond ((null cookie) nil)
        ((search "=" cookie) cookie)
        (t (format nil "REVEL_SESSION=~A" cookie))))

(defun ensure-load-dexador ()
  (unless (find-package :dexador)
    (when (find-package :ql)
      (funcall (intern (string :quickload) :ql) :dexador :silent t)))
  (unless (find-package :dexador)
    (error "Dexador package is not loaded. Load dexador before calling fetch-atcoder-samples.")))

(defun resolve-dexador-get-function ()
  "Resolve and return the function object of DEXADOR:GET lazily.

This indirection intentionally avoids writing DEXADOR:GET directly at read/load
time, so this file can still be loaded in environments where the DEXADOR package
is not present yet. We first ensure dexador is loaded, then resolve GET from the
DEXADOR package at runtime and return its function object."
  (ensure-load-dexador)
  (let ((symbol (find-symbol (string :get) :dexador)))
    (unless (and symbol (fboundp symbol))
      (error "DEXADOR:GET function is not available."))
    (symbol-function symbol)))

(defun fetch-url (url &key cookie)
  (let ((cookie-header (normalize-atcoder-cookie cookie))
        (dexador-get (resolve-dexador-get-function)))
    (if cookie-header
        (funcall dexador-get
                 url
                 :headers `(("Cookie" . ,cookie-header)))
        (funcall dexador-get url))))

(defun fetch-atcoder-samples (url &key cookie)
  "Fetch sample input/output pairs from an AtCoder task URL.
Returns a list of (input output)."
  (let* ((html (fetch-url url :cookie (or cookie *atcoder-session-cookie*)))
         (cases (parse-atcoder-samples-from-html html)))
    #-atcoder (when (null cases)
      (error "No samples found in URL: ~A" url))
    cases))

(defun test-url (fn url &key cookie)
  "Run test-case for all samples fetched from AtCoder task URL."
  (let ((cases (fetch-atcoder-samples url :cookie cookie)))
    (loop for (input expect) in cases
          for index from 1
          do (format t "[Sample ~D]~%" index)
             (test-case fn input expect))
    (length cases)))

(defun test (fn url &key cookie)
  (test-url fn url :cookie cookie))

;;;
