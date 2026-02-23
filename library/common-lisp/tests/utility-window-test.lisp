(in-package :cl-user)

(defpackage :utility-window-test
  (:use :cl :rove))
(in-package :utility-window-test)

(deftest window-map-basic
  (ok (equal '(3 5 7)
             (utility.window:window-map 'list 2 #'+ '(1 2 3 4))))
  (ok (equal '(6 24)
             (utility.window:window-map 'list 3 #'* '(1 2 3 4))))
  (ok (equal '(1 2 3)
             (utility.window:window-map 'list 1 #'identity '(1 2 3)))))

(deftest window-map-boundary
  (ok (handler-case (progn (utility.window:window-map 'list 5 #'+ '(1 2 3))
                           nil)
        (error () t)))
  (ok (handler-case (progn (utility.window:window-map 'list 4 #'+ '(1 2 3))
                           nil)
        (error () t)))
  (ok (handler-case (progn (utility.window:window-map 'list 2 #'+ nil)
                           nil)
        (error () t)))
  (ok (equal '(6)
             (utility.window:window-map 'list 3 #'+ '(1 2 3)))))

(deftest window-nmap-vector
  (let ((v (vector 1 2 3 4)))
    (ok (equal '(3 5 7)
               (coerce (utility.window:window-nmap 2 #'+ v) 'list)))
    (ok (equal '(3 5 7 7) (coerce v 'list))))
  (let ((v (vector 1 2 3)))
    (ok (handler-case (progn (utility.window:window-nmap 4 #'+ v)
                             nil)
          (error () t)))))

(deftest window-nmap-list
  (let ((lst (list 1 2 3 4)))
    (ok (equal '(3 5 7)
               (utility.window:window-nmap 2 #'+ lst)))
    (ok (equal '(nil 3 5 7) lst)))
  (let ((lst (list 1 2 3)))
    (ok (handler-case (progn (utility.window:window-nmap 4 #'+ lst)
                             nil)
          (error () t)))))
