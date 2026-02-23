;;; utility.window
;;;
(defpackage utility.window
  (:use :cl :utility.base)
  (:export ;; sequence
           :window-map
           :window-nmap))
(in-package :utility.window)

;;; sequence

(declaim (ftype (function ((or cons symbol class)
                           (integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-map))
(let ((t-fn (constantly t)))
  (defun window-map (result-type window-size fn sequence)
    (let ((result (let ((index 0)
                        (queue (list-queue:make-list-queue)))
                    (map result-type
                         (lambda (e)
                           (list-queue:list-queue-enqueue queue e)
                           (incf index)
                           (when (>= index window-size)
                             (prog1 (apply fn (list-queue:list-queue-raw queue))
                               (list-queue:list-queue-dequeue queue))))
                         sequence))))
      (and result
           (delete-if t-fn result :end (1- window-size))))))

(declaim (ftype (function ((integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-nmap))
(let ((t-fn (constantly t)))
  (defun window-nmap (window-size fn sequence)
    (delete-if t-fn
               (let ((index 0)
                     (queue (list-queue:make-list-queue)))
                 (utility.base::nmap
                  (lambda (e)
                    (list-queue:list-queue-enqueue queue e)
                    (incf index)
                    (when (>= index window-size)
                      (prog1 (apply fn (list-queue:list-queue-raw queue))
                        (list-queue:list-queue-dequeue queue))))
                  sequence))
               :end (1- window-size))))

;;;
