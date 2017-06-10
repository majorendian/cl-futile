(defpackage :futile
  (:use :common-lisp)
  (:export curry rcurry xcurry rxcurry
           compose rcompose 
           nilcompose list-nilcompose
           read-write))
(in-package :futile)

(declaim (ftype (function (&rest function) t) compose nilcompose list-nilcompose))
(declaim (ftype (function (function &rest function) t) xcurry rxcurry))

;TODO:
;make-binary-recusrion-tree


(defmacro curry (f &rest r)
  `(lambda (x) (funcall ,f ,@r x)))

(defmacro rcurry (f &rest r)
  `(lambda (x) (funcall ,f x ,@r)))

(defun xcurry (f &rest r)
  (declare (function f) (list r))
  (lambda (&rest a)
    (apply f (concatenate 'list r a))))

(defun rxcurry (f &rest r)
  (declare (function f) (list r))
  (lambda (&rest a)
    (apply f (concatenate 'list a r))))

(defun compose (&rest flist)
  (unless (and (typep flist 'list)
               (every 'functionp flist))
    (error 'type-error :text "All arguments must be functions"))

  (lambda (x)
    (labels ((tmp (fl arg)
               (if fl
                 (let ((r (funcall (first fl) arg)))
                   (tmp (rest fl) r))
                 arg)))
      (tmp flist x))))

(defun nilcompose (&rest flist)
  (lambda ()
    (labels ((tmp (fl)
               (if fl
                 (progn
                   (funcall (first fl))
                   (tmp (rest fl))))))
      (tmp flist))))

(defun list-nilcompose (&rest flist)
  (lambda ()
    (labels ((tmp (fl)
               (if fl
                 (let ((r (funcall (first fl))))
                   (cons r (tmp (rest fl)))))))
      (tmp flist))))

(defmacro rcompose (&rest flist)
  `(compose ,@(reverse flist)))

(declaim (ftype (function (fixnum fixnum (or null sb-alien-internals:alien-value) fixnum &optional function) t) read-write))
(defun read-write (from_fd to_fd alien_char_buffer count &optional (lambda_fun (lambda (alienbuff bytes_read) 
                                                                                           (cons alienbuff bytes_read))))
"Read data from FROM_FD and write them to TO_FD.
ALIEN_CHAR_BUFFER is a buffer of type (sb-alien:array char COUNT).
If COUNT is bigger than the character array, you're in trouble.

LAMBDA_FUN is a function that recieves the ALIEN_CHAR_BUFFER as it's argument
and the number of bytes that have been read into it as it's second argument.
It _MUST_ return a CONS cell whos first value is (sb-alien:array char bytes_read)
and the second value the number of bytes to write (normaly it should equal the number of bytes read).

SIDE EFFECTS: 
Destructively modifies ALIEN_CHAR_BUFFER."
  (let ((bytes_read (sb-posix:read from_fd alien_char_buffer count)))
    (if (> bytes_read 0)
      (let ((lambda_val (funcall lambda_fun alien_char_buffer bytes_read)))
        (sb-posix:write to_fd (car lambda_val) (cdr lambda_val))
        (if (< bytes_read count)
          (return-from read-write t)
          (read-write from_fd to_fd alien_char_buffer count lambda_fun)))
      t)))

;Calls 2 functions recursively with their arguments and their result.
(defun swap-funcall (f1 f2 args)
  (let ((result (funcall f1 args)))
    (cond 
      ((= -1 result) nil)
      ((null result) (swap-funcall f2 f1 `(,@args ,result)))
      (result (swap-funcall f2 f1 `(,@args ,result)))
      )))


