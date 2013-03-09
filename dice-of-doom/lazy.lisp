(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

;; 遅延リストライブラリ
(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
              (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;; 遅延リストに対するマッピングと検索
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
              (if (lazy-null lst-cur)
                (force (lazy-mapcan fun (lazy-cdr lst)))
                (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
        x
        (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
    (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))

;(print (lazy-car *integers*))
;(print (lazy-car (lazy-cdr *integers*)))
;(print (lazy-car (lazy-cdr (lazy-cdr *integers*))))
;
;(print (take 10 *integers*))
;(print (take 10 (make-lazy '(q w e r t y u i o p a s d f))))
;(print (take-all (make-lazy '(q w e r t y u i o p a s d f))))
;
;(print (take 10 (lazy-mapcar #'sqrt *integers*)))
;
;(print (take 10 (lazy-mapcan (lambda (x)
;                        (if (evenp x)
;                          (make-lazy (list x))
;                          (lazy-nil)))
;                      *integers*)))
;
;(print (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10))))
;
;(print (lazy-nth 4 (make-lazy '(a b c d e f g))))

