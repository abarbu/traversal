(module traversal *
(import chicken scheme srfi-1 vector-lib)
(require-extension srfi-1 vector-lib)

;;; Compatibility with QobiScheme
;; map-reduce2 and map-reduce3 also changed

(define (qfind x l) (finde x l))
(define (qcount x l) (counte x l))
(define (qposition x l) (positione x l))
(define (qremove x l) (removee x l))

(define (qreduce f l i)
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
              (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (qreduce-vector f v i)
 (let ((n (vector-length v)))
  (cond ((zero? n) i)
	((= n 1) (vector-ref v 0))
	(else (let loop ((i 1) (c (vector-ref v 0)))
	       (if (= i n) c (loop (+ i 1) (f c (vector-ref v i)))))))))

(define (qreduce-n f n i)
 (let loop ((i 0) (c i)) (if (>= i n) c (loop (+ i 1) (f c i)))))

(define (qmap-reduce g i f l . ls)
 (if (null? l)
     i
     (apply qmap-reduce
            g
            (g i (apply f (car l) (map car ls)))
            f
            (cdr l)
            (map cdr ls))))

(define (qmap-reduce-n g i f n)
 (if (zero? n) i (qmap-reduce-n g (g i (f (- n 1))) f (- n 1))))

(define (qmap-reduce-vector g i f v . vs)
 (let loop ((j 0) (result i))
  (if (= j (vector-length v))
      result
      (loop (+ j 1)
	    (g result
	       (apply f
		      (vector-ref v j)
		      (map (lambda (v) (vector-ref v j)) vs)))))))

(define (qset-equale? x y) (set-equale? x y))

;;; Sequences

(define rest cdr)

(define (list-set! l i x)
 (if (zero? i) (set-car! l x) (list-set! (cdr l) (- i 1) x)))

(define (list-insert l i x)
 (if (zero? i)
     (cons x l)
     (cons (first l) (list-insert (rest l) (- i 1) x))))

(define (list-remove l i)
 (if (zero? i) (rest l) (cons (first l) (list-remove (rest l) (- i 1)))))

(define (list-replace l i x)
 (if (zero? i)
     (cons x (rest l))
     (cons (first l) (list-replace (rest l) (- i 1) x))))

(define (but-last x) (reverse (rest (reverse x))))

(define (sublist list start end)
 (if (zero? start)
     (let loop ((list list) (k end))
      (if (zero? k) '() (cons (car list) (loop (cdr list) (- k 1)))))
     (sublist (cdr list) (- start 1) (- end 1))))

(define (reduce-n f i n)
 (let loop ((i 0) (c i)) (if (>= i n) c (loop (+ i 1) (f i c)))))

(define (reduce-vector f i v)
 (let ((n (vector-length v)))
  (cond ((zero? n) i)
	((= n 1) (vector-ref v 0))
	(else (let loop ((i 1) (c (vector-ref v 0)))
	       (if (= i n) c (loop (+ i 1) (f (vector-ref v i) c))))))))

(define (map-reduce g i f l . ls)
 (if (null? l)
     i
     (apply map-reduce
            g
            (g (apply f (car l) (map car ls)) i)
            f
            (cdr l)
            (map cdr ls))))

(define (map-reduce-n g i f n)
 (if (zero? n) i (map-reduce-n g (g (f (- n 1)) i) f (- n 1))))

(define (map-reduce-vector g i f v . vs)
 (let loop ((j 0) (result i))
  (if (= j (vector-length v))
      result
      (loop (+ j 1)
	    (g (apply f
		      (vector-ref v j)
		      (map (lambda (v) (vector-ref v j)) vs))
               result)))))

(define (sum l . n)
 (cond ((vector? l) (qreduce-vector + l 0))
       ((null? n) (qreduce + l 0))
       ((and (= (length n) 1) (procedure? l) (number? (car n)))
        (let ((f l) (n (first n)))
         (let loop ((n (- n 1)) (c 0))
          (if (negative? n) c (loop (- n 1) (+ c (f n)))))))
       ((and (procedure? l) (>= (length n) 1))
        (apply map-reduce + 0 l n))
       (else (error "fuck-up"))))

(define (product l . n)
 (cond ((vector? l) (qreduce-vector * l 1))
       ((null? n) (qreduce * l 1))
       ((and (= (length n) 1) (procedure? l) (number? (car n)))
        (let ((f l) (n (first n)))
         (let loop ((n (- n 1)) (c 1))
          (if (negative? n) c (loop (- n 1) (* c (f n)))))))
       ((and (procedure? l) (>= (length n) 1))
        (apply map-reduce * 1 l n))
       (else (error "fuck-up"))))

(define (factorial n) (product (lambda (i) (+ i 1)) n))

(define (choose n m) (product (lambda (i) (/ (+ i n (- m) 1) (+ i 1))) m))

(define some any)

(define (some-n p n)
 (let loop ((i 0)) (and (< i n) (or (p i) (loop (+ i 1))))))

(define some-vector vector-any)

(define (every-n p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define every-vector vector-every)

(define (one p l . &rest)
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (if (apply p (first l) (map first &rest))
	   (let loop ((l (rest l)) (&rest (map rest &rest)))
	    (or (null? l)
		(and (not (apply p (first l) (map first &rest)))
		     (loop (rest l) (map rest &rest)))))
	   (loop (rest l) (map rest &rest))))))

(define (one-n p n)
 (let loop ((i 0))
  (and (< i n)
       (if (p i)
	   (let loop ((i (+ i 1)))
	    (or (>= i n) (and (not (p i)) (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (one-vector p v . &rest)
 (let loop ((i 0))
  (and (< i (vector-length v))
       (if (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (let loop ((i (+ i 1)))
	    (or (>= i (vector-length v))
		(and (not (apply p
				 (vector-ref v i)
				 (map (lambda (v) (vector-ref v i)) &rest)))
		     (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (for-each-indexed f l)
 (let loop ((i 0) (l l))
  (unless (null? l) (f (first l) i) (loop (+ i 1) (rest l)))))

(define (for-each-indexed-vector f v)
 (for-each-n (lambda (i) (f (vector-ref v i) i)) (vector-length v)))

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (for-each-from-a-up-to-b f a b)
 (let loop ((i a)) (when (< i b) (f i) (loop (+ i 1)))))

(define (for-each-n-decreasing f n)
 (when (> n 0) (let ((i (- n 1))) (f i) (for-each-n-decreasing f i))))

(define (for-each-vector f v . &rest)
 (for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

(define (map-indexed f l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (l l) (c '()))
  (if (null? l)
      (reverse c)
      (loop (+ i 1) (rest l) (cons (f (first l) i) c)))))

(define (map-indexed-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) i (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (map-n f n)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (map-n-vector f n)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (when (< i n)
    (vector-set! v i (f i))
    (loop (+ i 1))))
  v))

(define (enumerate n)
 (let loop ((i (- n 1)) (c '()))
  (if (>= i 0) (loop (- i 1) (cons i c)) c)))

(define (enumerate-vector n)
 (let ((v (make-vector n)))
  (for-each-n (lambda (i) (vector-set! v i i)) n)
  v))

(define (memp p x l)
 (cond ((null? l) #f) ((p x (first l)) l) (else (memp p x (rest l)))))

(define (assp p x alist)
 (and (not (null? alist))
      (if (p x (car (first alist))) (first alist) (assp p x (rest alist)))))

(define (pairwise? p l)
 (or (null? l)
     (let loop ((l1 l) (l2 (rest l)))
      ;; needs work: To make tail recursive.
      (or (null? l2)
	  (and (p (first l1) (first l2)) (loop (rest l1) (rest l2)))))))

(define (adjoinq x l) (if (memq x l) l (cons x l)))

(define (adjoinv x l) (if (memv x l) l (cons x l)))

(define (adjoin x l) (if (member x l) l (cons x l)))

(define (adjoinp p x l) (if (memp p x l) l (cons x l)))

(define (removeq x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eq? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removev x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eqv? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removep p x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removee x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((equal? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if-not p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (positionq x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eq? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positione x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((equal? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (vector-positione val vector)
 (let loop ((i 0))
  (if (< i (vector-length vector))
      (if (equal? val (vector-ref vector i))
          i
          (loop (+ i 1)))
      #f)))

(define (position p l) (position-if p l))

(define (positionp p x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if-not p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l) (+ i 1)))
	(else i))))

(define (findq x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eq? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findv x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eqv? x (first l)) (first l))
	(else (loop (rest l))))))

(define (finde x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((equal? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findp p x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p x (first l)) (first l))
	(else (loop (rest l))))))

(define (find-if p l) (find p l))

(define (find-if-not p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l)))
	(else (first l)))))

(define (countq x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eq? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countv x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eqv? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (counte x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((equal? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countp p x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count-if p l) (count p l))

(define (count-if-not p l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (+ c 1))))))

(define (subsetq? x y) (every (lambda (xe) (memq xe y)) x))

(define (subsetv? x y) (every (lambda (xe) (memv xe y)) x))

(define (subsete? x y) (every (lambda (xe) (member xe y)) x))

(define (subsetp? p x y) (every (lambda (xe) (memp p xe y)) x))

(define (set-equalq? x y) (and (subsetq? x y) (subsetq? y x)))

(define (set-equalv? x y) (and (subsetv? x y) (subsetv? y x)))

(define (set-equale? x y) (and (subsete? x y) (subsete? y x)))

(define (set-equal? p x y) (and (subsetp? p x y) (subsetp? p y x)))

(define set-equalp? set-equal?)

(define (unionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (union x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (intersectionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersection x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (set-differenceq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencev x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-difference x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencep p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-duplicatesq x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memq (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesv x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memv (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicates x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((member (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesp p x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memp p (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (equivalence-classesq x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesq (rest x)))
	    (z (find-if (lambda (w) (eq? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesv x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesv (rest x)))
	    (z (find-if (lambda (w) (eqv? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classes x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classes (rest x)))
	    (z (find-if (lambda (w) (equal? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (transitive-equivalence-classesp p x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (transitive-equivalence-classesp p (rest x)))
	    (z (find-if (lambda (w) (p y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesp p x)
 ;; This wrapper is necessary since P may not be transitive.
 (define (equivalence-classesp p x)
  ;; needs work: To make tail recursive.
  (if (null? x)
      '()
      (let* ((y (first x))
	     (x (equivalence-classesp p (rest x)))
	     (z (find-if (lambda (w) (some (lambda (v) (p y v)) w)) x)))
       (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))
 (let loop ((c (map list x)))
  (let ((d (map (lambda (z) (qreduce append z '()))
		(equivalence-classesp
		 (lambda (x y) (some (lambda (xe) (memp p xe y)) x)) c))))
   (if (= (length d) (length c)) d (loop d)))))

(define (qtopological-sort p l)
 (let loop ((l l) (c '()))
  (if (null? l)
      (reverse c)
      (let ((x (find-if
		(lambda (x1)
		 (not (some (lambda (x2) (and (not (eq? x2 x1)) (p x2 x1)))
			    l)))
		l)))
       (unless x (error "fuck-up"))
       (loop (removeq x l) (cons x c))))))

(define (every-other list)
 (cond ((null? list) '())
       ((null? (rest list)) list)
       (else (cons (first list) (every-other (rest (rest list)))))))

(define (merge list1 list2 predicate key)
 (cond ((null? list1) list2)
       ((null? list2) list1)
       ((predicate (key (first list1)) (key (first list2)))
	(cons (first list1) (merge (rest list1) list2 predicate key)))
       (else (cons (first list2) (merge list1 (rest list2) predicate key)))))

(define (sort list predicate key)
 (if (or (null? list) (null? (rest list)))
     list
     (merge (sort (every-other list) predicate key)
	    (sort (every-other (rest list)) predicate key)
	    predicate
	    key)))

(define (unionvt x y) (if (or (eq? x #t) (eq? y #t)) #t (unionv x y)))

(define (intersectionvt x y)
 (cond ((eq? x #t) y) ((eq? y #t) x) (else (intersectionv x y))))

(define (set-differencevt x y)
 (cond ((eq? y #t) '()) ((eq? x #t) x) (else (set-differencev x y))))

(define (subsetvt? x y)
 (cond ((eq? y #t) #t)
       ((eq? x #t) #f)
       (else (every (lambda (xe) (memq xe y)) x))))

(define (lexicographically<? <? =?)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 (lambda (x y)
  (let loop ((x x) (y y))
   (and (not (null? y))
	(or (null? x)
	    (<? (first x) (first y))
	    (and (=? (first x) (first y)) (loop (rest x) (rest y))))))))

(define (minimal-membersp <? =? l)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 (when (null? l) (error "fuck-up"))
 (let loop ((xs (list (first l))) (l (rest l)))
  (if (null? l)
      xs
      (loop (cond ((<? (first l) (first xs)) (list (first l)))
		  ((=? (first l) (first xs)) (cons (first l) xs))
		  (else	xs))
	    (rest l)))))

(define (map-reduce2 g i f l)
 (if (null? l)
     i
     (map-reduce2
      g
      (g (f (car l)) i)
      f
      (cdr l))))

(define (map-reduce3 g i f l1 l2)
 (if (null? l1)
     i
     (map-reduce3
      g
      (g (f (car l1) (car l2)) i)
      f
      (cdr l1)
      (cdr l2))))

(define (map-accum f i l)
 (let loop ((i i) (l l) (r '()))
  (if (null? l)
      (list i (reverse r))
      (let ((x (f i (car l))))
       (loop (first x) (cdr l) (cons (second x) r))))))

(define (all-pairs l)
 (if (null? l)
     '()
     (append
      (all-pairs (cdr l))
      (map (lambda (r) (list (car l) r)) (cdr l)))))

(define (map-all-pairs f l) (map (lambda (e) (f (first e) (second e))) (all-pairs l)))

(define (map-linear f s e n)
 (map-n (lambda (v) (f (+ s (* v (/ (- e s) n))))) (+ 1 n)))

(define (map-medial f l key)
 (if (null? l)
     l
     (map (lambda (a b) (f (/ (+ (key a) (key b)) 2)))
	  (cdr (sort l < key))
	  (sort l < key))))

(define (ring-forward l)
 (if (> (length l) 1) (cons (last l) (reverse (cdr (reverse l)))) l))

(define (ring-backward l) (append (cdr l) `(,(car l))))

(define (ring-forward-to l o)
 (if (equal? o (car l)) l (ring-forward-to (ring-forward l) o)))

(define (ring-forward-between r a b)
 (take (ring-forward-to r a) (+ (positione b (ring-forward-to r a)) 1)))

(define (maximum l)
 (define (m l x)
  (if (null? l) x
      (if (> (car l) x) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))
(define (minimum l)
 (define (m l x)
  (if (null? l) x
      (if (< (car l) x) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))

(define (maximump p l)
 (define (m l x)
  (if (null? l) x
      (if (> (p (car l)) (p x)) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))
(define (minimump p l)
 (define (m l x)
  (if (null? l) x
      (if (< (p (car l)) (p x)) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))

(define (maximum-with-position l)
 (let loop ((i 0) (r -1) (m #f) (l l))
  (if (null? l)
      (list m r)
      (if (> (first l) (if m m -inf.0))
	  (loop (+ i 1) i (first l) (rest l))
	  (loop (+ i 1) r m (rest l))))))

(define (minimum-with-position l)
 (let loop ((i 0) (r -1) (m #f) (l l))
  (if (null? l)
      (list m r)
      (if (< (first l) (if m m +inf.0))
	  (loop (+ i 1) i (first l) (rest l))
	  (loop (+ i 1) r m (rest l))))))

(define (unzip l)
 (if (null? l) '()
     (map-n (lambda (i) (map (lambda (e) (list-ref e i)) l)) (length (first l)))))

(define (map-m-n f m n)
 (let loop ((i m) (c '()))
  (if (> i n) (reverse c) (loop (+ i 1) (cons (f i) c)))))
(define (map-m-n-indexed f m n)
 (let loop ((i m) (c '()))
  (if (> i n) (reverse c) (loop (+ i 1) (cons (f i (- i m)) c)))))
(define (for-each-m-n f m n) (do ((i m (+ i 1))) ((> i n) #f) (f i)))
(define (for-each-m-n-indexed f m n) (do ((i m (+ i 1))) ((> i n) #f) (f i (- i m))))
(define (for-each-m-n-dec f m n) (do ((i m (- i 1))) ((< i n) #f) (f i)))
)
