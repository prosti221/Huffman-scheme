(load "huffman.scm")

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

(define (infix-eval exp)
  ((cadr exp) (car exp) (caddr exp))
  )

(define (decode-hale bits tree)
    (define (decode-1 bits current-branch res)
      (if (null? bits)
          res
          (let ((next-branch
                 (choose-branch (car bits) current-branch)))
            (if (leaf? next-branch)
                 (let ((next (append res
                         (list (symbol-leaf next-branch)))))
                 (decode-1 (cdr bits) tree next))
                (decode-1 (cdr bits) next-branch res)))))
    (decode-1 bits tree '()))

(decode sample-code sample-tree)

(define (encode symbols tree)
  (define (encode-1 symbols current-branch res)
    (if (null? symbols)
        res
    (let ((next-branch
           (choose-branch-2 (car symbols)
                            current-branch)))
      (cond ((equal? '(leaf) (cdr next-branch)) (encode-1 (cdr symbols) tree (append res (list (car next-branch)))))
            (else (encode-1 symbols (cdr next-branch) (append res (list (car next-branch)))))))))
  (encode-1 symbols tree '()))

(define (element-of-set? x set)
   (cond ((null? set) #f)
         ((equal? x (car set)) #t)
         (else (element-of-set? x (cdr set)))))

(define (choose-branch-2 symb branch)
  (let ((next-right
         (right-branch branch))
         (next-left
         (left-branch branch)))
    (cond ((and (leaf? next-right) (equal? (cadr next-right) symb)) (list 1 'leaf))
          ((and (leaf? next-left) (equal? (cadr next-left) symb)) (list 0 'leaf))
          ((and (not (leaf? next-right)) (element-of-set? symb (caddr next-right))) (append '(1) next-right))
          ((and (not (leaf? next-left)) (element-of-set? symb (caddr next-left))) (append '(0) next-left)))))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

(define (grow-huffman-tree nodes)
  (define (iter result)
    (if (= (length result) 2)
        result
        (let ((current-pair
               (make-code-tree (car result)
                               (cadr result))))
          (iter (adjoin-set current-pair
                            (cddr result))))))
  (let ((sorted
         (make-leaf-set nodes)))
    (iter sorted)))

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)
