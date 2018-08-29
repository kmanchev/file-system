#lang racket

; ==== private methods ====

; getting last element of a list
(define (last-element ls)
  (cond ((null? (cdr ls)) (car ls))
        (else (last-element (cdr ls)))
  )
)

;remove last element of list
(define (remove-list-last-element ls)
  (if (null? ls) '()
      (reverse (cdr (reverse ls)))
  )
)

;counting number of elements
(define (count-list-elements ls)
  (if (null? ls) 0
      (+ 1 (count-list-elements (cdr ls)))
  )
)

; appending element to the tail of a list
(define (append-tail elem lst)
  (foldr cons (list elem) lst)
)

; ==== end private methods ====

;creates a list of entries by splitting the full name of a dir/file by the symbol '/'
(define (split-entry-name path)
  (if (equal? path "/") '("")
      (regexp-split #rx"/" path)
  )
)

(define (split-string-by-space path)
  (regexp-split #rx" " path)
)

(define (build-entry-name-from-list ls)
  (build-entry-name-helper (cdr ls) "/")
)

(define (build-entry-name-helper ls str)
  (cond ((null? ls) str)
        ((null? (cdr ls)) (build-entry-name-helper (cdr ls) (string-append str (car ls))))
        (else (build-entry-name-helper (cdr ls) (string-append str (car ls) "/") ) )
  )
)

; getting the name for given node. This is a little bit ugly, given the convoluted data model
(define (get-node-full-name node)
  (cadar node)
)

(define (get-node-simple-name node)
  (trim-name (cadar node))
)

;getting the name of the folder/file only 
(define (trim-name full-name)
  (last-element (split-entry-name full-name))
)

(define (remove-member-from-entry-list path entry-list)
    (cond
     ((null? entry-list) entry-list)
     ((equal? path (get-node-full-name (car entry-list))) (cdr entry-list))
     (else (cons (car entry-list) (remove-member-from-entry-list path (cdr entry-list))))
    )
)

(define (remove-member-from-node path node)
  (append-tail (cons "content" (cons (remove-member-from-entry-list path (get-entry-list node)) '())) (append-tail (cons "type" (cons "dir" '()) ) (append-tail (cons "name" (cons (get-node-full-name node) '()) ) '() ) ))
)

; getting the type for given node. This is a little bit ugly, given the convoluted data model
(define (get-node-type node)
  (cadadr node)
)

; getting the members list for given node. This is a little bit ugly, given the convoluted data model
(define (get-entry-list node)
  (car (cdaddr node))
)

; init the initial structure of a new directory
(define (init-dir-meta dir-full-name)
  (append-tail (cons "content" (cons '() '())) (append-tail (cons "type" (cons "dir" '()) ) (append-tail (cons "name" (cons dir-full-name '()) ) '() ) ))
)

(define (add-to-content-in-entry content entry)
  (cons (car entry) (cons (cadr entry) (cons (cons "content" (cons (cons content (get-entry-list entry)) '() ) ) '()) ) )
)

; == working with Nodes segment ==

(define (get-member-from-entry-list node entry-name)
  (get-member-from-entry-list-helper (get-entry-list node) entry-name)
)

(define (get-member-from-entry-list-helper content-list entry-name)
  (cond ((null? content-list) '())
        ((equal? (get-node-simple-name (car content-list) ) entry-name) (car content-list))
        (else (get-member-from-entry-list-helper (cdr content-list) entry-name))
  )
)

; get a node from the file-system by full path 
(define (get-node-by-full-path path)
  (get-node-by-full-path-helper file-system (split-entry-name path))
)

(define (get-node-by-full-path-helper node dir-list)
  (cond ((and (null? (cdr dir-list) ) (equal? (get-node-simple-name node) (car dir-list) ) ) node)
        ((and (equal? (get-node-simple-name node) (car dir-list) ) (not (null? (get-member-from-entry-list node (cadr dir-list) ) ) ) ) (get-node-by-full-path-helper (get-member-from-entry-list node (cadr dir-list) ) (cdr dir-list) ) )
        (else '())
  )
)

; == working with Nodes END segment ==

; == CD segment ==
(define (cd path)
  (cond ((and (= (count-list-elements (split-entry-name path) ) 1) (equal? (car (split-entry-name path)) "")) (modify-current-dir "/"))
        ((is-relative-path (split-entry-name path)) (change-dir-relative (split-entry-name path)))
        ((and (equal? (car (split-entry-name path) ) "") (check-dir-exist file-system (split-entry-name path) ) ) (modify-current-dir path) )
        ((and (not (equal? (car (split-entry-name path) ) "") ) (check-dir-exist file-system (append (split-entry-name current-dir) (split-entry-name path)) ) ) (modify-current-dir (string-append current-dir "/" path)) )
  )
)

(define (change-dir-relative dir-list)
  (modify-current-dir (build-entry-name-from-list (change-dir-relative-helper (split-entry-name current-dir) dir-list) ) )
)

(define (change-dir-relative-helper current-dir-list dir-list)
  (cond ((or (and (null? dir-list) (null? current-dir-list)) (null? current-dir-list)) '(""))
        ((null? dir-list) current-dir-list)
        ((equal? (car dir-list ) "..") (change-dir-relative-helper (remove-list-last-element current-dir-list) (cdr dir-list)))
        ((change-dir-relative-helper (append-tail (car dir-list) current-dir-list) (cdr dir-list)))
  )
)

(define (check-dir-exist node dir-list)
  (cond ((and (= (count-list-elements dir-list) 1) (equal? (get-node-simple-name node) (car dir-list) ) ) #t)
        ((and (equal? (get-node-simple-name node) (car dir-list) ) (not (null? (get-member-from-entry-list node (cadr dir-list) ) ) ) ) (check-dir-exist (get-member-from-entry-list node (cadr dir-list) ) (cdr dir-list) ) )
        (else #f)
  )
)

(define (is-relative-path path)
  (if (not (equal? (car path) "")) #t
      #f
  )
)

; == end CD segment ==


; == MKDIR segment ==

; creating a directory 
(define (mkdir dir)
  (if (is-path dir) (modify-file-system (mkdir-in-path dir))
      (modify-file-system (mkdir-in-current-dir dir))
  )
)

(define (is-path entry)
  (if (> (count-list-elements (split-entry-name entry) ) 1) #t
      #f
  )
)

(define (mkdir-in-path dir)
  (if (check-dir-exist file-system (remove-list-last-element (split-entry-name dir))) (build-new-file-system (add-to-content-in-entry (init-dir-meta dir) (get-node-by-full-path (get-parent-dir-path dir) ) ) )
      "wrong path"
  )
)

(define (mkdir-in-current-dir dir)
  (if (equal? current-dir "/") (add-to-content-in-entry (init-dir-meta (string-append current-dir dir)) (get-node-by-full-path (get-parent-dir-path (string-append current-dir dir)) ) )
      (build-new-file-system (add-to-content-in-entry (init-dir-meta (string-append current-dir "/" dir)) (get-node-by-full-path (get-parent-dir-path (string-append current-dir "/" dir)) ) ) )
  )
)

(define (build-new-file-system node-to-add)
  (build-new-file-system-helper (get-parent-dir-path (get-node-full-name node-to-add)) node-to-add)
)


(define (build-new-file-system-helper parent-dir node-to-add)
  (if (equal? parent-dir "/") (add-to-content-in-entry node-to-add (remove-member-from-node (get-node-full-name node-to-add) (get-node-by-full-path parent-dir)))
      (build-new-file-system-helper (get-parent-dir-path parent-dir) (add-to-content-in-entry node-to-add (remove-member-from-node (get-node-full-name node-to-add) (get-node-by-full-path parent-dir))))
  )
)

;helper
(define (get-parent-dir-path dir)
  (if (equal? dir "/") "/"
      (build-entry-name-from-list (remove-list-last-element (split-entry-name dir) ) )
  )
)

; == MKDIR segment END ===


; == LS PWD segment ==

;printing the full current directory
(define (pwd) current-dir)

;listing all files and folders in the current directory if no path parameter given
(define (ls . path)
  (if (null? path) (list-current-dir)
      (list-given-path (car path)))
)

;helper for listing directories and folders when path is provided
(define (list-given-path path)
  (list-dir-helper (get-entry-list (get-node-by-full-path path)))
)

;helper
(define (list-current-dir)
  (list-dir-helper (get-entry-list (get-node-by-full-path current-dir)))
)

;helper
(define (list-dir-helper content)
  (when (not (null? content))
	(display (cdaar content))
        (display (get-node-full-name content))
	(newline)
	(list-dir-helper (cdr content) )
  )
)

; == LS PWD segment END ==


; == RM Segment START ==

(define (rm params)
  (cond (to-rm-elem-from-current-dir (split-string-by-space params)) (rm-elem-from-current-dir (last-element (split-string-by-space params)))
        (to-rm-elements-recursively (split-string-by-space params)) (rm-elements-recursively (last-element (split-string-by-space params)))
  )
)

;to implement
(define (to-rm-elem-from-current-dir params)
  #f
)

;to implement
(define (to-rm-elements-recursively params)
  #f
)

;to implement
(define (rm-elem-from-current-dir path)
  #f
)

;to implement
(define (rm-elements-recursively path)
  #f
)

; the current directory model 
(define file-system '(("name" "/") ("type" "dir") ("content" () ) ))

(define current-dir (get-node-full-name file-system))

;modifying file-system
(define (modify-file-system new-fs)
  (set! file-system new-fs)
)

;modifying the current dir
(define (modify-current-dir new-dir)
  (set! current-dir new-dir)
)



; This is testing perpose model - to test nested folder structure 
;======================================================================================================

(define test-fs '(
  ("name" "/") 
  ("type" "dir") 
  ("content"
    (
	  ( ("name" "/dir1") ("type" "dir") ("content" ())) 
	  ( ("name" "/dir2") ("type" "dir") ("content" ()))
	  ( ("name" "/dir3") 
	    ("type" "dir") 
	    ("content" (
	                 (("name" "/dir3/dir31") ("type" "dir") ("content" ())) 
	                 (("name" "/dir3/dir32") ("type" "dir") ("content" ()))
	                 (("name" "/dir3/dir33") ("type" "dir") ("content" (
                                                                                 (("name" "/dir3/dir33/dir331") ("type" "dir") ("content" ())) 
	                                                                         (("name" "/dir3/dir33/dir332") ("type" "dir") ("content" ()))
                                                                               )
                                                                    )
                         )
	              )  
	    )
	  )
	)
  )
))

(define test-list '(("name" "/dir3/dir33") ("type" "dir") 
                              ("content" ( (("name" "/dir3/dir33/dir333") ("type" "dir") ("content" ())) 
					   (("name" "/dir3/dir33/dir331") ("type" "dir") ("content" ())) 
					   (("name" "/dir3/dir33/dir332") ("type" "dir") ("content" ()))
                                         )
                              ))

)

(define test-list2 '(("name" "/dir3")
  ("type" "dir")
  ("content"
   ((("name" "/dir3/dir31") ("type" "dir") ("content" ()))
    (("name" "/dir3/dir32") ("type" "dir") ("content" ()))
    (("name" "/dir3/dir33") ("type" "dir") ("content" ((("name" "/dir3/dir33/dir331") ("type" "dir") ("content" ())) (("name" "/dir3/dir33/dir332") ("type" "dir") ("content" ())))))))))

;======================================================================================================
