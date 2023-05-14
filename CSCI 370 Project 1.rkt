#lang scheme/load

; worked with Jaid Goh

; defining the goal and start states
; (number of missionaries, number of cannibals, boat state)
; A boat state of 0 signifies that the boat is on the left side of the river, while a boat state of 1 signifies that the boat
; is on the right side of the river.
(define gg '(0 0 1))
(define ss '(3 3 0))

; Function that displays all of the possible "next" states given a particular state where the boat is on the left side of the
; river. Note that it does not change the boat state.
(define move-right
  (lambda (cur-state)
         (append (list(list (-(first cur-state) 1)(second cur-state)0) )
                 (append (list(list(first cur-state)(-(second cur-state) 1)0))
                        (append (list(list(-(first cur-state) 1)(-(second cur-state) 1)0))
                               (append (list(list(-(first cur-state) 2)(second cur-state)0))
                                      (append (list (list (first cur-state)(-(second cur-state) 2)0))
                                             '())))))))

; Function that displays all of the possible "next" states given a particular state where the boat is on the right side of the
; river. Note that it does not change the boat state.
(define move-left
  (lambda (cur-state)
         (append (list(list (+(first cur-state) 1)(second cur-state)0)) 
                 (append (list(list (first cur-state)(+(second cur-state) 1)0))
                        (append (list(list(+(first cur-state) 1)(+(second cur-state) 1)0))
                               (append (list )(list (+(first cur-state) 2)(second cur-state)0)
                                      (append (list(list (first cur-state)(+(second cur-state) 2) 0))
                                             '())))))))

; Function that checks to see which states in a given list of states (the possible "next" states for a given state)
; are valid. It returns a list of the valid states.
(define only-valid-states
  (lambda (cur-state state-list new-list)
    (cond
      ((null? state-list) new-list)
      ((or(> (first (car state-list)) 3)(>(second (car state-list))3)) (only-valid-states cur-state (cdr state-list) new-list)) 
      ((or(< (first (car state-list)) 0)(<(second (car state-list))0)) (only-valid-states cur-state (cdr state-list) new-list)) 
      ((and (>(second (car state-list))(first (car state-list))) (not (eqv? (first (car state-list)) 0))) (only-valid-states cur-state (cdr state-list) new-list))
      ((and (>(- 3 (second (car state-list)))(- 3 (first (car state-list)))) (not (eqv? (first (car state-list)) 3))) (only-valid-states cur-state (cdr state-list) new-list))
      (else (define temp-list (cons (car state-list) new-list))
       (only-valid-states cur-state (cdr state-list) temp-list)))))

; Function that returns a list of all possible "next" states for a given state without changing the boat state. It calls
; move-left or move-right according to the current boat state.
(define get-next-states-directions 
  (lambda (cur-state)
    (cond
      ((eqv? (third cur-state) 1) (move-left cur-state)) 
      (else (move-right cur-state)))))

; Function that returns a list of only the valid possible "next" states.
(define get-next-states
  (lambda (cur-state)
    (only-valid-states cur-state (get-next-states-directions cur-state) '())))

; Function that checks to see whether a state from the possible "next" states has already been visited. If so, it does not add
; it to a list of new "next" states that is returned; if not, it adds it to the list that is returned.
(define get-new-states
  (lambda (cur-state state-list)
    (cond
      ((null? state-list) '())
       ((member? (car state-list) cur-state)(get-new-states cur-state (cdr state-list)))
      (else (cons (cons (car state-list) cur-state)
                  (get-new-states cur-state (cdr state-list)))))))

;;------------------ Utility Functions ------------------------------

(define appendToEnd
  (lambda ()
(lambda (newStuff oldStuff)
      (reverse (append newStuff (reverse oldStuff))))))

(define member? (lambda (a lat)
                  (cond
                    ((null? lat) #f)
                    (else (or (equal? (car lat) a)
                              (member? a (cdr lat)))))))

(define list-index
  (lambda (s los)
    (list-index-helper s los 0)))

(define list-index-helper
  (lambda (s los cnt)
    (cond
      ((null? los) -1)
      ((eq? s (car los)) cnt)
      (else (list-index-helper s (cdr los) (+ cnt 1))))))

;------------------------------Main functions that handle the searching---------------------

(define extend-paths
  (lambda ()
    (lambda (x)
      (get-new-states x (get-next-states (car x))))))

; Helper function that conducts dfs and bfs
(define generalized-search
  (lambda (finish path-Extender extend-method path-container closed nodeCnt)
    (newline) (newline) (newline)
    (display "Current set of visited nodes: ")
    (display closed)
    (newline)
    (display "Current Path-container: ")
    (display path-container)
    (newline)
    (cond
      ((null? path-container) (begin (display "search complete") (newline)))
      ((finish (car path-container)) 
(display nodeCnt)
        (display " nodes examined")
        (newline)
        (display (reverse (car path-container)))
        (newline)
        (display ""))

      ((member? (car path-container) closed)
        (display "reject path " )
        (display (reverse (car path-container)))
        (newline)
        (generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      (else 
       (display "Extending path ")
            (display (reverse (car path-container)))
            (newline)
            (generalized-search finish path-Extender extend-method
                                (extend-method (path-Extender (car path-container)) (cdr path-container))
                                (cons (caar path-container) closed)
(+ 1 nodeCnt))))))

;;----------------------------------------------------------------------------------------
;;
;; Provide a function, given a partial path, that will determine if the goal state 
;; has been reached.
;;

(define goal?
  (lambda (finish)
    (lambda (x)
      (equal? finish (car x)))))

; Function that is called to begin a depth-first search on the provided start state. The goal state is also
; provided.
(define mc-dfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) append (list (list start)) '() 0)))

; Function that is called to begin a breadth-first search on the provided start state. The goal state is also
; provided.
(define mc-bfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) (appendToEnd) (list (list start)) '() 0)))

