;;;; CSci 1901 Project - Spring 2008
;;;; dots++ Player AI

;;;======================;;;
;;;  SUBMISSION DETAILS  ;;;
;;;======================;;;

;; List both partners' information below.
;; Leave the second list blank if you worked alone.
(define authors 
  '((
     "Eli Johnson"
     "john4042"
     "2157097"
     "Sec. 02"
     )
    (
     ""   ;; Author 2 Name
     ""   ;; Author 2 X500
     ""   ;; Author 2 ID
     ""   ;; Author 2 Section
     )))

;; ITLabs Machine Tested On: 
;;


;;;====================;;;
;;;  Player-Procedure  ;;;
;;;====================;;;

(define player-procedure
  (let () 

    ;; ABOVE: may want to include 'board' as a let-defined constant.
    ;; be sure to check how 'board' is implemented.
    
    ;;===================;;
    ;; Helper Procedures ;;
    ;;===============================================================;;
    ;; Include procedures used by get-move that are not available in ;;
    ;;  the util.scm file.  Note: dots++.scm loads util.scm, so you  ;;
    ;;  do not need to load it from this file.                       ;;
    ;; You also have access to the constants defined inside of       ;;
    ;;  dots++.scm.                                                  ;;
    ;;===============================================================;;

    ;; Returns a random-element from a list.
    (define (random-element lst)
      (list-ref lst (random (length lst))))
    
    ;; Below is no longer used.
    
    ;;(define (get-random-valid-position board)
    ;;  (let* ((position (random-element (get-open-positions board)))
	;;     (line (random-element (get-open-lines position board)))
	;;     (line-position (make-line-position line position)))
	;;line-position))
    
    ;; returns true if the line-position would not set the opponent up for an immediate
    ;; hex capture, otherwise returns false
    (define (decent-move? move board)
      (let ((move2 (if (line-shared-with move) (line-shared-with move) move)))
        (and (not (= 2 (count-open-lines (get-position move) board)))
             (not (= 2 (count-open-lines (get-position move2) board))))))

    ;; returns true if the hex has at least one line such that decent-move? is true.
    (define (has-decent-move? position board)
      (define (helper lst)
        (and (not (null? lst))
             (or (decent-move? (make-line-position (car lst) position) board)
                 (helper (cdr lst)))))
      (helper (get-open-lines position board)))
    
    ;; returns true if there is any move anywhere that doesn't set up the opponent
    ;; for an immediate capture.
    (define (exists-decent-move? board)
      (define (helper lst)
        (and (not (null? lst))
             (or (has-decent-move? (car lst) board) (helper (cdr lst)))))
      (helper (get-open-positions board))
      )
    
    ;; currently the same as random position, except will not select lines that
    ;; set the opponent up for an easy capture.
    (define (get-decent-valid-position board)
      (let* ((position (random-element (filter (lambda (x) (has-decent-move? x board))
                                               (get-open-positions board))))
             (line (random-element (filter (lambda (x) 
                                             (decent-move? (make-line-position x position) 
                                                                               board))
                                           (get-open-lines position board))))
             (line-position (make-line-position line position)))
        line-position))

    ;; returns a list of the number of open lines for every board position.
    (define (line-count-list board)
      (map (lambda (pos) (count-open-lines pos board)) (enumerate-positions)))
    
    ;; Evaluates true if a list contains a 1, and false otherwise.
    (define (takeable? list)
      (and (not (null? list)) (or (= 1 (car list)) (takeable? (cdr list)))))

    ;; returns line-position that will capture a hex.
    (define (capture-hex board)
      (define (helper A)
	(cond ((null? A) #f)
              ((not (pair? A)) #f)
	      ((= 1 (count-open-lines (car A) board)) (make-line-position
                                                       (car (get-open-lines (car A) board))
                                                       (car A)))
	      (else (helper (cdr A)))))
      (helper (get-open-positions board)))
    
    ;;===========================;;
    ;; End-game helper functions ;;
    ;;===========================;;

    ;; returns the subset of get-open-positions that have only 2-lines open.
    (define (get-corridor-positions board)
      (filter (lambda (x) (= 2 (count-open-lines x board))) (get-open-positions board)))
    
    ;; slightly modified from lab 9.  this takes two sets and returns their set
    ;; difference.  expensive to use -- O(n^2).  used only by partition-board.
    (define (set-diff A B)
      (cond ((null? A) '())
            ((null? B) A)
            ((member (car A) B) (set-diff (cdr A) (cdr B)))
            (else (cons (car A) (set-diff (cdr A) B)))))
    
    ;; set diff tester:
    (define (set-diff-tester set1)
      (let ((set2 set1)
            (set3 set1))
        (set-diff set2 set3)))
    
    ;; procedure is passed a non-null list of "corridor positions" and returns a
    ;; connected component of the list. this is a helper procedure for partition-board. 
    ;; NOTE: makes certain assumptions about board that will be true in endgame.
    
    ;; NOTE: Runtime error most likely in this procedure.  Possibly infinite loop.
    
    (define (exhaust-section position-lst board)
      (define (helper new-lst)
        (let ((head (make-line-position (car (get-open-lines (car new-lst) board)) (car new-lst)))
              (tail (make-line-position (cadr (get-open-lines (car new-lst) board)) (car new-lst)))
              (status 1))
          (cond ((or (not (line-shared-with head)) 
                     (not (memq (get-position (line-shared-with head)) position-lst)))
                 (if (not (eq? status 1)) new-lst 
                     (begin (set! status head) (set! head tail) 
                            (set! tail status) (helper new-lst))))
                ((eq? tail (line-shared-with head)) new-lst)
                ((eq? (car (get-open-lines (get-position (line-shared-with head)) board))
                      (get-line (line-shared-with head)))
                 (begin (set! head (cadr (get-open-lines (get-position (line-shared-with head)) board)))
                        (helper (cons (get-position head) new-lst))))
                (else 
                 (begin (set! head (car (get-open-lines (get-position (line-shared-with head)) board)))
                        (helper (cons (get-position head) new-lst))))
                 )))
      (helper (list (car position-lst))))
    
    ;; this is an endgame procedure that returns a list of lists; each inner list
    ;; is the positions composing a connected component of the board.
    ;; called by grudgingly-concede
    (define (partition-board board)
      (let ((sublist '()))
        (define (helper diagram remaining)
          (cond ((null? remaining) diagram)
                (else (begin (set! sublist (exhaust-section remaining board))
                             (helper (cons sublist diagram) (set-diff remaining sublist))))))
        (helper '() (get-corridor-positions board))))
    
    ;; when implemented, will give up the smallest region to the other player.
    ;; called when there is no choice but to add a 5th line to a hex.  NOTE: no
    ;; null value since partition board will never be called to return null.
    (define (grudgingly-concede board)
      (let ((good-hex 'unchosen)
            (partition (partition-board board)))
        (define (helper nested-lst)
          (cond ((null? (cdr nested-lst)) (car nested-lst))
                ((< (length (car nested-lst)) (length (cadr nested-lst)))
                 (helper (cons (car nested-lst) (cddr nested-lst))))
                (else (helper (cdr nested-lst)))))
        (begin (set! good-hex (car (helper partition)))
               (make-line-position (car (get-open-lines good-hex board)) good-hex))))

    ;;====================;;
    ;; Get-Move Procedure ;;
    ;;===============================================================;;
    ;; This is the procedure called by dots++.scm to get your move.  ;;
    ;; Returns a line-position object.
    ;;===============================================================;;

    (define (get-move player board)
      (cond ((takeable? (line-count-list board)) (capture-hex board))
            ((not (null? (set-diff-tester (get-open-positions board))))
             "set-diff is officially broken")
            ((not (null? (get-corridor-positions board)))
             (make-line-position (car (get-open-lines (car (get-corridor-positions board)) board))
                                 (car (get-corridor-positions board))))
            ((exists-decent-move? board) (get-decent-valid-position board))
	    (else (get-random-valid-position board))))


    ;; Return get-move procedure
    get-move

    )) ;; End of player-procedure
    
