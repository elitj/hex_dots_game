;;;; CSci 1901 Project - Spring 2008
;;;; dots++ Player AI

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

;; ITLabs Machine Tested On: Hawk
;;


;;;====================;;;
;;;  Player-Procedure  ;;;
;;;====================;;;

(define player-procedure
  (let ((master-diag '())) 
    
    ;;===================================;;
    ;;    Procedures:                    ;;
    ;;    1. Helper Predicates           ;;
    ;;    2. Position Filters            ;;
    ;;    3. Master-Diagram Constructors ;;
    ;;    4. Line-Position Selectors     ;;
    ;;    5. Miscellaneous Functions     ;;
    ;;    6. Endgame Decision Tree       ;;
    ;;    7. Get-Move Procedure          ;;
    ;;===================================;;
    
    ;;======== 1 ========;;
    ;; Helper Predicates ;;
    ;;===================;;

    ;; returns true if the line-position would not set the opponent up for an immediate
    ;; hex capture, otherwise returns false
    (define (decent-move? move board)
      (let ((move2 (if (line-shared-with move) (line-shared-with move) move)))
        (and (not (= 2 (count-open-lines (get-position move) board)))
             (not (= 2 (count-open-lines (get-position move2) board))))))

    ;; returns true if the hex has at least one line such that decent-move? is true.
    (define (has-decent-move? position board)
      (not (null? (filter (lambda (x) (decent-move? (make-line-position x position) board)) (get-open-lines position board)))))
    
    ;; Evaluates true if there is a hex to be taken, and false otherwise.
    (define (takeable? board)
      (not (null? (get-all-takeable board))))
    
    ;; returns true if the positions are the same.
    (define (same-position? pos1 pos2)
      (and (= (cadr pos1) (cadr pos2)) (= (caddr pos1) (caddr pos2))))

    ;; determines whether the board is sufficiently "saturated" to use endgame procedures.
    (define (endgame? board)
      (null? (filter (lambda (x) (has-decent-move? x board)) (get-connectors board))))
 
    ;; returns true if the next component on the board to be taken is a chain of 3 or more hexes
    (define (big-chain? board)
      (define (helper long-chains)
        (and (not (null? long-chains))
             (or (non-empty-intersection? (get-all-takeable board) 
                                          (car long-chains))
                 (helper (cdr long-chains)))))
      (helper (filter (lambda (x) (> (length x) 2)) master-diag)))
    
    ;; returns true if the two lists share one or more elements
    (define (non-empty-intersection? lstA lstB)
      (cond ((or (null? lstA) (null? lstB)) #f)
            ((not (null? (filter (lambda (x) (same-position? (car lstA) x)) lstB))) #t)
            (else (non-empty-intersection? (cdr lstA) lstB))))
    
    ;; returns true if i can collect more than one region. Expensive to use -- O(n^3)
    ;; implemented for opponents who behave erratically, and also when opponent gives up a region
    ;; by dividing it in two.
    (define (embarassment-of-riches? board)
      (< 2 (length (filter (lambda (x) (non-empty-intersection? (get-all-takeable board) x)) master-diag))))
    
    ;; returns true if the line-position is on the edge of a connected component
    (define (boundary? line-pos)
      (or (not (line-shared-with line-pos))
          (< 2 (get-open-lines (get-position (line-shared-with line-pos)) board))))
    
    ;; returns true if a region is not a loop and is not closed off on both ends
    (define (open-set? A)
      (and (not (null? A))
           (or (not (null? (filter boundary? (get-open-lines (car A) board))))
               (open-set? (cdr A)))))

    ;; returns true if a connected pair of hexes is able to be captured.
    (define (take-two? board)
      (define (helper diag)
        (and (= 2 (length (car diag)))
             (or (= 1 (count-open-lines (caar diag) board))
                 (= 1 (count-open-lines (cadar diag) board))
                 (and (not (null? (cdr diag)))
                      (helper (cdr diag))))))
      (helper master-diag))
    
    
    ;;======== 2 ========;;
    ;; Position Filters  ;;
    ;;===================;;
    
    ;; returns a list of all takeable positions on the board
    (define (get-all-takeable board)
      (filter (lambda (x) (= 1 (count-open-lines x board))) (get-open-positions board)))

    ;; returns the subset of get-open-positions that have exactly 2 lines open.
    (define (get-corridor-positions board)
      (filter (lambda (x) (= 2 (count-open-lines x board))) (get-open-positions board)))
    
    ;; returns the subset of open positions that have 1 or 2 lines open.
    (define (get-active-regions board)
      (filter (lambda (x) (> 3 (count-open-lines x board))) (get-open-positions board)))
    
    ;; returns the endgame positions that connect active regions / takeable components
    (define (get-connectors board)
      (filter (lambda (x) (< 2 (count-open-lines x board))) (get-open-positions board)))
    
    ;; return a portion of the master-diag that includes all single-hexes available for the taking.
    (define (lonely-hexes board)
      (filter (lambda (x) (and (= 1 (length x)) (= 1 (length (get-open-lines (car x) board))))) master-diag))
    
    ;; modified from lab 9.  returns the set-difference of lists of positions.
    (define (set-diff A B)
      (cond ((null? A) '())
            ((null? B) A)
            (else (set-diff (filter (lambda (x) (not (same-position? (car B) x))) A) (cdr B)))))
    
    ;; Returns a random-element from a list.
    (define (random-element lst)
      (list-ref lst (random (length lst))))
    
    ;;============= 3 =============;;
    ;; Master-Diagram Constructors ;;
    ;;=============================;;
    
    ;; Procedure is passed a non-null list of "active regions" by partition-board and returns 
    ;; a connected component of the list. This makes certain assumptions about board that will 
    ;; be true in endgame. Admittedly the code is sloppy.  It is due to the fact that it had 
    ;; to be heavily changed as I added more functionality to the player.  If I had more time 
    ;; I would rework this to be more readable.
    ;; The general explanation is this: The procedure begins with a random open hex and adds 
    ;; hexes that are connected to it by moving the line-position 'head'.  The variable 'status' 
    ;; is a counter that determines whether it will be necessary to go back and explore in the
    ;; other direction.
    (define (exhaust-section position-lst board)
      (let ((head (make-line-position (car (get-open-lines (car position-lst) board)) 
                                        (car position-lst)))
	    (status 1))
	(define (helper new-lst)
	  (cond ((= 1 (count-open-lines (get-position head) board))
                 (cond ((or (= status 2)
                            (not (line-shared-with head))
                            (not (member (get-position (line-shared-with head)) position-lst))) new-lst)
                       ((= 1 (count-open-lines (get-position (line-shared-with head)) board))
                        (cons (get-position (line-shared-with head)) new-lst))
                       ((eq? (car (get-open-lines (get-position (line-shared-with head)) board))
			     (get-line-number (line-shared-with head)))
			(begin (set! status (+ status 1))
			       (set! head (make-line-position (cadr (get-open-lines (get-position (line-shared-with head)) 
										    board))
							      (get-position (line-shared-with head))))
			       (helper (cons (get-position head) new-lst))))
		       (else (begin (set! status (+ status 1))
			       (set! head (make-line-position (car (get-open-lines (get-position (line-shared-with head)) 
										   board))
							      (get-position (line-shared-with head))))
			       (helper (cons (get-position head) new-lst))))))
                ((or (not (line-shared-with head))
                     (not (member (get-position (line-shared-with head)) position-lst)))
                 (if (= status 2) new-lst 
                     (begin (set! status (+ status 1)) 
                            (set! head (make-line-position 
                                        (cadr (get-open-lines (car position-lst) board))
                                        (car position-lst))) 
                            (helper new-lst))))
                ((member (get-position (line-shared-with head)) new-lst) new-lst)
                ((= 1 (count-open-lines (get-position (line-shared-with head)) board))
                 (if (= status 2) (cons (get-position (line-shared-with head)) new-lst)
                     (begin (set! status (+ status 1))
			    (set! new-lst (cons (get-position (line-shared-with head)) new-lst))
                            (set! head (make-line-position 
                                        (cadr (get-open-lines (car position-lst) board))
                                        (car position-lst)))
			    (helper new-lst))))
                ((eq? (car (get-open-lines (get-position (line-shared-with head)) board))
                      (get-line-number (line-shared-with head)))
                 (begin (set! head (make-line-position 
                                    (cadr (get-open-lines (get-position (line-shared-with head)) board))
                                    (get-position (line-shared-with head))))
                        (helper (cons (get-position head) new-lst))))
                (else 
                 (begin (set! head (make-line-position 
                                    (car (get-open-lines (get-position (line-shared-with head)) board))
                                    (get-position (line-shared-with head))))
                        (helper (cons (get-position head) new-lst))))))
	(helper (list (car position-lst)))))
    
    ;; this is an endgame procedure that returns a list of lists; each inner list
    ;; is the positions composing a connected component of the board.
    (define (partition-board board)
      (define (helper diagram remaining)
        (cond ((null? remaining) diagram)
              (else (let ((sublist (exhaust-section remaining board)))
                      (helper (cons sublist diagram) (set-diff remaining sublist))))))
      (helper '() (get-active-regions board)))
    
    ;; takes the above non-null partition and orders it from smallest region of hexes to largest.
    ;; this only gets called once per turn, but it returns the value for the variable 'master-diag',
    ;; a diagram which is an essential shorthand for the state of the board in many procedures.
    (define (order-partition partition)
      (define (outsideproc ordered unordered)
        (define (helper new elem)
          (cond ((null? new) (cons elem new))
                ((<= (length elem) (length (car new))) (cons elem new))
                (else (cons (car new) (helper (cdr new) elem)))))
        (cond ((null? unordered) ordered)
              (else (outsideproc (helper ordered (car unordered)) (cdr unordered)))))
      (outsideproc '() partition))

    
    ;;=========== 4 ===========;;
    ;; Line-position selectors ;;
    ;;=========================;;
    
    ;; returns _ANY_ line-position that will capture a hex.
    (define (capture-hex board)
      (let ((pos (car (get-all-takeable board))))
        (make-line-position (car (get-open-lines pos board)) pos)))
    
    ;; selects any random position that will not set the opponent up for immediate capture
    (define (get-decent-valid-position board)
      (let* ((position (random-element (filter (lambda (x) (has-decent-move? x board))
                                               (get-open-positions board))))
             (line (random-element (filter (lambda (x) (decent-move? (make-line-position x position) board))
                                           (get-open-lines position board))))
             (line-position (make-line-position line position)))
        line-position))
    
    ;; will give up the smallest region to the other player.
    (define (give-away-smallest board)
      (let ((good-hex (caar master-diag)))
        (make-line-position (car (get-open-lines good-hex board)) good-hex)))

    ;; attempts to sacrifice a pair of hexes by returning it back to the other player
    (define (attempt-return board)
      (if (= 2 (count-open-lines (get-position (line-shared-with (capture-hex board))) board))
          (get-other-line-position (line-shared-with (capture-hex board)) board)
          (capture-hex board)))
    
    ;; gives up a region of two hexes to the other player, with the potential of it being sacrificed back
    (define (soft-gift board)
      (let* ((pos1 (caar master-diag))
             (pos2 (cadar master-diag))
             (line1 (make-line-position (car (get-open-lines pos1 board)) pos1)))
        (cond ((not (line-shared-with line1)) line1)
              ((same-position? pos2 (get-position (line-shared-with line1))) (get-other-line-position line1 board))
              (else line1))))
    
    ;; gives a region of two hexes to the other player in such a way that he must accept it.
    (define (hard-gift board)
      (let* ((pos1 (caar master-diag))
             (pos2 (cadar master-diag))
             (line1 (make-line-position (car (get-open-lines pos1 board)) pos1)))
        (cond ((not (line-shared-with line1)) (get-other-line-position line1 board))
              ((same-position? pos2 (get-position (line-shared-with line1))) line1)
              (else (get-other-line-position line1 board)))))

    
    ;;=========== 5 ===========;;
    ;; Miscellaneous Functions ;;
    ;;=========================;;    

    ;; given one line-position, returns the second line-position in the same hex as the first.
    ;; NOTE: only gets called with hexes that have exactly two lines open
    (define (get-other-line-position line-position board)
      (if (= (get-line-number line-position) (car (get-open-lines (get-position line-position) board)))
          (make-line-position (cadr (get-open-lines (get-position line-position) board)) (get-position line-position))
          (make-line-position (car (get-open-lines (get-position line-position) board)) (get-position line-position))))
    
    ;; counts the number of pairs, i.e, regions which consist of two hexes
    (define (count-pairs diag)
      (define (helper count diag)
        (cond ((null? diag) (if (even? count) count (+ count 1))) ;; guarantees we don't make sacrifice 
                                                                  ;; if the whole board is pairs
              ((not (= 2 (length (car diag)))) count)
              (else (helper (+ 1 count) (cdr diag)))))
      (helper 0 master-diag))

    
    
    
    ;;========== 6 ==========;;
    ;; Endgame Decision Tree ;;
    ;;=======================;;    
    
    ;; All my endgame strategy is summed up in this procedure.  Much of the endgame requires a certain shorthand
    ;; for the board, which is what master-diag is: an ordered list of position "components," i.e. sets of positions
    ;; that will all be taken on the same turn.  First the procedure checks for disconnected single components, or if
    ;; we are on the final component, since these are always safe to take.  Secondly, the procedure looks for a big 
    ;; chain that can be taken and will orchestrate it so that the final two hexes are left for the other player.  
    ;; The take-two? and give-two? predicates determine when to take components of size two and when to sacrifice 
    ;; them back to the other player. Otherwise the final condition is reached and we ensure that we give up the 
    ;; smallest region to the other player.
    
    ;; The strategy centers around "pairs", components of size two that dictate the control of the game.
    ;; In a nutshell, for most long chains, it's possible to maintain control of the game by sacrificing
    ;; the final pair.  If there is a sequence of pairs before the larger chains, the order of who takes
    ;; which pair is crucial.  This is why sacrificing, and preventing sacrifice, are key.
    
    ;; The strategy is fairly simplified.  It could be improved by counting points and further analyzing
    ;; which regions are loops.  Also, the number of pairs can fluctuate, especially on a large board, when
    ;; "connector" hexes are closed up and join two sections.  If I had time I would attack these issues 
    ;; in the next generation.
    
    (define (endgame-proc board)
      (begin (set! master-diag (order-partition (partition-board board)))
             (cond ((not (null? (lonely-hexes board)))  ;; take individual hexes
                    (make-line-position (car (get-open-lines (caar (lonely-hexes board)) board)) 
                                        (caar (lonely-hexes board))))
                   ((or (big-chain? board) (and (= 1 (length master-diag)) (takeable? board))) 
                    (capture-hex board)) ;; take final big chains
                   ((take-two? board) (cond ((or (even? (count-pairs master-diag)) ;; take two
                                                 (embarassment-of-riches? board)) (capture-hex board))
                                            (else (attempt-return board))))
                   ((and (= 2 (length (car master-diag))) (not (takeable? board)))
                    (if (even? (count-pairs master-diag)) ;; give two
                        (soft-gift board) 
                        (hard-gift board)))
                   ((takeable? board) (capture-hex board)) ;; any situation not covered above
                   (else (give-away-smallest board)))))  ;; give up the smallest region / minimize loss
                
                
    ;;======== 7 =========;;
    ;; Get-Move Procedure ;;
    ;;====================;;

    (define (get-move player board)
      (begin (set! master-diag '())  ;; redundant, but makes sure it's reset.
             (cond ((endgame? board) (endgame-proc board))
                   ((takeable? board) (capture-hex board))
                   (else (get-decent-valid-position board)))))


    ;; Return get-move procedure
    get-move

    )) ;; End of player-procedure
    
