;==============================================================================;
;--*   P2   *----*        *----*        *    *        *----*        *----*   P2;
;   \      /      \           ____________________   /      \      /      \    ;
;    *----*   P2   *----*    |                    | *   P2   *----*   P1   *---;
;          \      /          |       dots++       |  \      /      \      /    ;
;--*        *----*        *  |____________________|   *----*   P1   *----*     ;
;   \      /      \                                  /      \      /           ;
;    *    *   P2   *    *        *----*        *----*   P1   *----*        *   ;
;==============================================================================;
;        Computer Science 1901 - Spring 2008 - University of Minnesota         ;
;                             Final Course Project                             ;
;==============================================================================;


;;;=====================;;;
;;;  DEFINED CONSTANTS  ;;;
;;;=====================;;;

;; Tracks version of code in case updates are released.
(define *SOURCE-REVISION* 57)

;; Size of the board.  
(define *NUM-ROWS* 3)
(define *NUM-COLUMNS* 6)  ;; Must be even.

;; Game-Play Delay Settings
(define *SLEEP-MILLISECONDS* 100) ;; Time to pause between moves.
(define *STALL* #t)               ;; Prompt to continue.

;; Symbols used to keep track of players, position status, and game results.
(define *OPEN-SYMBOL*     'OPEN)
(define *PLAYER-1-SYMBOL* 'P1)
(define *PLAYER-2-SYMBOL* 'P2)
(define *TIE-SYMBOL*      'TIE)

;; Value to specify if line is open or taken.
(define *LINE-OPEN*  0)
(define *LINE-TAKEN* 1)

;; Number of sides each piece has.  Do not modify.
(define *NUM-SIDES* 6)

;; Specifies symbols to use to draw the board.
(define *BOARD-OPEN*     "    ")
(define *BOARD-PLAYER-1* " P1 ")
(define *BOARD-PLAYER-2* " P2 ")
(define *BOARD-DOT*      "*")

;; Specifies how lines are drawn on the board.  One pair for each line.  
;; First item is drawn when the line is open, second is drawn when taken.
(define *BOARD-LINES* 
  '(board-line-pairs ("    " "----") (" " "\\") (" " "/")
		     ("    " "----") (" " "\\") (" " "/")))

;; Prompt Strings
(define *ROW-PROMPT* 
  (string-append "Select Row (1-" (number->string *NUM-ROWS*) "): "))
(define *COLUMN-PROMPT* 
  (string-append "Select Column (1-" (number->string *NUM-COLUMNS*) "): "))
(define *LINE-PROMPT* 
  (string-append "Select Line (1-" (number->string *NUM-SIDES*) "): "))
(define *PLAYER-1-PROMPT* "Player 1's Turn")
(define *PLAYER-2-PROMPT* "Player 2's Turn")
(define *WAIT-PROMPT*    "Hit enter to continue:")

;; Error Messages
(define *INVALID-POSITION-STRING* "Invalid position specified.")
(define *INVALID-LINE-STRING*     "Invalid line specified.")
(define *CANNOT-MOVE-STRING*      "Selected line is not open.")

;; Game Over Strings
(define *GAME-OVER-INVALID-MOVE*    "Game Over!\nInvalid move by player.\n")
(define *GAME-OVER-TIE-STRING*      "Game Over!\nGame ends in tie.\n")
(define *GAME-OVER-PLAYER-1-STRING* "Game Over!\nPlayer 1 wins.\n")
(define *GAME-OVER-PLAYER-2-STRING* "Game Over!\nPlayer 2 wins.\n")

;; Display Score Strings
(define *PLAYER-1-SCORE* "Player 1 Score: ")
(define *PLAYER-2-SCORE* "Player 2 Score: ")


;;;======================;;;
;;;  COMPATIBILITY CODE  ;;;
;;;======================;;;

;; SELECT YOUR IMPLEMENTATION ;;
;;----------------------------;;
(define implementation 
  (let ((mit  0)  ;; GNU/MIT Scheme
	(plt  1)  ;; PLT Scheme Implementations -- Dr. Scheme
	(stk  2)) ;; STk -- Scheme w/ Tk

    ;;Implementation being used:
    plt
))

(define (sleep-ms ms)
  (cond ((= implementation 0) ;; MIT
	 (sleep-current-thread ms))
	((= implementation 1) ;; PLT
	 (sleep (/ *SLEEP-MILLISECONDS* 1000.0)))
	((= implementation 2) ;; STk
	 (sleep ms))
	(else
	 #f)))


;;;=======================;;;
;;;  LOAD EXTERNAL FILES  ;;;
;;;=======================;;;
(load "util.scm")  ;; General Utility Procedures
(load "help.scm")  ;; dots++ Helper Procedures


;;;====================;;;
;;;  GLOBAL VARIABLES  ;;;
;;;====================;;;
(define player-procedure #f)


;;;==================;;;
;;;  MAIN PROCEDURE  ;;;
;;;==================;;;
(define (play-game player1-file player2-file)


  ;;======================;;
  ;;  DISPLAY PROCEDURES  ;;
  ;;======================;;

  ;; Display current game board.
  (define (display-board board)
    (display (board->string board)))

  ;; Get string with player label and score.
  (define (score-string player board)
    (string-append
     (if (eq? player *PLAYER-1-SYMBOL*)
	 *PLAYER-1-SCORE*
	 *PLAYER-2-SCORE*)
     (number->string (count-owned-positions player board))))

  ;; Display both player's current scores.
  (define (display-score board . winner)
    (let* ((player-1-score-string (score-string *PLAYER-1-SYMBOL* board))
	   (player-2-score-string (score-string *PLAYER-2-SYMBOL* board))
	   (length1 (string-length player-1-score-string))
	   (length2 (string-length player-2-score-string))
	   (maxlength (max length1 length2))
	   (spacer (make-string
		    (round (/ (- (* *NUM-COLUMNS* 7) maxlength) 2)) 
		    #\space)))

      (display+ spacer "|  " player-1-score-string 
		(make-string (- maxlength length1) #\space) "  |\n")
      (display+ spacer "|  " player-2-score-string 
		(make-string (- maxlength length2) #\space) "  |\n")
      (display+ spacer "'" (make-string (+ maxlength 4)  #\-) "'\n")

      (if (not (null? winner))
	  (display+ (winner-string (car winner)) "\n"))))

  ;; Display string specifying who's turn it is.
  (define (display-player-prompt current-player)
    (display+ (prompt-string current-player) "\n"))


  ;; Converts a board to a [large, overly complex] string.
  (define (board->string board)

    ;; Get the string to display on the board for a given symbol.
    (define (symbol->board-string symbol)
      (cond ((eq? symbol *PLAYER-1-SYMBOL*) *BOARD-PLAYER-1*)
	    ((eq? symbol *PLAYER-2-SYMBOL*) *BOARD-PLAYER-2*)
	    ((eq? symbol *OPEN-SYMBOL*)     *BOARD-OPEN*)
	    (else "")))

    ;; Returns a string for the item ([0] owner or [1-6] line) at row/col.
    (define (S row col item)
      (let* ((pos (make-position row col))
	     (hex (if (valid-position? pos)
		      (lookup-hex pos board)
		      (make-empty-hex))))

	(if (= item 0)
	    ;; Return the string representing the owner of the position.
	    (symbol->board-string (car hex))

	    ;; Get the string to display depending on line status.
	    (list-ref  
	     (list-ref *BOARD-LINES* item) ;; Output Choices. Ex: '(" " "/")
	     (list-ref hex item)))))       ;; Line Status -- 0 or 1

    
    ;; Short-Hand Definitions for Spacing
    (define 1e " ")      (define 5e "     ")     (define 9e  "         ")
    (define 2e "  ")     (define 6e "      ")    (define 10e "          ")
    (define 3e "   ")    (define 7e "       ")   (define 11e "           ")
    (define 4e "    ")   (define 8e "        ")  (define 12e "            ")

    ;; Build list with odd column indices
    ;;   When building board string, will do two columns at a time.
    (define odd-columns 
      (filter odd? (map 1+ (iota *NUM-COLUMNS*))))

    ;; Build list representing partial board string, given a reference row.
    ;; Includes items : 0,1,2,3,5,6 of odd  column positions of row r
    ;;                  4           of odd  column positions of row r-1
    ;;                  1,2,6       of even column positions of row r
    ;;                  0,3,4,5     of even column positions of row r-1
    (define (row->string-tree r)
      (list

       ;; Line 1
       ;;--------
       (append 
	(list "| ")
	(concat-map
	 (lambda (c)
	   (list 2e *BOARD-DOT* (S r c 1) *BOARD-DOT* 2e (S (- r 1) (+ c 1) 0)))
	 odd-columns)

	;; Special Case For Top Row
	(if (= r 1) 
	    (list "    |\n")
	    (list 2e *BOARD-DOT* " |\n")))

       ;; Line 2
       ;;--------
       (append 
	(list "| ")
	(concat-map 
	 (lambda (c)
	   (list 1e (S r c 6) 6e (S r c 2) 5e))
	 odd-columns)

	;; Special Case For Top Row
	(if (= r 1) 
	    (list "    |\n")
	    (list 1e (S (- r 1) *NUM-COLUMNS* 3) "  |\n")))

       ;; Line 3
       ;;--------
       (append 
	(list "| ")
	(concat-map 
	 (lambda (c)
	   (list *BOARD-DOT* 2e (S r c 0) 2e *BOARD-DOT* (S r (+ c 1) 1)))
	 odd-columns)
	(list *BOARD-DOT*)
	(list "   |\n"))

       ;; Line 4
       ;;--------
       (append 
	(list "| ")
	(concat-map 
	 (lambda (c)
	   (list 1e (S r c 5) 6e (S r c 3) 5e))
	 odd-columns)
	(list 1e (S r *NUM-COLUMNS* 2))
	(list "  |\n"))
       ))


    ;; List representing the bottom of the board.
    (define footer-row
      (let ((r *NUM-ROWS*))
	(list

	 ;; Line 1
	 ;;--------
	 (append 
	  (list "| ")
	  (concat-map 
	   (lambda (c)
	     (list 2e *BOARD-DOT* (S r c 4) *BOARD-DOT* 2e (S r (+ c 1) 0)))
	   odd-columns)
	  (list 2e *BOARD-DOT*)
	  (list " |\n"))
       
	 ;; Line 2
	 ;;--------
	 (append 
	  (list "| ")
	  (list 8e (S r 2 5) 5e) ;; First Two Columns
	  (concat-map 
	   (lambda (c)
	     (list 1e (S r (- c 1) 3) 6e (S r (+ c 1) 5) 5e))
	   (cdr odd-columns))
	  (list 1e (S r *NUM-COLUMNS* 3))
	  (list "  |\n"))
	 
	 ;; Line 3
	 ;;--------
	 (append 
	  (list "| ")
	  (list 9e *BOARD-DOT* (S r 2 4)) ;; First Column
	  (concat-map 
	   (lambda (c)
	     (list *BOARD-DOT* 8e *BOARD-DOT* (S r (+ c 1) 4)))
	   (cdr odd-columns))
	  (list *BOARD-DOT*)
	  (list "   |\n"))
	 )))

    (define top-border
      (string-append
       "\n"
       ".-[ dots++ ]-" (make-string (- (* *NUM-COLUMNS* 7) 8) #\-) "-.\n"
       "| " (make-string (+ (* *NUM-COLUMNS* 7) 3) #\space) " |\n"))

    (define bottom-border
      (string-append
       "| " (make-string (+ (* *NUM-COLUMNS* 7) 3) #\space) " |\n"
       "'-" (make-string (+ (* *NUM-COLUMNS* 7) 3) #\-)     "-'\n"))

    (string-append
     top-border
     (apply string-append
	    (enumerate-tree 
	     (append 
	      (concat-map 
	       (lambda (row) 
		 (row->string-tree row))
	       (map 1+ (iota *NUM-ROWS*)))
	      footer-row)))
     bottom-border))



  ;;======;;
  ;; MISC ;;
  ;;======;;

  (define (safe-load filename)
    (let ((result (ignore-errors (lambda () (load filename)))))
      (if (condition? result)
	  'error 
	  result)))

  ;; Convert file string to player-procedure
  (define (file->player file)
    (cond ((equal? file 'human) 'human)
	  (else
	   (set! player-procedure #f)
	   (load file)
	   (if player-procedure
	       player-procedure
	       (error "player-procedure not properly defined.")))))

  (define (read-line)
    (let ((char (peek-char)))
      (if (eof-object? char)
	  char
	  (list->string
	   (let loop ((char char))
	     (if (or (eof-object? char)
		     (equal? #\newline char)
		     (equal? #\return char)
		     (equal? #\linefeed char))
		 (begin
		   (read-char)
		   '())
		 (begin
		   (read-char)
		   (cons char
			 (loop (peek-char))))))))))

  ;;=======;;
  ;; MOVES ;;
  ;;=======;;

  ;; Gets a move from the human and doesn't give up
  (define (get-human-move player board board-string)

    ;; Displays error if the input was invalid and prompts again.
    (define (handle-invalid invalid-string) 
      (newline) (display board-string)
      (newline) (display invalid-string)
      (newline) (display (prompt-string player))
      (newline) (get-human-move player board board-string))

    ;; Prompts for human player to input row, column, and line.
    (define (get-line-position)
      (let ((row '()) (column '()) (line '()))
	(display *ROW-PROMPT*) (flush-output) (set! row (read))
	(display *COLUMN-PROMPT*) (flush-output) (set! column (read))
	(display *LINE-PROMPT*) (flush-output) (set! line (read))
	(make-line-position line row column)))


    (let ((line-position (get-line-position)))
      (cond ((not (valid-line-position? line-position))
	     (handle-invalid *INVALID-LINE-STRING*))
	    ((not (valid-position? (get-position line-position)))
	     (handle-invalid *INVALID-POSITION-STRING*))
	    ((not (open-line? (get-line-number line-position)
			      (get-position line-position) 
			      board))
	     (handle-invalid *CANNOT-MOVE-STRING*))
	    (else line-position))))


  ;;=============;;
  ;; INFORMATIVE ;; 
  ;;=============;;

  ;; Returns PLAYER-1-SYMBOL, PLAYER-2-SYMBOL, or TIE-SYMBOL
  (define (get-winner board)
    (let* ((player-1-count (count-owned-positions *PLAYER-1-SYMBOL* board))
	   (player-2-count (count-owned-positions *PLAYER-2-SYMBOL* board)))

	(cond ((> player-1-count player-2-count) *PLAYER-1-SYMBOL*)
	      ((> player-2-count player-1-count) *PLAYER-2-SYMBOL*)
	      (else *TIE-SYMBOL*))))


  ;;======================;;
  ;;  Begin Main Process  ;;
  ;;======================;;

  (let* ((player1-procedure (file->player player1-file))
	 (player2-procedure (file->player player2-file))
	 (current-player *PLAYER-1-SYMBOL*)
	 (board (make-board)))

    ;; Gets a position from human or player procedure
    (define (get-a-line-position board board-string)
      (let ((player-procedure 
	     (if (eq? current-player *PLAYER-1-SYMBOL*) 
		 player1-procedure 
		 player2-procedure)))
	
	(if (eq? player-procedure 'human)
	    (get-human-move current-player board board-string)
	    (player-procedure current-player (tree-copy board)))))

    ;; Forces Loop To Pause
    (define (do-delay)
      (if (not (or (eq? player1-file 'human) 
		   (eq? player2-file 'human)))
	  (begin
	    (flush-output) 
	    (sleep-ms *SLEEP-MILLISECONDS*)
	    (if *STALL* 
		(begin (display *WAIT-PROMPT*) 
		       (flush-output) 
		       (read-line)
		       (newline))))))
  
    ;;===================;;
    ;; Main program loop ;;
    ;;===================;;
    (define (loop)
      (let* ((board-string (board->string board))
	     (line-position '()))

	;; Display Current Board State and Score
	(display board-string)
	(display-score board)

	;; Get Input From Player
	(display-player-prompt current-player) (flush-output)
	(set! line-position (get-a-line-position board board-string))
	
	;; Check Input For Validity
	(cond ((and (valid-line-position? line-position)
		    (valid-position? (get-position line-position))
		    (open-position? (get-position line-position) board)
		    (open-line? (get-line-number line-position)
				(get-position line-position) board))
	       
	      
	       ;; Execute Move.  
	       ;; If make-move! returns true, they captured
	       ;;  a position and get to go again.
	       (if (make-move! current-player line-position board)
		   (if (board-filled? board)

		       ;; Game Over
		       (let ((winner (get-winner board)))
			 (display (board->string board))
			 (display-score board winner)
			 winner)
		       
		       ;; Game Continues
		       (begin
			 (do-delay)
			 (loop)))
		  
		   ;; No Positions Captured
		   (begin
		     (set! current-player (other-player current-player))
		     (do-delay)
		     (loop))))
	      
	      (else (display board-string)
		    (display-score board (other-player current-player))
		    (display *GAME-OVER-INVALID-MOVE*)
		    (other-player current-player)))))

    ;; Execute Main Program Loop
    (loop))

  )  ;; End Play-Game Procedure



;;;==============;;;
;;; TESTING CODE ;;;
;;;==============;;;

;; Run play-game n times and display results.
(define (run-tests player1-file player2-file n)
  ;; Store settings temporarily.
  (let ((old-sleep *SLEEP-MILLISECONDS*)
	(old-stall *STALL*))
    
    ;; Update Settings
    (set! *SLEEP-MILLISECONDS* 0)
    (set! *STALL* #f)

    ;; Suppress normal output.
    ;; If there is a crash while this procedure is running, display and newline
    ;;   may not work.  In that case, from the prompt, make the following calls:
    ;;     (display 'reset)
    ;;     (newline 'reset)
    (set! newline  
	  (let ((old-newline newline))
	    (define (temp-newline . args) ;; Using define rather than lambda
	      (if (and (not (null? args)) ;; to let this work in Dr Scheme
		       (eq? (car args) 'reset)) 
		  (set! newline old-newline) #f))
	    temp-newline))

    (set! display 
	  (let ((old-display display))
	    (define (temp-display . args) ;; Using define rather than lambda
	      (if (and (not (null? args)) ;; to let this work in Dr Scheme
		       (eq? (car args) 'reset)) 
		  (set! display old-display) #f))
	    temp-display))

    ;; Run games and store results in a list.
    (let ((results (map
		    (lambda (n)
		      (play-game player1-file player2-file))
		    (iota n))))

      ;; Restore previous definitions of display and newline.
      (display 'reset) (newline 'reset)

      ;; Display Results
      (newline) (newline)
      (display "TEST RESULTS: ") (newline)
      (display "============================================") (newline)
      (display "Player 1: ") (display player1-file) (newline)
      (display "Player 2: ") (display player2-file) (newline)
      (display "============================================") (newline)
      (display "Total Games Run: ") (display n) (newline)
      (newline)

      (display "Player 1 Wins: ") 
      (display (count *PLAYER-1-SYMBOL* results)) (newline)
      (display "Player 1 Win Percentage: ") 
      (display (* 100.0 (/ (count *PLAYER-1-SYMBOL* results) n))) (display "%")
      (newline) (newline)
    
      (display "Player 2 Wins: ") 
      (display (count *PLAYER-2-SYMBOL* results)) (newline)
      (display "Player 2 Win Percentage: ") 
      (display (* 100.0 (/ (count *PLAYER-2-SYMBOL* results) n))) (display "%")
      (newline) (newline)

      (display "Tie Games: ")
      (display (count *TIE-SYMBOL* results)) (newline)
      (display "Tie Percentage: ")
      (display (* 100.0 (/ (count *TIE-SYMBOL* results) n))) (display "%")
      (newline))

    ;; Restore Settings
    (set! *SLEEP-MILLISECONDS* old-sleep)
    (set! *STALL* old-stall)

    )
  'done)

