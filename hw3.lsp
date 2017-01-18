;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
					;
;;helper: visit all squares inside the grid:
;;i.e. visit all numbers in the list of lists

(defun goal-test (s)
   ;;(format t "~%input list:~a" s)
  (cond ( (null s) t) ;;reached the end of the list without finding a box, then reutrn true
	( t (cond
	      ( (null (car s)) (goal-test (cdr s)) )
	      ( t (cond
		    ( (null (caar s)) nil)
		    ( (isBox(caar s)) nil)
		    (t (goal-test (append (list (cdar s)) (cdr s) )))
		    );;end of first cond
		  );;end of t
	      );;end of second cond
	    );;end of t
	);;end of first cond 
		    
  );end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;;

;;numbering counts from (0,0) beeing the upper right corner 
(defun get-square (s r c)
;;  (format t "~%input list:~a" s)
  ;;(format t "~%r=:~a" r)
  ;;(format t "~%c=:~a" c)
  (cond ( (null s) wall) ;;reached the end of the list means we are out of bounds, then return the value of the wall
	( t (cond
	      ( (> r 0) (get-square (cdr s) (- r 1) c ) )
	      ( (< r 0) wall) ;;out of bounds
	      ( t (cond
		    ( (null (caar s)) wall)
		    ( (equal c 0) (caar s) )
		    ( (< c 0) wall)
		    ( (> c 0) (get-square (list (cdar s)) r (- c 1) ) ) 

		    );;end of first cond
		  );;end of t
	      );;end of second cond
	    );;end of t
	);;end of first cond

    );end defun 

(defun set-square (s r c v)
;;  (format t "~%input list:~a" s) 
  (cond ( (null s) nil)
	( t (cond
	      ( (> r 0) (append (list (car s)) (set-square (cdr s) (- r 1) c v ) ) ) ;;go down to the correct row 
	      (t (cond
		   ;;((format t "~%caar:~a" (cdr s)) )
		   ( (null (caar s)) nil)
		   ( (equal c 0) (append (list (append (list v) (cdar s))) (cdr s)))
		   (t   (append
			 (list (append
				(list (caar s))
				(car (set-square (append (list (cdar s)) (cdr s) ) r (- c 1) v) ))) ;;go in the list and append initial element
			 (cdr s))  );;at the end append with the last element
		   );;end cond
		 );;end t
	      );;end cond
	    );;end t
	);;end cond
  );;end defun

;;given keeper at position r,c can he move the box up,down,left right?
(defun move-box(s r c d)
  (cond ( (null s) nil)
	( (equal d 'DOWN) (cond
			    ( (or
			       (equal (get-square s (+ r 2) c) wall);;wall
			       (equal (get-square s (+ r 2) c) box);;or box
			       (equal (get-square s( + r 2) c) boxstar)) nil);;or box on goal then nil
			    (t) ;;end t
			    );;end cond
	  );;end down
	( (equal d 'UP) (cond
			  ( (or
			     (equal (get-square s (- r 2) c) wall);;wall
			     (equal (get-square s (- r 2) c) box);;or box
			     (equal (get-square s( - r 2) c) boxstar)) nil);;or box on goal then nil
			  (t) ;;end t
			  );;end cond
	  );;end up

	( (equal d 'RIGHT) (cond
			     ( (or
				(equal (get-square s r (+ c 2)) wall);;wall
				(equal (get-square s r (+ c 2)) box);;box
				(equal (get-square s r (+ c 2)) boxstar)) nil);;or boxstar return nil
			     (t);;end t
			     );;end cond
	  );;end Right
	( (equal d 'LEFT) (cond
			    ( (or
			       (equal (get-square s r (- c 2)) wall);;wall
			       (equal (get-square s r (- c 2)) box);;box
			       (equal (get-square s r (- c 2)) boxstar)) nil);;or boxstar return nil
			    (t);;end t
			    );;end cond
	  );;end LEFT
				
			      
	);end cond
  );;end defun 
			    
	

(defun merge-box (starp)
  (cond ( (isStar starp) boxstar)
	(t box)
	)
  )

(defun merge-keeper (starp)
;;  (format t "~%MERGE-KEEPER:~a" starp)
  (cond ( (isStar starp) keeperstar)
	(t keeper)
	)
  )

;;what does the keeper leave behind him?
(defun leaving-keeper(keeperp)
  (cond (( isKeeper keeperp) blank)
	(t star)
	)
  )
  
		     



(defun try-move (s d)
  ;;(format t "~%~a " s)
  (let* (( keep (getKeeperPosition s 0))
	 ( c (car keep))
	 (r (car (cdr keep))) );;need the number not the list
    (cond
      ( (equal d 'DOWN) (cond
			
			  ( (equal (get-square s (+ r 1) c) wall) nil) ;;cannot walk into a wall
			  ( (equal (get-square s (+ r 1) c) box ) (cond
								    ( (not (move-box s r c d)) nil);;cannot move box return nil
								    (t (set-square
									(set-square
									 (set-square s (+ r 1) c keeper);;set keeper
									 r c (leaving-keeper(get-square s r c)) );;set blank or goal
									(+ r 2) c (merge-box (get-square s (+ r 2) c)) );;move box 
								       );;end of t
								    );;end of cond
			    );;end of equal

			  ( (equal (get-square s (+ r 1) c) boxstar) (cond
								       ( (not (move-box s r c d)) nil);;cannot move box return nil
								       (t (set-square
									   (set-square
									    (set-square s (+ r 1) c keeperstar);;set keeper
									    r c (leaving-keeper(get-square s r c)));;set blank or goal
									   (+ r 2) c (merge-box (get-square s (+ r 2) c)) );;move box
									  );;end of t
								       );;end of cond
			    );;end of equal

			  ( (equal (get-square s (+ r 1) c) star ) (set-square (set-square s (+ r 1) c keeperstar) r c (leaving-keeper(get-square s r c)) ) );;falling onto a goal

			  ( (equal (get-square s r c) keeperstar) (set-square (set-square s (+ r 1) c (merge-keeper (get-square s (+ r 1) c)))  r c star) );;if current position is keeperstar 
								    
			  ( t  (set-square (set-square s (+ r 1) c keeper) r c (leaving-keeper(get-square s r c)) ) );;first set keeper then call fun again to free
			                                                             ;;prev position of the keeper
			  );;end of cond
	);;end of down
	( (equal d 'UP) (cond
			  ( (equal (get-square s (- r 1) c) wall) nil);;cannot walk into wall
			  ( (equal (get-square s (- r 1) c) box ) (cond
								    ( (not (move-box s r c d)) nil);;cannot move box return nil
								    (t (set-square
									(set-square
									 (set-square s (- r 1) c keeper);;set keeper
									 r c (leaving-keeper(get-square s r c)) );;set blank
									(- r 2) c (merge-box (get-square s (- r 2) c)) );;move box
								       );;end of t
								    );;end of cond
			    );;end of equal

			  ( (equal (get-square s (- r 1) c) boxstar) (cond
								       ( (not (move-box s r c d)) nil);;cannot move box return nil
								       (t (set-square
									   (set-square
									    (set-square s (- r 1) c keeperstar);;set keeper
									    r c (leaving-keeper(get-square s r c)) );;set blank or goal
									   (- r 2) c (merge-box (get-square s (- r 2) c)) );;move box
									  );;end of t
								       );;end of cond
			    );;end of equal

			  ( (equal (get-square s (- r 1) c) star ) (set-square (set-square s (- r 1) c keeperstar) r c (leaving-keeper(get-square s r c)) ) );;falling onto a goal

			  ( (equal (get-square s r c) keeperstar) (set-square (set-square s (- r 1) c (merge-keeper (get-square s (- r 1) c)))  r c star) );;if current position is keeperstar 
			  
			  (t (set-square (set-square s (- r 1) c keeper) r c (leaving-keeper(get-square s r c)) ) );;blank
			  
			  );;end of cond
	  
	  );;end of up

	( (equal d 'RIGHT) (cond
			     ( (equal (get-square s r (+ c 1)) wall) nil);;cannot walk into a wall
			     ( (equal (get-square s r (+ c 1)) box) (cond
								     ( (not (move-box s r c d)) nil);;cannot move box return nil
								     (t (set-square
									 (set-square
									  (set-square s r (+ c 1) keeper);;set keeper
									  r c (leaving-keeper(get-square s r c)) );;set blank or goal
									 r (+ c 2) (merge-box (get-square s r (+ c 2))) );;move box
									);;end of t
								     );;end of cond
				      
			       );;end of equal
			     ( (equal (get-square s r (+ c 1)) boxstar) (cond
									  ( (not (move-box s r c d)) nil);;cannot move box return nil
									  (t (set-square
									      (set-square
									       (set-square s r (+ c 1) keeperstar);;set keeper
									       r c (leaving-keeper(get-square s r c)) );;set blank or goal
									      r (+ c 2) (merge-box (get-square s r (+ c 2))) );;move box
									     );;end of t
									  );;end of cond
			       ) ;;end of equal
			     ( (equal (get-square s r (+ c 1)) star ) (set-square (set-square s r (+ c 1) keeperstar) r c (leaving-keeper(get-square s r c)) ) );;falling onto a goal

			     ( (equal (get-square s r c) keeperstar) (set-square (set-square s r (+ c 1) (merge-keeper (get-square s r (+ c 1) )))  r c star) );;if current position is keeperstar 

			     
			     (t (set-square (set-square s r (+ c 1) keeper) r c (leaving-keeper(get-square s r c)) ) )
			     );;end of cond
	  );;end of right

	( (equal d 'LEFT) (cond
			    ( (equal (get-square s r (- c 1)) wall) nil);;cannot walk into a wall
			    ( (equal (get-square s r (- c 1)) box) (cond
								     ( (not (move-box s r c d)) nil);;cannot move box return nil
								     (t (set-square
									 (set-square
									  (set-square s r (- c 1) keeper );;set keeper
									  r c (leaving-keeper(get-square s r c)) );;set blank or goal?
									 r (- c 2) (merge-box (get-square s r (- c 2))) );;move box
									);;end of t
								     );;end of cond
			      );;end of equal
			    ( (equal (get-square s r (- c 1)) boxstar) (cond
									 ( (not (move-box s r c d)) nil);;cannot move box return nil
									 (t (set-square
									     (set-square
									      (set-square s r (- c 1) keeperstar);;set keeper
									      r c (leaving-keeper(get-square s r c)) );;set blank
									     r (- c 2) (merge-box (get-square s r (- c 2))) );;move box
									    );;end of t
									 );;end of cond
			      );;end of equal

			    ( (equal (get-square s r (- c 1)) star ) (set-square (set-square s r (- c 1) keeperstar) r c (leaving-keeper(get-square s r c)) ) );;falling onto a goal

			    ( (equal (get-square s r c) keeperstar) (set-square (set-square s r (- c 1) (merge-keeper (get-square s r (- c 1) )))  r c star) );;if current position is keeperstar 

			    
			    (t (set-square (set-square s r (- c 1) keeper) r c (leaving-keeper(get-square s r c)) ) )
			    );;end of cond
	  );;end of left
			      
								     
									  
			      
	
	
	(t 'invalidDirection)
	
			      
	
	);;end of cond
    
    );;end of let
  
  );;end defun

    
    
    
    



    
(defun next-states (s)
  (cleanuplist (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT) ))
  )

;;helper
(defun print-next-states (s)
    (format t "~%input state:~a ~%" s)
  (printstate s)
  (cond
    ( (null s) nil)
    ( t (printstates (next-states s) 0.1) )
    );;end of cond
  );;end defun 

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;;simply visit every node in the state and check if it a box
;;if the box is inside the goal then it will be a boxstar

(defun check-List( li)
  (cond
    ( (null li) 0)
    (t (cond
	 ( (isBox (car li)) (+ 1 (check-List (cdr li)) ))
	 (t (+ 0 (check-List (cdr li))))
	 );;end of cond
       );;end of t
    );;end of cond
  );;ebd defun 

;;this is an admissible heristic
(defun h1 (s)

  (cond
    ( (null s) 0)
    (t (+ (check-List (car s)) (h1 (cdr s))))
    )
  
  
  )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;;

(defun boolean-to-number(b)
  (cond ( (equal b t) 1)
	(t 0)
	)
  )

;;count moves of box at position r c
(defun count-freedom(s r c)
  (let* ((x 0))
    (+ x
       (boolean-to-number( move-box s (- r 1) c 'up))
       (boolean-to-number( move-box s (+ r 1) c 'down))
       (boolean-to-number( move-box s r (+ c 1) 'left))
       (boolean-to-number( move-box s r (- c 1) 'right))

       ) )
  );;end of sum

	     
	     


;;helper function finds distance of box and targets
;;given the coordinates of the box
(defun can-move-box(s r c)
;;  (format t "~%input list:~a" s)
  (cond ( (null s) 0) ;;reached the end of the list without finding a box, then reutrn true
	( t (cond
	      ( (null (car s)) (can-move-box (cdr s) (+ r 1) 0 ) )
	      ( t (cond
		    ( (null (caar s)) nil)
		    ( (isBox(caar s)) (+ (count-freedom s r c)
					 (can-move-box (append (list (cdar s)) (cdr s) ) r (+ 1 c) )) )
				    
		    (t  (can-move-box (append (list (cdar s)) (cdr s) ) r (+ 1 c) ) )
		    );;end of first cond
		  );;end of t
	      );;end of second cond
	    );;end of t
	);;end of first cond

    );end defun 
	 

;;we want the state with the most available moves to be the best
;;thus since we calculate the maximum number of moves we should
;;negate the results and then picking the minimum works as a heristic
(defun h004142185 (s)
  (- 0 (can-move-box s 0 0));;start checking from 0,0
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#


;;MY EXAMPLES DELETE AFTERWARDS
(setq panos1 '((0 0 2 0 0)
	       (0 0 2 0 0)
	       (4 5 3 5 4)
	       (0 0 5 0 0)
	       (0 0 0 0 0)
	       ))

(setq panos2 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 5 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	              (1 1 1 1 1 1))) 


;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;;MY EXAMPLE
(setq myp4 '( (1 1 1)
	    (4 0 0)
	   ;; (0 1 0)
	    (2 1 0)
	    (3 1 0)
	    )
     )

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))
;;EXAMPLE MY P9

(setq myp9 '(
	     (1 1 1 1 1 1 )
	     (1 1 1 0 0 1)
	     (1 0 0 0 2 0)
	     (1 0 1 0 0 1)
	     (1 0 4 2 6 1)
	     (1 1 1 1 1 1)
	     ))
	   
      

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
