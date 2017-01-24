Artificial Intelligence solver for the Game Sokoban
Ro run the file enter the lisp interpreter and do:

(printstates (a* p5 #'goal-test #'next-states #'h0) 0.4) 

where p5 is the level and 0.4 is the speed of display