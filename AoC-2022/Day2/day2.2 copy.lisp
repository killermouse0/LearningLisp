(ql:quickload "str")


(defparameter *data-file* "AoC/Day2/aoc_2.txt")
(defparameter *rules-win*
              (list (cons 'rock 'scissors)
                    (cons 'scissors 'paper)
                    (cons 'paper 'rock)))

(defparameter *strategy*
              '(rock (scissors rock paper)
                     paper (rock paper scissors)
                     scissors (paper scissors rock)))

(defun item-value (x)
  (cond ((equal x 'rock) 1)
        ((equal x 'paper) 2)
        ((equal x 'scissors) 3)))

(defun my-score (x y)
  (cond ((equal x y) (+ (item-value y) 3)) ; DRAW
        ((member (cons x y) *rules-win* :test #'equal) (+ (item-value y) 0)) ; OPPONENT WON
        (t (+ (item-value y) 6)))) ; WE WON

(defun apply-strategy (opponent-play reco)
  (let ((move-candidates (cadr (member opponent-play *strategy*))))
    (cond ((eql reco 'X) (first move-candidates))
          ((eql reco 'Y) (second move-candidates))
          ((eql reco 'Z) (third move-candidates)))))

(defun map-moves (move)
  (case move ((A X) 'rock)
    ((B Y) 'paper)
    ((C Z) 'scissors)
    (otherwise "Nothing")))

(defun tally (in acc)
  (let ((line (read-line in nil :eof)))
    (cond ((eq line :eof) acc)
          ((equal line "") acc)
          (t (let* ((words (str:words line))
                    (opponent-play (map-moves (intern (first words))))
                    (my-play (apply-strategy opponent-play (intern (second words))))
                    (round-score (my-score opponent-play my-play)))
               (tally in (+ round-score acc)))))))

(with-open-file (in *data-file*)
  (tally in 0))