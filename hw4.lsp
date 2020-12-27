;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
    ;uses the formula (n-1)*k+c to compute variable index
    (+ (* (- n 1) k) c)
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
    ;call node2var(n c k) with the original c until c = k and return the list of all outputs
    ;the OR of each variable is true if at least one variable is true
    (cond ((> c k) nil)
          ((= c k) (list (node2var n c k)))
          (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
        )
  )

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;

;function should only return true if either 0 or 1 variables are true
;ORs the NOT of each variable to each of the other variables in pairs and ANDs all the results
;if more than 1 variable is true, then one of the pairs must return false, otherwise they
;should all return true
(defun at-most-one-color (n c k)
    ;gets a list of clauses of each combination pair within the constrained set of variables
    (let ((L (at-least-one-color n c k)))
        (cond ((null (cdr L)) nil)
              (t (append (comb (car L) (cdr L)) (at-most-one-color n (+ c 1) k)))
        ))
  )

;helper function for at-most-one-color that takes in a variable v and a list of variables L
;returns a list of pairs consisting of v with each element in L
;negates each variable in the process
(defun comb (v L)
    (cond ((null L) nil)
          (t (cons (cons (* -1 v) (list (* -1 (car L)))) (comb v (cdr L))))
        )
    )


; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;

;the constraint (exactly 1) is true if (at least 1) AND (at most 1) = true
(defun generate-node-clauses (n k)
    (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;

;e is a list containing a pair of nodes
;the function returns true if no node in e contains variables from the same index that are both true
;gets list of pairs of negated variables from both nodes at each index from 1 to k
(defun generate-edge-clauses (e k)
    ;generates the ordered lists of possible variables from each node
    ;get the list of pair clauses between the two lists
    (pairs (at-least-one-color (car e) 1 k) (at-least-one-color (cadr e) 1 k))
  )

;helper function for generate-edge-clauses that takes in two lists L1 and L2
;returns a list of variable pairs from both lists at each index
;each variable is negated
(defun pairs (L1 L2)
    (cond ((null L1) nil)
          (t (cons (cons (* -1 (car L1)) (list (* -1 (car L2)))) (pairs (cdr L1) (cdr L2)))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
