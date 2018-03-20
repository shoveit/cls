;; input a char-list, (next-buffer '(a b c a b c a))
(defun next-buffer (PATTERN)
  "build prefix-suffix buffer for a pattern"
  (if (= 1 (length PATTERN))
	  (list 0)
	  (let* ((j (car (last PATTERN)))
			 (prefix (next-buffer (butlast PATTERN)))
			 (i (car (last prefix))))
		(if (char= (nth i PATTERN) j)
			(append (next-buffer (butlast PATTERN)) (list (+ 1 i)))
			(append (next-buffer (butlast PATTERN)) (list  0))))))


(defun kmp-match-1 (P S i j)
  "find first match return position index, otherwise return -1.  i is starting index of P, j is starting index of S."
  (let ((next-buffer (next-buffer P)))
	(cond ((= i (length P)) (1- j)) ;; j-1 go back to last iteration and return
		  ((= j (length S)) -1)
		  ((char= (nth i P) (nth j S)) (kmp-match-1 P S (1+ i) (1+ j)))
		  (t
		   (if (= i 0)
			   (kmp-match-1 P S 0 (1+ j))
			   (kmp-match-1 P S (nth (1- i) next-buffer) j)))))))
		  
