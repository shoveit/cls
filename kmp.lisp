;; (declaim (optimize (debug 0) (safety 0) (speed 3)))
;; input a string, (next-buffer "abcabca")
(defun next-buffer (PATTERN)
  (if (= 1 (length PATTERN))
	  (list 0)
	  (let* ((j (char PATTERN (1- (length PATTERN))))
			 (prefix (next-buffer (subseq PATTERN 0 (1- (length PATTERN)))))
			 (i (car (last prefix))))
		(if (char= (char PATTERN i) j)
			(append (next-buffer (subseq PATTERN 0 (1- (length PATTERN)))) (list (+ 1 i)))
			(append (next-buffer (subseq PATTERN 0 (1- (length PATTERN)))) (list  0))))))

(defun next-buffer (PATTERN nil) 
  (if (= 1 (length PATTERN))
	  (list '(0))
	  (let* ((j (char PATTERN (1- (length PATTERN))))
			 (prefix (next-buffer (subseq PATTERN 0 (1- (length PATTERN)))))
			 (i (car (last prefix))))
		(if (char= (char PATTERN i) j)
			(append (next-buffer (subseq PATTERN 0 (1- (length PATTERN)))) (list (+ 1 i)))
			(append (next-buffer (subseq PATTERN 0 (1- (length PATTERN))) (cons)) (list  0))))))

;; input 2 strings, 2 indexes, (kmp-match "abc" "abeabca")
(defun kmp-match-1 (P S)
  "find first match return position index, otherwise return -1.  i is starting index of P, j is starting index of S."
	(labels ((match (P S i j buffer)
				 (cond ((= i (length P)) (- j (length P))) ;; go back to start position
					   ((= j (length S)) -1)
					   ((char= (char P i) (char S j)) (match P S (1+ i) (1+ j) buffer))
					   (t
						(if (= i 0)
							(match P S 0 (1+ j) buffer)
							(match P S (nth (1- i) buffer) j buffer))))))
	  (match P S 0 0 (next-buffer P))))

(defun kmp-match-all (P S)
  "find first match return position index, otherwise return -1.  i is starting index of P, j is starting index of S."
	(labels ((match (P S i j buffer pos)
			   (cond ((= i (length P)) (match P S 0 j buffer (cons (- j (length P)) pos))) ;; go back to start position
					 ((= j (length S)) pos)
					 ((char= (char P i) (char S j)) (match P S (1+ i) (1+ j) buffer pos))
					 (t
					  (if (= i 0)
						  (match P S 0 (1+ j) buffer pos)
						  (match P S (nth (1- i) buffer) j buffer pos))))))
	  (reverse (match P S 0 0 (next-buffer P) nil))))

;; -- TODO (fuzzy-kmp-next-buffer)
(defun kmp-match-all (P S pos)
  (let ((j (kmp-match-1 P S 0 0)))
	(if (= j -1)
		(kmp-match-all P (subseq S j) pos)
		(kmp-match-all P (subseq S j) (cons j pos)))))


