;;; FUNCTIONAL IMPLEMENTATION WITH HIGH-ORDER FUNCTIONS AND KEY PARAMETERS

;;; FIXED VERSION OF HOF IMPLEMENTATION

(defun bubble-sort-hof (lst &key (key #'identity) (test #'>))
  "Sorts list using bubble sort with high-order functions.
   :key - function to extract comparison value from element
   :test - comparison predicate (default: #'> for ascending order)"
  (if (null lst)
      nil
      (let ((key-values (mapcar key lst)))  ; Apply key once to minimize calls
        (bubble-sort-hof-recursive lst key-values test))))

(defun bubble-sort-hof-recursive (lst key-values test)
  "Main recursive sorting with cached key values."
  (let ((pass-result (bubble-pass-hof lst key-values nil nil nil test)))  ; Added missing nil for swapped
    (if (first pass-result)  ; if swap occurred
        (bubble-sort-hof-recursive (second pass-result) 
                                   (third pass-result) 
                                   test)
        (second pass-result))))  ; no swaps - sorted

(defun bubble-pass-hof (lst keys acc-lst acc-keys swapped test)
  "Single pass using high-order approach with cached keys.
   Returns (swapped new-list new-keys)."
  (cond
    ((null lst) 
     (list swapped 
           (reverse acc-lst)
           (reverse acc-keys)))
    ((null (cdr lst)) 
     (list swapped 
           (reverse (cons (car lst) acc-lst))
           (reverse (cons (car keys) acc-keys))))
    ((funcall test (car keys) (cadr keys))  ; Compare using cached key values
     ;; Swap elements and their keys
     (bubble-pass-hof (cons (car lst) (cddr lst))
                      (cons (car keys) (cddr keys))
                      (cons (cadr lst) acc-lst)
                      (cons (cadr keys) acc-keys)
                      t  ; set swapped to true
                      test))
    (t  ; No swap needed
     (bubble-pass-hof (cdr lst)
                      (cdr keys)
                      (cons (car lst) acc-lst)
                      (cons (car keys) acc-keys)
                      swapped
                      test))))

;;; TESTING FUNCTIONS WITH OUTPUT

(defun test-hof-sorting ()
  "Test sorting with key and test parameters - shows before and after."
  (format t "~%===== HOF BUBBLE SORT TESTS =====~%")
  
  ;; Test 1: Basic numbers (ascending)
  (let ((input '(3 1 4 1 5 9 2 6)))
    (format t "~%Test 1: Basic numbers (ascending)~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input))
    (format t "  Expected: (1 1 2 3 4 5 6 9)~%"))
  
  ;; Test 2: Sort by absolute value
  (let ((input '(-5 3 -1 4 -2)))
    (format t "~%Test 2: Sort by absolute value~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :key #'abs))
    (format t "  Expected: (-1 -2 3 4 -5)~%"))
  
  ;; Test 3: Descending order
  (let ((input '(3 1 4 1 5)))
    (format t "~%Test 3: Descending order~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :test #'<))
    (format t "  Expected: (5 4 3 1 1)~%"))
  
  ;; Test 4: Strings by length
  (let ((input '("cat" "a" "elephant" "dog" "bird")))
    (format t "~%Test 4: Strings by length~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :key #'length))
    (format t "  Expected: (a cat dog bird elephant)~%"))
  
  ;; Test 5: Complex objects (pairs sorted by CDR)
  (let ((input '((john . 25) (mary . 30) (bob . 20) (alice . 22))))
    (format t "~%Test 5: People by age (pairs by CDR)~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :key #'cdr))
    (format t "  Expected: ((BOB . 20) (ALICE . 22) (JOHN . 25) (MARY . 30))~%"))
  
  ;; Test 6: Already sorted list
  (let ((input '(1 2 3 4 5)))
    (format t "~%Test 6: Already sorted~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input))
    (format t "  Expected: (1 2 3 4 5)~%"))
  
  ;; Test 7: Reverse sorted
  (let ((input '(5 4 3 2 1)))
    (format t "~%Test 7: Reverse sorted~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input))
    (format t "  Expected: (1 2 3 4 5)~%"))
  
  ;; Test 8: Empty list
  (let ((input nil))
    (format t "~%Test 8: Empty list~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input))
    (format t "  Expected: NIL~%"))
  
  ;; Test 9: Single element
  (let ((input '(42)))
    (format t "~%Test 9: Single element~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input))
    (format t "  Expected: (42)~%"))
  
  ;; Test 10: Lists sorted by first element
  (let ((input '((3 a) (1 b) (4 c) (1 d) (5 e))))
    (format t "~%Test 10: Lists by first element~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :key #'car))
    (format t "  Expected: ((1 B) (1 D) (3 A) (4 C) (5 E))~%")))

;;; Run the tests
(test-hof-sorting)



;;; Lab 4 - Part 2
;;; Variant 8: remove-each-rnth-reducer

(defun remove-each-rnth-reducer (n &key key)
  "Returns a function for use with reduce that removes each n-th element 
   when traversing from the end, where key returns non-nil."
  (let ((counter 0))
    (lambda (item acc)
      ;; При from-end t: елементи обробляються справа наліво
      (cond
        ;; Якщо немає key - рахуємо всі елементи
        ((null key)
         (incf counter)
         (if (zerop (mod counter n))
             acc  ; Пропускаємо n-й елемент
             (cons item acc)))  ; Зберігаємо елемент
        ;; Якщо є key - рахуємо лише ті, для яких key повертає true
        ((funcall key item)
         (incf counter) 
         (if (zerop (mod counter n))
             acc  ; Пропускаємо n-й відповідний елемент
             (cons item acc)))  ; Зберігаємо елемент
        ;; Елемент не відповідає key - завжди зберігаємо
        (t (cons item acc))))))


;;; TESTING FUNCTIONS

(defun test-remove-each-rnth ()
  "Tests for remove-each-rnth-reducer function."
  (format t "~%===== REMOVE-EACH-RNTH-REDUCER TESTS =====~%")
  
  ;; Test 1: Remove every 2nd element (no key)
  (let ((input '(1 2 3 4 5)))
    (format t "~%Test 1: Remove every 2nd element (counting from end)~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (1 3 5)~%"))
  
  ;; Test 2: Remove every 2nd even number from end
  (let ((input '(1 2 2 2 3 4 4 4 5)))
    (format t "~%Test 2: Remove every 2nd even number (from end)~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Note: Counting evens from end: 4, 4, 4, 2, 2, 2 -> remove 4, 2, 2~%")
    (let ((result (reduce (remove-each-rnth-reducer 2 :key #'evenp)
                          input
                          :from-end t
                          :initial-value nil)))
      (format t "  Result:   ~A~%" result)
      (format t "  Expected: (1 2 3 4 4 5)~%")))
  
  ;; Test 3: Remove every 3rd element
  (let ((input '(a b c d e f g h i)))
    (format t "~%Test 3: Remove every 3rd element~%")
    (format t "  Input:    ~A~%" input)
    (format t "  From end: I(1), H(2), G(3-remove), F(4), E(5), D(6-remove), C(7), B(8), A(9-remove)~%")
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 3)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (B C E F H I)~%"))
  
  ;; Test 4: Remove every element (n=1)
  (let ((input '(1 2 3 4 5)))
    (format t "~%Test 4: Remove every element (n=1)~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 1)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: NIL~%"))
  
  ;; Test 5: With key that matches nothing
  (let ((input '(1 3 5 7 9)))
    (format t "~%Test 5: Remove every 2nd even (no evens in list)~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2 :key #'evenp)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (1 3 5 7 9)~%"))
  
  ;; Test 6: Empty list
  (let ((input nil))
    (format t "~%Test 6: Empty list~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: NIL~%"))
  
  ;; Test 7: Remove every 2nd number > 3
  (let ((input '(1 2 3 4 5 6 7 8)))
    (format t "~%Test 7: Remove every 2nd number > 3~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Numbers >3 from end: 8(1), 7(2-remove), 6(3), 5(4-remove), 4(5)~%")
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2 :key (lambda (x) (> x 3)))
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (1 2 3 4 6 8)~%")))
  
  ;; Test 8: Complex - remove every 3rd string by length > 1
  (let ((input '("a" "bb" "c" "ddd" "e" "fff" "g")))
    (format t "~%Test 8: Remove every 3rd string with length > 1~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 3 :key (lambda (s) (> (length s) 1)))
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (a c ddd e fff g)~%")))

;;; Run tests
(test-remove-each-rnth)