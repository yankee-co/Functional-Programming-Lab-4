<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Землянський Едуард КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з використанням функцій вищого порядку та додати два ключових параметри: `:key` та `:test`
2. Реалізувати функцію, що створює замикання для роботи з функціями вищого порядку згідно з варіантом

## Варіант першої частини 3

**Алгоритм сортування**: Exchange Sort #2 (Bubble Sort з прапорцем) у порядку зростання

## Лістинг реалізації першої частини завдання

```lisp
;;; FUNCTIONAL IMPLEMENTATION WITH HIGH-ORDER FUNCTIONS

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
  (let ((pass-result (bubble-pass-hof lst key-values nil nil nil test)))
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
```

### Тестові набори та утиліти першої частини

```lisp
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
    (format t "  Expected: (\"a\" \"cat\" \"dog\" \"bird\" \"elephant\")~%"))
  
  ;; Test 5: Complex objects (pairs sorted by CDR)
  (let ((input '((john . 25) (mary . 30) (bob . 20) (alice . 22))))
    (format t "~%Test 5: People by age (pairs by CDR)~%")
    (format t "  Before: ~A~%" input)
    (format t "  After:  ~A~%" (bubble-sort-hof input :key #'cdr))
    (format t "  Expected: ((BOB . 20) (ALICE . 22) (JOHN . 25) (MARY . 30))~%")))
```

### Тестування першої частини

```lisp
CL-USER> (test-hof-sorting)

===== HOF BUBBLE SORT TESTS =====

Test 1: Basic numbers (ascending)
  Before: (3 1 4 1 5 9 2 6)
  After:  (1 1 2 3 4 5 6 9)
  Expected: (1 1 2 3 4 5 6 9)

Test 2: Sort by absolute value
  Before: (-5 3 -1 4 -2)
  After:  (-1 -2 3 4 -5)
  Expected: (-1 -2 3 4 -5)

Test 3: Descending order
  Before: (3 1 4 1 5)
  After:  (5 4 3 1 1)
  Expected: (5 4 3 1 1)

Test 4: Strings by length
  Before: (cat a elephant dog bird)
  After:  (a cat dog bird elephant)
  Expected: ("a" "cat" "dog" "bird" "elephant")

Test 5: People by age (pairs by CDR)
  Before: ((JOHN . 25) (MARY . 30) (BOB . 20) (ALICE . 22))
  After:  ((BOB . 20) (ALICE . 22) (JOHN . 25) (MARY . 30))
  Expected: ((BOB . 20) (ALICE . 22) (JOHN . 25) (MARY . 30))
```

## Варіант другої частини 8

**Завдання**: Написати функцію `remove-each-rnth-reducer`, яка має один основний параметр `n` та один ключовий параметр — функцію `key`. Функція повертає функцію для використання з `reduce`, яка при обході списку з кінця видаляє кожен n-й елемент, для якого функція `key` повертає істину.

## Лістинг реалізації другої частини завдання

```lisp
(defun remove-each-rnth-reducer (n &key key)
  "Returns a function for use with reduce that removes each n-th element 
   when traversing from the end, where key returns non-nil.
   If key is not provided, removes every n-th element."
  (let ((counter 0))  ; Counter for tracking elements - closure variable
    (lambda (item acc)
      ;; When traversing from end with :from-end t
      (cond
        ;; If no key provided, count all elements
        ((null key)
         (incf counter)
         (if (zerop (mod counter n))
             acc  ; Skip this element (it's the n-th)
             (cons item acc)))  ; Keep this element
        ;; If key provided, only count elements where key returns true
        ((funcall key item)
         (incf counter) 
         (if (zerop (mod counter n))
             acc  ; Skip this n-th matching element
             (cons item acc)))  ; Keep this element
        ;; Element doesn't match key, always keep it
        (t (cons item acc))))))
```

### Тестові набори та утиліти другої частини

```lisp
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
  
  ;; Test 2: Remove every 2nd even number
  (let ((input '(1 2 2 2 3 4 4 4 5)))
    (format t "~%Test 2: Remove every 2nd even number (from end)~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2 :key #'evenp)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: (1 2 3 4 4 5)~%"))
  
  ;; Test 3: Remove every 3rd element
  (let ((input '(a b c d e f g h i)))
    (format t "~%Test 3: Remove every 3rd element~%")
    (format t "  Input:    ~A~%" input)
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
  
  ;; Test 5: Empty list
  (let ((input nil))
    (format t "~%Test 5: Empty list~%")
    (format t "  Input:    ~A~%" input)
    (format t "  Result:   ~A~%" 
            (reduce (remove-each-rnth-reducer 2)
                    input
                    :from-end t
                    :initial-value nil))
    (format t "  Expected: NIL~%")))
```

### Тестування другої частини

```lisp
CL-USER> (test-remove-each-rnth)

===== REMOVE-EACH-RNTH-REDUCER TESTS =====

Test 1: Remove every 2nd element (counting from end)
  Input:    (1 2 3 4 5)
  Result:   (1 3 5)
  Expected: (1 3 5)

Test 2: Remove every 2nd even number (from end)
  Input:    (1 2 2 2 3 4 4 4 5)
  Result:   (1 2 3 4 4 5)
  Expected: (1 2 3 4 4 5)

Test 3: Remove every 3rd element
  Input:    (A B C D E F G H I)
  Result:   (B C E F H I)
  Expected: (B C E F H I)

Test 4: Remove every element (n=1)
  Input:    (1 2 3 4 5)
  Result:   NIL
  Expected: NIL

Test 5: Empty list
  Input:    NIL
  Result:   NIL
  Expected: NIL
```
