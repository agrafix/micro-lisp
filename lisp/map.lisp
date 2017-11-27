((lambda
   (map)
   (map (lambda (x) (+ x x)) (quote (1 2 3 4 5 6 7 8 9))))

 (lambda
   (f list)
   ((lambda
      (mapGo)
      (mapGo mapGo list))
    (lambda
      (mapGo^ l)
      (if (null? l)
          l
        (cons
         (f (car l))
         (mapGo^ mapGo^ (cdr l))
         )))
   )))
