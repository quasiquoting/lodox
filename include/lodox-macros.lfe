(defmacro doto
  (`(,x . ,forms)
   `(progn
      ,@(lists:map
         (match-lambda
           ([form] (when (is_list form))
            `(,(car form) ,x ,@(cdr form)))
           ([f] `(,f ,x)))
         forms)
      ,x)))
