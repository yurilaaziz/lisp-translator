;      We are trying to single-handedly solve the problem of Machine Translation.
; MT systems like Systran have been developed by teams of scientists and engineers
; for decades and they are still far from perfect.

; @functions
; (isword input:word)    : true if word is in dictionary
; (fetch input:list)     : translate each word of list
; (wordsfromlist n list) : return the n first words from list 
; (unsetfromlist n list) : unset the n first words from list 
; (getword word)         : return the corresponding word
; (filtre list dict n)   : adjust string to list 

; Supported phrases
;    I love house-music             :      J aime le house-music
;    I like food                    :      J aime la nourriture
;    sharing data means sharing knowledge and sharing culture
;  le partage de données signifie le partage des connaissances et le partage de la culture
(defun dict ()
       (prog (db1)
         (setq db1 '(
                     ((I) (je))
                     ((you) (vous))
                     ((we) (nous))
                     ((she) (elle))
                     ((he) (il))
                     ((it) (il))
; NOUNS
                     ((sharing) (le partage de))
                     ((data) (données))
                     ((knowledge) (connaissances))
                     ((culture) (la culture))
                     ((and) (et))
                     
; VERBS
                     ((means)(signifie))
                     ((mean) (signifient))
                     ((like) (aime))
                     ((money) (argent))
                     ((am) (suis))
                     ((have) (ai))
                     ((save))
                     ((need)(ai besoin de ))
                     ((have to) (dois))
; expressions
                     ((am going to) (vais à))
                     (() (vais à))
                   )
                  )
         
         (return db1)))

(defun getword (w db)
  (cond
   ((null db ) ())
   ((equal (caar db) w) (cadar db))
   (t (getword w (cdr db)))
))

(defun isword (w)
  (not (eq () (getword w (dict)))))


(defun resolv (w)
  (cond 
   ((isword w) (getword w (dict)))
   (t w)
   )
)

;input double list
;output word list
(defun fetch (l) 
   (cond 
    ((null l) ())
    (t (append (resolv (car l)) (fetch (cdr l) )))
   ))



(defun wordsfromlist (n l)
  (cond
   ((= n 0) ()) 
   ((< (length l) n) l)
   (t (cons (car l) (wordsfromlist (1- n) (cdr l))))
))

(defun unsetfromlist (n l)
  (cond 
   ((null l) ())
   ((< (length l) n) ())
   ((= n 0) l)
   (t (unsetfromlist (1- n) (cdr l)))
))
;input list
;output double list

(defun filtre (l db v)
 (cond 
  ((null l) ())
  ((> v 3) (cons (wordsfromlist 1 l) (filtre (unsetfromlist 1 l) db 1)))
  ((isword (wordsfromlist v l)) (cons (wordsfromlist v l) (filtre (unsetfromlist v l) db 1)))
  (t (filtre l db (1+ v)))
))

(defun scanner (str)
  (if (not (streamp str))
    (scanner (make-string-input-stream str))
     (if (listen str)
       (cons (read str) (scanner str))
              nil)))


(defun translate ()
 (fetch (filtre (scanner (read)) (dict) 1))

)
