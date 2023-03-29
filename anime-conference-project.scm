;Emily Torres
;Introduction to Functional Programming
;Anime Database Conference Project

;; anime constructor function
(define make-anime
  (lambda (english-title japanese-title year episodes genres creator)
     (list english-title japanese-title year episodes genres creator)))

;; define anime-database
(load "scheme_anime_database.scm")

;; anime selector functions

(define get-english-title
  (lambda (anime)
    (car anime)))

(define get-japanese-title
  (lambda (anime)
    (cadr anime)))

(define get-year
  (lambda (anime)
    (caddr anime))) 

(define get-episodes
  (lambda (anime)
    (cadddr anime)))  

(define get-genres
  (lambda (anime)
    (car (cddddr anime))))  

(define get-creator
  (lambda (anime)
    (car (cdr (cddddr anime)))))

;;-----------------------------------------------------------------

(define member?
  (lambda (x input-list)
    (cond
      [(null? input-list) #f]
      [(equal? (car input-list) x) #t]
      [else (member? x (cdr input-list))])))

(define filter
  (lambda (predicate? input-list)
    (cond
      [(null? input-list) '()]
      [(predicate? (car input-list))
       (cons (car input-list) (filter predicate? (cdr input-list)))]
      [else (filter predicate? (cdr input-list))])))

;;-----------------------------------------------------------------

(define japanese-title-of
  (lambda (english-title database)
    (cond
      [(null? database) 'unknown]
      [(equal? (get-english-title (car database)) english-title) (get-japanese-title (car database))]
      [else (japanese-title-of english-title (cdr database))])))

;;sample input : (japanese-title-of '(chobits) anime-database)
;;sample answer : (chobittsu)

(define year-released
  (lambda (english-title database)
    (cond
      [(null? database) 'unknown]
      [(equal? (get-english-title (car database)) english-title) (get-year (car database))]
      [else (find-year-released english-title (cdr database))])))

;;sample input : (year-released '(yuri on ice) anime-database)
;;sample answer : 2016

(define num-episodes
  (lambda (english-title database)
    (cond
      [(null? database) 'unknown]
      [(equal? (get-english-title (car database)) english-title) (get-episodes (car database))]
      [else (num-episodes english-title (cdr database))])))

;;sample input : (num-episodes '(food wars) anime-database)
;;sample answer : 86

(define genres-of
  (lambda (english-title database)
    (cond
      [(null? database) 'unknown]
      [(equal? (get-english-title (car database)) english-title) (get-genres (car database))]
      [else (genres-of english-title (cdr database))])))

;;sample input : (genres-of '(assassination classroom) anime-database)
;;sample answer : ((action) (comedy) (school) (shounen))

(define creator-of
  (lambda (english-title database)
    (cond
      [(null? database) 'unknown]
      [(equal? (get-english-title (car database)) english-title) (get-creator (car database))]
      [else (creator-of english-title (cdr database))])))

;;sample input : (creator-of '(i want to eat your pancreas) anime-database)
;;sample answer : (yoru sumino)

;;------------------------------------------------------------------

;;find functions

(define find-anime
  (lambda (predicate? database)
    (cond
      [(null? database) 'unknown]
      [(predicate? (car database)) (car database)]
      [else (find-anime predicate? (cdr database))])))

(define find-japanese-title
  (lambda (english-title database)
    (get-japanese-title (find-anime (lambda (m) (equal? (get-english-title m) english-title)) database))))

;;sample input : (find-japanese-title '(dragonball) anime-database)
;;sample answer : (doragon boru)

(define find-year-released
  (lambda (english-title database)
    (get-year (find-anime (lambda (m) (equal? (get-english-title m) english-title)) database))))

;;sample input : (find-year-released '(fruits basket) anime-database)
;;sample answer : 2001

(define find-num-episodes
  (lambda (english-title database)
    (get-episodes (find-anime (lambda (m) (equal? (get-english-title m) english-title)) database))))

;;sample input : (find-num-episodes '(negima: magister negi magi) anime-database)
;;sample answer : 26

(define find-genres-of
  (lambda (english-title database)
    (get-genres (find-anime (lambda (m) (equal? (get-english-title m) english-title)) database))))

;;sample input : (find-genres-of '(naruto) anime-database)
;;sample answer : ((action) (adventure) (comedy) (martial arts) (shounen))

(define find-creator-of
  (lambda (english-title database)
    (get-creator (find-anime (lambda (m) (equal? (get-english-title m) english-title)) database))))

;;sample input : (find-creator-of '(my ordinary life) anime-database)
;;sample answer : (keiichi arawi)

;;------------------------------------------------------------------

;;animes-with functions

(define animes-with-num-episodes
  (lambda (episodes database)
    (map get-english-title (filter (lambda (anime) (equal? episodes (get-episodes anime))) database))))

;;sample input (for titles) : (animes-with-num-episodes 26 anime-database)
;;sample answer: ((azumanga daioh)
; (cowboy bebop)
; (dororo)
; (maid sama)
; (my ordinary life)
; (negima: magister negi magi)
; (ouran high school host club)
; (school rumble)
; (to love ru))

;;sample input (for length) : (length (animes-with-num-episodes 26 anime-database))
;;sample answer: 9

(define animes-with-year
  (lambda (year database)
    (map get-english-title (filter (lambda (anime) (= year (get-year anime))) database))))

;;sample input (for titles) : (animes-with-year 2004 anime-database)
;;sample answer: ((bleach) (school rumble))

;;sample input (for length) : (length (animes-with-year 2004 anime-database))
;;sample answer: 2

(define animes-with-genre
  (lambda (genre database)
    (map get-english-title (filter (lambda (anime) (member? genre (get-genres anime))) database))))

;;sample input (for titles) : (animes-with-genre '(comedy) anime-database)
;;sample answer: ((angel beats)
; (assassination classroom)
; (azumanga daioh)
; (backstreet girls gokudolls)
; (black butler)
; (bleach)
; (cells at work)
; (chobits)
; (clannad)
; (cowboy bebop)
; (dragonball)
; (dragonball z)
; (fruits basket)
; (fullmetal alchemist)
; (silver soul)
; (dog yaksha)
; (the troubled life of miss kotoura)
; (lucky star)
; (maid sama)
; (my ordinary life)
; (naruto)
; (negima: magister negi magi)
; (one piece)
; (one punch man)
; (ouran high school host club)
; (place to place)
; (potemayo)
; (ranma 1/2)
; (rascal does not dream of bunny girl senpai)
; (rurouni kenshin)
; (school rumble)
; (soul eater)
; (the devil is a part-timer)
; (to love ru)
; (toradora)
; (yuri on ice))

;;sample input (for length) : (length (animes-with-genre '(comedy) anime-database))
;;sample answer: 36

(define animes-with-creator
  (lambda (creator database)
    (map get-english-title (filter (lambda (anime) (equal? creator (get-creator anime))) database))))

;;sample input (for titles) : (animes-with-creator '(osamu tezuka) anime-database)
;;sample answer: ((astro boy) (dororo))

;;sample input (for length) : (length (animes-with-creator '(osamu tezuka) anime-database))
;;sample answer: 2

;;------------------------------------------------------------------

;;find-animes

(define find-animes
  (lambda (predicate? selector database)
   (map get-english-title (filter (lambda (anime) (predicate? anime)) database))))

;;sample input (for titles) : (find-animes (lambda (anime) (= (get-year anime) 2002)) get-english-title anime-database)
;;sample answer: ((azumanga daioh) (chobits) (naruto))

;;sample input (for length) : (length (find-animes (lambda (anime) (= (get-year anime) 2002)) get-english-title anime-database))
;;sample answer: 3

;;-----------------------------------------------------------------

(define matches?
  (lambda (pattern query)
    (cond
      [(and (null? pattern) (null? query)) #t]
      [(or (null? pattern) (null? query)) #f]
      [(equal? (car pattern) '...)
       (if (null? (cdr pattern))
           #t
           (error "... must go at the end of the pattern"))]
      [(equal? (car pattern) '_) (matches? (cdr pattern) (cdr query))]
      [(equal? (car pattern) (car query)) (matches? (cdr pattern) (cdr query))]
      [else #f])))

(define get-all-substitutions
  (lambda (pattern query)
    (cond
      [(not (matches? pattern query)) (error "pattern and query don't match!")]
      ;;
      [(null? pattern) '()]
      [(equal? (car pattern) '...) (list query)]
      [(equal? (car pattern) '_)
       (cons (car query) (get-all-substitutions (cdr pattern) (cdr query)))]
      [else (get-all-substitutions (cdr pattern) (cdr query))])))

;;-----------------------------------------------------------------------
;; the pattern-action database

(define pattern1 '(what animes were made between _ and _))

(define action1
  (lambda (year1 year2)
    (find-animes (lambda (anime)
		   (and (>= (get-year anime) year1)
			(<= (get-year anime) year2)))
                 get-english-title
                 anime-database)))

(define pattern2 '(what animes were made in _))

(define action2
  (lambda (year)
    (find-animes (lambda (anime) (= (get-year anime) year))
                 get-english-title
                 anime-database)))

(define pattern3 '(who is the creator of ...))

(define action3
  (lambda (english-title)
    (find-animes (lambda (anime) (equal? (get-english-title anime) english-title))
                 get-creator
                 anime-database)))

(define pattern4 '(who created ...))

(define action4
  (lambda (english-title)
    (find-animes (lambda (anime) (equal? (get-english-title anime) english-title))
                 get-creator
                 anime-database)))

(define pattern5 '(what animes are categorized as a ...))

(define action5
  (lambda (genre)
    (find-animes (lambda (anime) (member? genre (get-genres anime)))
                 get-english-title
                 anime-database)))

(define pattern-action-database
  (list (list pattern1 action1)
        (list pattern2 action2)
        (list pattern3 action3)
        (list pattern4 action4)
        (list pattern5 action5)))

;;-----------------------------------------------------

(define first-pattern-of
  (lambda (list-of-pattern-actions)
    (car (car list-of-pattern-actions))))

(define first-action-of
  (lambda (list-of-pattern-actions)
    (car (cdr (car list-of-pattern-actions)))))

(define answer-by-pattern
  (lambda (query list-of-pattern-actions)
    (cond
      [(null? list-of-pattern-actions) (display '(i do not understand))]
      [(matches? (first-pattern-of list-of-pattern-actions) query)
       (let ([pattern (first-pattern-of list-of-pattern-actions)]
             [action (first-action-of list-of-pattern-actions)])
         (let ([subs (get-all-substitutions pattern query)])
           (let ([results (apply action subs)])
             (cond
               [(null? results) (display '(i do not know))]
               [(= (length results) 1) (display (car results))]
               [else (display results)]))))]
      [else (answer-by-pattern query (cdr list-of-pattern-actions))])))

;;sample input: (answer-by-pattern '(what animes were made between 2000 and 2010) pattern-action-database)
;;sample answer: ((angel beats) (azumanga daioh) (black butler) (bleach) (chobits) (clannad) (death note) (fruits basket) (fullmetal alchemist)
;(silver soul) (hellsing) (dog yaksha) (lucky star) (maid sama) (naruto) (negima: magister negi magi) (ouran high school host club)
;(potemayo) (school rumble) (soul eater) (to love ru) (toradora) (vampire knight))

;;sample input: (answer-by-pattern '(who created dragonball z) pattern-action-database)
;;sample answer: (akira toriyama)

;;sample input: (answer-by-pattern '(what animes are categorized as a comedy) pattern-action-database)
;;sample answer: ((angel beats) (assassination classroom) (azumanga daioh) (backstreet girls gokudolls) (black butler) (bleach) (cells at work)
;(chobits) (clannad) (cowboy bebop) (dragonball) (dragonball z) (fruits basket) (fullmetal alchemist) (silver soul) (dog yaksha) (the troubled life of miss kotoura) (lucky star) (maid sama)
;(my ordinary life) (naruto) (negima: magister negi magi) (one piece)
;(one punch man) (ouran high school host club) (place to place) (potemayo) (ranma 1/2) (rascal does not dream of bunny girl senpai)
;(rurouni kenshin) (school rumble) (soul eater) (the devil is a part-timer) (to love ru) (toradora) (yuri on ice))
