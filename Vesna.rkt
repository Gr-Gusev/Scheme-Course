; "Весна". Осень 2021
#lang scheme

; подключаем функции для работы с векторами и мутируемыми списками
(require racket/vector)
(require scheme/mpair)


; ----------------------------------------------------------------------
; блок функций для работы с n-граммами и хранящей их структурой-таблицей
; ----------------------------------------------------------------------

; конструктор таблицы n-грамм
(define (make-n-gram-table)
    (cons 'n-gram-table (vector '() '() '()))
)

; селекторы таблицы n-грамм
; получить список n-грамм, стоящих в начале предложений
(define (get-begin-n-gram-list table) (vector-ref (cdr table) 0))

; получить список n-грамм, стоящих в середине предложений
(define (get-middle-n-gram-list table) (vector-ref (cdr table) 1))

; получить список n-грамм, стоящих в конце предложений
(define (get-end-n-gram-list table) (vector-ref (cdr table) 2))


; конструктор структуры n-граммы
(define (make-n-gram-structure n-gram) (vector n-gram '() '()))

; селекторы структуры n-граммы:
; получить n-грамму
(define (get-n-gram structure) (vector-ref structure 0))

; получить слова-предшественники n-граммы
(define (get-n-gram-pred-word-list structure) (vector-ref structure 1))

; получить слова-последователи n-граммы
(define (get-n-gram-succ-word-list structure) (vector-ref structure 2))

; получить частоту n-граммы
; если n-граммы в таблице нет, вернуть #f
(define (get-n-gram-frequency table position n-gram)
    (let loop ((lst (vector-ref (cdr table) position)))
        (if (null? lst)
            #f
            (if (equal? n-gram (get-n-gram (mcar (car lst))))
                (mcdr (car lst))
                (loop (cdr lst))
            )
        )
    )
)

; получить частоту n-граммы - начала предложения
(define (get-begin-n-gram-frequency table n-gram)
    (get-n-gram-frequency table 0 n-gram)
)

; получить частоту n-граммы из середины предложения
(define (get-middle-n-gram-frequency table n-gram)
    (get-n-gram-frequency table 1 n-gram)
)

; получить частоту n-граммы - конца предложения
(define (get-end-n-gram-frequency table n-gram)
    (get-n-gram-frequency table 2 n-gram)
)

; найти n-грамму в таблице, вернуть структуру n-граммы
; если n-граммы в таблице нет, вернуть #f
(define (find-n-gram table position n-gram)
    (let loop ((lst (vector-ref (cdr table) position)))
        (if (null? lst)
            #f
            (if (equal? n-gram (get-n-gram (mcar (car lst))))
                (mcar (car lst))
                (loop (cdr lst))
            )
        )
   )
)

; найти n-грамму - начало предложения в таблице, вернуть структуру n-граммы
(define (find-begin-n-gram table n-gram)
    (find-n-gram table 0 n-gram)
)

; найти n-грамму из середины предложения в таблице, вернуть структуру n-граммы
(define (find-middle-n-gram table n-gram)
    (find-n-gram table 1 n-gram)
)

; найти n-грамму - конец предложения в таблице, вернуть структуру n-граммы
(define (find-end-n-gram table n-gram)
    (find-n-gram table 2 n-gram)
)

; найти n-грамму в таблице
(define (find-any-n-gram table n-gram)
    (or
        (find-begin-n-gram table n-gram)
        (find-middle-n-gram table n-gram)
        (find-end-n-gram table n-gram)
    )
)

; найти какую-либо n-грамму из середины предложения с заданным словом, вернуть структуру n-граммы
; если таковой нет, вернуть #f
(define (find-middle-n-gram-with-word table word)
    (let loop ((lst (get-middle-n-gram-list table)))
        (if (null? lst)
            #f
            (if (member word (get-n-gram (mcar (car lst))))
                (mcar (car lst))
                (loop (cdr lst))
            )
        )
    )
)

; добавить n-грамму в таблицу, вернуть структуру n-граммы
; если n-грамма уже есть в таблице, увеличить на 1 частоту её встречаемости
(define (add-n-gram! table position n-gram)
    (let loop ((lst (vector-ref (cdr table) position)))
        (if (null? lst)
            (begin
                (vector-set!
                    (cdr table)
                    position
                    (cons (mcons (make-n-gram-structure n-gram) 1) (vector-ref (cdr table) position))
                )
                (mcar (car (vector-ref (cdr table) position)))
            )
            (if (equal? n-gram (get-n-gram (mcar (car lst))))
                (begin
                    (set-mcdr! (car lst) (add1 (mcdr (car lst))))
                    (mcar (car lst))
                )
                (loop (cdr lst))
            )
        )
    )
)

; добивать n-грамму-начало предложения в таблицу, вернуть структуру n-граммы
(define (add-begin-n-gram! table n-gram)
    (add-n-gram! table 0 n-gram)
)

; добивать n-грамму из середины предложения в таблицу, вернуть структуру n-граммы
(define (add-middle-n-gram! table n-gram)
    (add-n-gram! table 1 n-gram)
)

; добивать n-грамму-конец предложения в таблицу, вернуть структуру n-граммы
(define (add-end-n-gram! table n-gram)
    (add-n-gram! table 2 n-gram)
)

; добавить слово (предшественник или последователь) к n-грамме
; если слово уже есть, увеличить на 1 частоту его встречаемости
(define (add-word-to-n-gram! structure pos word)
    (let loop ((lst (vector-ref structure pos)))
        (if (null? lst)
            (vector-set! structure pos (cons (mcons word 1) (vector-ref structure pos)))
            (if (equal? (mcar (car lst)) word)
                (set-mcdr! (car lst) (add1 (mcdr (car lst))))
                (loop (cdr lst))
            )
        )
   )
)

; добавить слово-предшественник к n-грамме
; если слово уже есть, увеличить на 1 частоту его встречаемости
(define (add-n-gram-pred-word! structure word)
    (add-word-to-n-gram! structure 1 word)
)

; добавить слово-последователь к n-грамме
; если слово уже есть, увеличить на 1 частоту его встречаемости
(define (add-n-gram-succ-word! structure word)
    (add-word-to-n-gram! structure 2 word)
)

; установить список слов-предшественников n-граммы
(define (set-n-gram-pred-word-list! structure lst)
    (vector-set! structure 1 lst)
)

; установить список слов-последователей n-граммы
(define (set-n-gram-succ-word-list! structure lst)
    (vector-set! structure 2 lst)
)

; установить частоту n-граммы
(define (set-n-gram-frequency! table n-gram position frequency)
    (let loop ((lst (vector-ref (cdr table) position)))
        (if (null? lst)
            (error "Error from set-n-gram-frequency!: n-gram not found")
            (if (equal? (vector-ref (mcar (car lst)) 0) n-gram)
                (set-mcdr! (car lst) frequency)
                (loop (cdr lst))
            )
        )
   )
)

; --------------------------------------------------------------------
; блок функций для работы со строками, предложениями, вводом и выводом
; --------------------------------------------------------------------

; предложение - это список символов

; список пунктуационных символов
(define punctuation-list (string->list ".,?!-'’:;%\""))

; функция-предикат проверки, является ли слово (тип symbol) концом предложения
; - точкой, вопросительным или восклицательным знаком
(define (is-sentence-end? word)
    (or
        (equal? word '\.)
        (equal? word '?)
        (equal? word '!)
    )
)

; функция удаляет знак препинания в конце предложения, если он есть
(define (delete-sentence-end sentence)
    (if (null? sentence)
        '()
        (let ((sentence (reverse sentence)))
            (if (is-sentence-end? (car sentence))
                (reverse (cdr sentence))
                (reverse sentence)
            )
        )
    )
)

; функция-предикат проверки наличия пунктуационных символов в списке
(define (contains-punctuation? lst)
    (let loop ((lst lst))
        (if (null? lst)
            #f
            (let ((str (symbol->string (car lst))))
                (if (and
                        (= 1 (string-length str))
                        (member (car (string->list str)) punctuation-list)
                    )
                    #t
                    (loop (cdr lst))
                )
            )
         )
    )
)

; найти самое длинное слово в предложении
; если таких несколько, вернуть случайное
(define (get-longest-word sentence)
    (let loop ((sentence sentence) (result '()) (max-length 0))
        (if (null? sentence)
            (pick-random-list result)
            (let ((length (string-length (symbol->string (car sentence)))))
                (cond
                    ((< max-length length)
                     (loop (cdr sentence) (list (car sentence)) length)
                    )
                    ((= max-length length)
                     (loop (cdr sentence) (cons (car sentence) result) max-length)
                    )
                    (else
                     (loop (cdr sentence) result max-length)
                    )
                )
            )
        )
    )
)

; функция-предикат проверки, оканчивается ли предложение точкой, вопросительным или восклицательным знаком
(define (is-whole-sentence? sentence)
    (if (null? sentence)
        #f
        (is-sentence-end? (car (reverse sentence)))
    )
)

; возвращает список предложений, каждое предложение - это список слов (слова имеют тип symbol)
; предложения (кроме, возможно, последнего) оканчиваются точкой, вопросительным или восклицательным знаком
(define (get-sentences text)
    (let loop-by-sentence ((input text) (sentences '()))
        (if (null? input)
            (reverse sentences)
            (let loop-by-word ((input input) (sentence '()))
                (cond
                    ((and (null? input) (null? sentence))
                     (void)
                    )
                    ((and (null? input) (not (null? sentence)))
                     (loop-by-sentence input (cons (reverse sentence) sentences))
                    )
                    ((eof-object? input)
                     (loop-by-sentence '() (cons (reverse (cons '\. sentence)) sentences))
                    )
                    ((is-sentence-end? (car input))
                     (loop-by-sentence (cdr input) (cons (reverse (cons (car input) sentence)) sentences))
                    )
                    (else
                     (loop-by-word (cdr input) (cons (car input) sentence))
                    )
                )
            )
        )
    )
)

; функция удаляет из списка символов все не используемые в письменной речи символы
(define (remove-excess-chars lst)
    (map
        (lambda (x)
            (if
                (or
                    (char-alphabetic? x)
                    (char-numeric? x)
                    (char-whitespace? x)
                    (member x punctuation-list)
                )
                x
                #\space
            )
        )
        lst
    )
)

; функция добавляет пробельные символы в списке символов вокруг символов пунктуации, кроме апострофа и дефиса
(define (separate-punctuation lst)
    (reverse
        (foldl
            (lambda (elem acc)
                (if
                    (and
                        (member elem punctuation-list)
                        (not (equal? elem #\'))
                        (not (equal? elem #\’))
                        (not (equal? elem #\-))
                        (not (equal? elem #\%))
                    )
                    (append (list #\space elem #\space) acc)
                    (cons elem acc)
                )
            )
            '()
            lst
        )
    )
)

; функция присоединяет знаки пунктуации (кроме тире) к словам в предложении, возвращает список строк
(define (join-punctuation sentence)
    (let loop ((sentence sentence) (ret '()))
        (if (and  ; если предложении не менее двух слов
                (not (null? sentence))
                (not (null? (cdr sentence)))
            )
            (if (and  ; если второе слово в предложении - знак пунктуации (кроме тире)
                    (member (car (string->list (symbol->string (cadr sentence)))) punctuation-list)
                    (not (equal? (cadr sentence) '-))
                )
                ; склеить первые два слова (из которых второе - знак пунктуации) в одно:
                (loop (cddr sentence) (cons (string-join (map symbol->string (list (car sentence) (cadr sentence))) "") ret))
                ; иначе - добавить первое из слов к списку результата
                (loop (cdr sentence) (cons (symbol->string (car sentence)) ret))
            )
            (if (null? sentence)
                (reverse ret) ; развернуть список результата и вернуть его
                (reverse (cons (symbol->string (car sentence)) ret))
            )
        )
   )
)

; прочитать строку, разделить её по пробельным символам,
; заменить в полученном списке строковое представление чисел на сами числа
; вернуть список символов и чисел
(define (read-line-with-numbers . input-port)
    (let ((input
              (if (null? input-port)
                  (read-line)
                  (read-line (car input-port))
              ))
        )
        (if (eof-object? input)
            input
            (map
                (lambda (x)
                    (let ((num (string->number x)))
                        (if num num (string->symbol x))
                    )
                )
                (string-split input)
            )
        )
    )
)

; прочитать строку, разделить её на слова и вернуть список слов (слова имеют тип symbol)
; под словами понимаются также знаки пунктуации
; опционально принимает порт ввода, по умолчинию порт ввода стандартный
(define (my-read-line . input-port)
    (let ((input
              (if (null? input-port)
                  (read-line)
                  (read-line (car input-port))
              ))
        )
        (if (eof-object? input)
            input
            (map
                string->symbol
                (string-split (list->string
                    (separate-punctuation (remove-excess-chars
                        (string->list (string-downcase input))
                    ))
                ))
            )
        )
   )
)

; функция выводит на экран предложение, откорректировав пунктуацию и поставив заглавную букву в начале
(define (my-print-line sentence)
    (if (not (null? sentence))
        (let ((str (join-punctuation sentence)))
            (displayln
                (string-join
                    (append
                        (list (string-titlecase (car str))) ; установить заглавную букву в первом слове
                        (cdr str)
                    )
                )
            )
        )
        (newline)
    )
)

; ---------------------------------------------
; блок функций для обработки списков и векторов
; ---------------------------------------------

; вернуть первые pos элементов списка
; если в списке меньше, чем pos элеметнов, вернуть #f 
(define (my-list-take lst pos)
    (let loop ((i 0) (lst lst) (ret '()))
        (if (= i pos)
            (reverse ret)
            (if (null? lst)
                #f
                (loop (add1 i) (cdr lst) (cons (car lst) ret))
            )
        )
    )
)

; вернуть последние pos элементов списка
; если в списке меньше, чем pos элеметнов, вернуть #f 
(define (my-list-take-back lst pos)
    (let ((res (my-list-take (reverse lst) pos)))
        (if res
            (reverse res)
            #f
        )
    )
)

; вернуть хвост списка после первых pos элементов
; если в списке меньше, чем pos элементов, вернуть #f
(define (my-list-tail lst pos)
    (let loop ((i 0) (lst lst))
        (if (= i pos)
            lst
            (if (null? lst)
                #f
                (loop (add1 i) (cdr lst))
            )
        )
   )
)

; выбрать случайный подсписок заданной длины
(define (get-random-sublist lst len)
    (let ((lst-length (length lst)))
        (cond
            ((< lst-length len) '())
            ((= lst-length len) lst)
            (else
             (my-list-take (my-list-tail lst (random (add1 (- lst-length len)))) len)
            )
        )
    )
)

; построчно напечатать список
(define (print-list lst output)
    (let loop-by-word ((lst lst))
        (if (null? lst)
            (displayln "" output)
            (begin
                (display (car lst) output)
                (display " " output)
                (loop-by-word (cdr lst))
            )
        )
    )
)

; построчно напечатать список мутируемых пар
(define (print-word-with-weight-list lst output)
    (let loop-by-word ((lst lst))
        (if (null? lst)
            (void)
            (begin
                (display (mcar (car lst)) output)
                (display " " output)
                (displayln (mcdr (car lst)) output)
                (loop-by-word (cdr lst))
            )
        )
    )
)

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vector)
    (if (equal? #() vector)
        '()
        (vector-ref vector (random (vector-length vector)))
    )
)

; случайный выбор одного из элементов списка lst
(define (pick-random-list lst)
    (if (null? lst)
        '()
        (list-ref lst (random (length lst)))
    )
)

; посчитать сумму всех весов элементов списка
(define (sum-weights lst)
    (if (null? lst)
        0
       (let ((tmp-cdr
             (if (mpair? (car lst))
                  mcdr
                  cdr
             )))
            (foldl
                (lambda (elem acc) (+ acc (tmp-cdr elem)))
                0
                lst
            )
        )
    )
)

; случайные выбор одного из элементов списка lst с учётов весов
; каждый элемент списка -- пара: значение и его вес
; возвращает значение из пары
(define (pick-random-with-weight lst)
    (let ((rand (random  ; rand -- случайное число из полуинтервала [0, <сумма весов элементов>)
                    (sum-weights lst)
                 ))
         (tmp-car (if (mpair? (car lst))
                      mcar
                      car
                  ))
         (tmp-cdr (if (mpair? (car lst))
                      mcdr
                      cdr
                  ))
        )
        (let loop ((sum 0) (lst lst))
            (if (<= (+ sum (tmp-cdr (car lst))) rand)
                (loop (+ sum (tmp-cdr (car lst))) (cdr lst))
                (tmp-car (car lst))
            )
        )
    )
)

; ---------------------------------
; блок функций для обучения Доктора
; ---------------------------------

; длина (N-1)-грамм
;(define N-1 4)

; напечатать n-грамму
(define (print-n-gram n-gram frequency output)
    (begin
        (writeln 'n-gram output)
        (print-list (vector-ref n-gram 0) output)
                    
        (writeln frequency output)
                    
        (writeln 'pred-words-list output)
        (print-word-with-weight-list (vector-ref n-gram 1) output)
        (writeln 'end-of-word-list output)
                    
        (writeln 'succ-words-list output)
        (print-word-with-weight-list (vector-ref n-gram 2) output)
        (writeln 'end-of-word-list output)
     )
)

; напечатать список n-грамм
(define (print-n-gram-list lst output)
    (let loop-by-n-gram ((lst lst))
        (if (not (null? lst))
            (begin
                (print-n-gram (mcar (car lst)) (mcdr (car lst)) output)
                (loop-by-n-gram (cdr lst))
            )
            (print-list (list 'n-gram-list-end) output)
        )
    )
)

; сохранить результаты обучения в файл
; файл перезаписывается целиком
(define (safe-train-data file-name table)
    (define output (open-output-file file-name #:exists 'truncate))
    (begin
        (writeln 'n-gram-table output)
  
        (print-list (list 'n-gram-list 0) output)
        (print-n-gram-list (get-begin-n-gram-list table) output)
    
        (print-list (list 'n-gram-list 1) output)
        (print-n-gram-list (get-middle-n-gram-list table) output)
    
        (print-list (list 'n-gram-list 2) output)
        (print-n-gram-list (get-end-n-gram-list table) output)

        (close-output-port output)
    )
)

; прочитать список слов и их весов
; вернуть список мутируемых пар
(define (read-word-with-weight-list input)
    (let loop ((line (read-line-with-numbers input)) (result '()))
        (if (equal? 'end-of-word-list (car line))
            (reverse result)
            (if (or (not (= 2 (length line))) (not (number? (cadr line))))
                (error "Error from read-word-with-weight-list: uncorrect file")
                (loop
                    (read-line-with-numbers input)
                    (cons (mcons (car line) (cadr line)) result)
                )
            )
        )
    )
)

; прочитать список n-грамм заданной позиции
; добавить прочитанные n-граммы в таблицу
(define (read-n-gram-list! table position input)
    (define error-message "Error from read-n-gram-list: uncorrect file")
    (let loop-by-n-gram ((header (read-line-with-numbers input)))
        (if (equal? 'n-gram-list-end (car header))
            (void)
            (if (not (equal? 'n-gram (car header)))
                (error error-message)
                (let ((structure (add-n-gram! table position (my-read-line input))))
                    (begin
                        (set-n-gram-frequency!
                            table
                            (get-n-gram structure)
                            position
                            (car (read-line-with-numbers input))
                        )
                        (if (equal? 'pred-words-list (car (read-line-with-numbers input)))
                            (set-n-gram-pred-word-list! structure (read-word-with-weight-list input))
                            (error error-message)
                        )
                        (if (equal? 'succ-words-list (car (read-line-with-numbers input)))
                            (set-n-gram-succ-word-list! structure (read-word-with-weight-list input))
                            (error error-message)
                        )
                        (loop-by-n-gram (read-line-with-numbers input))
                    )
                )
            )
        )
    )
)

; прочитать результаты обучения из файла, заполнить ими таблицу
(define (read-train-data! file-name table)
    (define input (open-input-file file-name))
    (define error-message "Error from read-train-data: uncorrect file")
    (if (not (equal? '(n-gram-table) (my-read-line input)))
        (error error-message)
        (let loop-by-n-gram-list ((lst (read-line-with-numbers input)))
            (begin
                (if (not (equal? 'n-gram-list (car lst)))
                    (error error-message)
                    (read-n-gram-list! table (cadr lst) input)
                )
                (if (< (cadr lst) 2)
                    (loop-by-n-gram-list (read-line-with-numbers input))
                    (close-input-port input)
                )
            )
        )
    )
)

; выделить из предложения n-граммы и слова-последователи и предшественники, добавить их в таблицу n-грамм
(define (train-on-sentence! table sentence)
    (let loop ((n-gram (my-list-take sentence N-1)) (pred null) (tail (my-list-tail sentence N-1)) (part 'begin))
        (cond
            ((and (equal? part 'begin) n-gram) ; n-грамма в начале предложения
             (if (null? tail)
                 (begin
                     (add-begin-n-gram! table n-gram)
                     (add-end-n-gram! table n-gram)
                 )
                 (begin
                     (let ((structure (add-begin-n-gram! table n-gram)))
                         (add-n-gram-succ-word! structure (car tail))
                     )
                    (loop (append (cdr n-gram) (list (car tail))) (car n-gram) (cdr tail) 'middle)
                 )
             )
            )
            ((and (equal? part 'middle) (not (null? tail)))  ; n-грамма в середине предложения
             (let ((structure (add-middle-n-gram! table n-gram)))
                 (begin
                     (add-n-gram-pred-word! structure pred)
                     (add-n-gram-succ-word! structure (car tail))
                 )
             )
             (loop (append (cdr n-gram) (list (car tail))) (car n-gram) (cdr tail) 'middle)
            )
            ((and (equal? part 'middle) (null? tail))  ; n-грамма в конце предложения
             (let ((structure (add-end-n-gram! table n-gram)))
                 (add-n-gram-pred-word! structure pred)
             )
            )
        )
   )
)

; обучить Доктора - извлечь n-граммы из файла, добавить их в таблицу n-грамм
(define (train! file-name table)
    (define input (open-input-file file-name)) ; открыть файл для чтения
    (let loop-by-line ((line (my-read-line input))) ; цикл по строкам файла
        (if (eof-object? line)
            (close-input-port input) ; файл прочитан целиком
            (let loop-by-sentence ((sentences (get-sentences line))) ; цикл по предложениям в строке
                (if (null? sentences)
                    (loop-by-line (my-read-line input))
                    ; предложение может быть не полным, тогда его конец находится на следующей строке:
                    (if (is-whole-sentence? (car sentences))
                        (begin
                            (train-on-sentence! table (car sentences))
                            (loop-by-sentence (cdr sentences))
                        )
                       (loop-by-line (append (car sentences) (my-read-line input)))
                    )
                )
            )
        )
    )
)

; -----------------------------
; блок основных функций Доктора
; -----------------------------

; изменённая основная функция, запускающая "Доктора"
; с ограничением на количество принимаемых пациентов и стоп-словом
; параметр name -- имя пациента
(define (visit-doctor-v2 stop-word max-number)
    (define end-phrase '(time to go home \.)) ; заключительная фраза
    (let visit-doctor-loop ((name (ask-patient-name)) (cur-number 1))
        (if (equal? name stop-word)
            (my-print-line end-phrase)   ; выход по стоп-слову
            (begin
                (printf "Hello, ~a!\n" name)
                (my-print-line '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name)
                (if (>= cur-number max-number)
                    (my-print-line end-phrase)     ; приняли последнего пациента
                    (visit-doctor-loop (ask-patient-name) (add1 cur-number))  ; приём следующего пациента
                )
            )
        )
    )
)

; вызов очередного пациента, ввод его имени
(define (ask-patient-name)
    (begin
        (my-print-line '(next!))
        (my-print-line '(who are you?))
        (print '**)
        (let ((name (my-read-line)))
            (if (null? name)
                (ask-patient-name)
                (car name)
            )
        )
    )
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop-v2 name)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let loop ((user-response (get-sentences (my-read-line))) (history #()))  ; history - массив предыдущих реплик пациента
        (cond
            ((null? user-response) ; ответ пациента пуст
                (print '**) ; приглашение ко вводу очередной реплики пациента
                (loop (get-sentences (my-read-line)) history)
            )
            ((equal? user-response (list (list 'goodbye))) ; выход из цикла
                (printf "Goodbye, ~a!\n" name)
                (my-print-line '(see you next week \.))
                (newline)
            )
            (else
                ; Доктор генерирует ответ на одну случайно выбранную реплику из ответа пациента:
                (my-print-line (reply-v2
                                   Strategies
                                   (delete-sentence-end (pick-random-list user-response))
                                   history                                
                                   Dataset
                               )
                )
                (print '**) ; приглашение ко вводу очередной реплики пациента
                ; продолжает цикл, сохраняет реплики:
                (loop
                    (get-sentences (my-read-line))
                    (vector-append history (list->vector (map delete-sentence-end user-response)))
                )
            )
        )
    )
)

; обобщённый reply
(define (reply-v2 strategies user-response history table)
    (let ((strategy (pick-random-with-weight  ; выбрать одну стратегию strategy случайным образом с учётом её веса
              (map  ; построить список пар -- применимая стратегия и её вес
                  (lambda (x) (cons x (strategy-weight x)))
                  (vector->list
                      (vector-filter  ; построить вектор применимых стратегий
                          (lambda (x) ((strategy-predicat x) user-response history table))
                          strategies
                      )
                  )
              )
         )))
         (let ((args (sort (strategy-arguments strategy) symbol<?)))
             (cond
                 ((null? (strategy-arguments strategy)) ((strategy-main-function strategy)))
                 ((equal? args (list 'user-response)) ((strategy-main-function strategy) user-response))
                 ((equal? args (list 'history)) ((strategy-main-function strategy) history))
                 ((equal? args (list 'table)) ((strategy-main-function strategy) table))
                 ((equal? args (list 'table 'user-response)) ((strategy-main-function strategy) table user-response))
             )
         )
    )
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-v3 '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
			(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
			(yourself myself)
			(we you)
			(us you)
			(our your)
			(ours yours)
			(ourselves yourselves)
			(yourselves ourselves)
			(shall will))
                      phrase)
 )

; версия many-replace с функциями высших порядков
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-v3 replacement-pairs lst)
    (map
        (lambda (elem)
            (let ((pat-rep (assoc elem replacement-pairs)))
                (if pat-rep
                    (cadr pat-rep)
                    elem
                )
            )
        )
        lst
    )
)

; --------------------------------------------------------------------------------------------------------------------
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
; --------------------------------------------------------------------------------------------------------------------

(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (are you sure that)
                                       (why do you think that)
                                       (you consider that))
                )
                (append
                    (delete-sentence-end (change-person user-response))
                    '(?)
                )
        )
 )

; -------------------------------------------------------------------------------------------------------------------------
; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
; -------------------------------------------------------------------------------------------------------------------------

(define (hedge)
        (pick-random-vector '#((please go on \.)
                              (many people have the same sorts of feelings \.)
                              (many of my patients have told me the same thing \.)
                              (please continue...)
                              (i understand your doubts \.)
                              (say something else...)
                              (you are not only one who feels that \.))
         )
)

; -------------------------------------------------------------------------------------------------------------------
; 3й способ генерации ответной реплики -- случайный выбор одной из предыдущих реплик пользователя и замена лица в ней
; -------------------------------------------------------------------------------------------------------------------

(define (history-answer history)
        (append
               '(earlier you said that)
               (append (change-person (pick-random-vector history)) (list '\.))
         )
)

; -----------------------------------------------------------------------------------------
; 4й способ генерации ответной реплики -- в зависимости от ключевых слов в реплике пациента
; -----------------------------------------------------------------------------------------

; структура данных с группами ключевых слов и ответных реплик:
(define keywords-structure '#(
    ( ; начало данных 1й группы
        (depressed suicide exams university apathy) ; список ключевых слов 1й группы
        ( ; список шаблонов для составления ответных реплик 1й группы 
            (when you feel depressed, go out for ice cream \.)
            (depression is a disease that can be treated \.)
            (there is a way out of any situation \.)
            (let's look at the situation from another view \.)
        )
    ) ; завершение данных 1й группы
    ( ; начало данных 2й группы ...
        (mother father parents brother sister uncle ant grandma grandpa)
        (
            (tell me more about your * , i want to know all about your * \.)
            (why do you feel that way about your * ?)
            (how do you estimate your relationship with your * ?)
            (what did your * say you ?)
        )
    )
    (
        (university scheme lections)
        (
            (your education is important \.)
            (how many time do you spend to learning ?)
            (do you take brakes during your learning ?)
            (what is the problem with * ?)
        )
    )
   (
       (relationship friend friends)
       (
           (tell me about your *)
           (do you have friends ?)
           (how often do you meet with your friend ?)
           (do you feel comfortable with your friends ?)
       )
   )
   (
       (sadness apathy melancholy despondency depressed)
       (
           (why do you feel * ?)
           (what do you do when you feel * ?)
           (try to walk or watch a film when you feel *)
           (many people fell * in autumn \.)
       )
   )
))

; список всех ключевых слов
(define keyword-list
    (foldl
        (lambda (elem acc)
            (append
                (filter (lambda (x) (not (member x acc))) (car elem))
                acc
            )
        )
        '()
        (vector->list keywords-structure)
    )
)

; функция-предикат проверки наличия ключевых слов в реплике пациента
(define (has-keywords? response)
    (ormap (lambda (x) (is-keyword? x)) response)
)

; функция-предикат проверки является ли слово ключевым
(define (is-keyword? word)
    (member word keyword-list)
)

; построение списка всех ключевых слов в реплике пациента
(define (get-keywords response)
    (filter (lambda (x) (is-keyword? x)) response)
)

; построение списка всех возможных шаблонов для ответа по данному ключевому слову
(define (get-templates keyword)
    (foldl append '()
        (map
            (lambda (x)
                (if (member keyword (car x))
                    (cadr x)
                    '()
                )
            )
            (vector->list keywords-structure)
        )
    )
)

; генератор ответной реплики по ключевым словам
(define (keyword-answer response)
    (let ((keyword (pick-random-list (get-keywords response))))  ; keyword - ключевое слово, по которому строится ответ
        (many-replace-v3     ; заменить * на keyword
            (list (list '* keyword))
            (pick-random-list (get-templates keyword))  ; шаблон для ответа
        )
    )
)

; -----------------------------------------------------------------------------
; 5й способ генерации ответной реплики -- прямое построение ответа по n-граммам
; -----------------------------------------------------------------------------

; выбрать слово-последователь для заданной части ответа
; учитывается длина получаемого ответа - чем он длиннее, тем с большей вероятностью полученное предложение будет объявлено ответом
(define (n-gram-answer-get-succ-word response n-gram-struct)
    (let ((succ-word-list (if n-gram-struct (get-n-gram-succ-word-list n-gram-struct) '())))
        (if (is-whole-sentence? response)
            null
            (let ((ends (filter (lambda (x) (is-sentence-end? (mcar x))) succ-word-list))
                  (no-ends (filter (lambda (x) (not (is-sentence-end? (mcar x)))) succ-word-list))
                 )
                 (if (<= 0
                     (random
                         (- (sum-weights no-ends))
                         (if (null? ends)
                             0
                             (+ (sum-weights ends) (inexact->exact (round (* 0.3 (length response)))))
                         )
                     )
                     )
                     (pick-random-with-weight ends)
                     (pick-random-with-weight no-ends)
                 )
            )
        )
    )
)

; построить ответ по данной n-грамме, добавляя слова в конец предложения
(define (build-straight-n-gram-answer table n-gram-struct)
    (let loop ((response (get-n-gram n-gram-struct)) (n-gram-struct n-gram-struct))
        (let ((succ-word (n-gram-answer-get-succ-word response n-gram-struct)))
            (if (null? succ-word)
                response
                (loop
                    (append response (list succ-word))
                    (find-middle-n-gram table (my-list-take-back (append response (list succ-word)) N-1))
                )
            )
        )
    )
)

; основная функция стратегии
; случайным образом с учётом весов выбирается n-грамма - начало преложения и достраивается ответ
(define (straight-n-gram-answer table)
    (let ((n-gram-struct (pick-random-with-weight (get-begin-n-gram-list table))))
        (build-straight-n-gram-answer table n-gram-struct)
    )
)

; -------------------------------------------------------------------------------
; 6й способ генерации ответной реплики -- обратное построение ответа по n-граммам
; -------------------------------------------------------------------------------

; выбрать слово-предшественник для заданной части ответа
; учитывается длина получаемого ответа - чем он длиннее, тем с большей вероятностью полученное предложение будет объявлено ответом
(define (n-gram-answer-get-pred-word response n-gram-struct table)
    ; функция-предикат проверки, не пора ли заканчивать предлложение
    ; оценивает длину постоенного ответа и частотность n-граммы из начала посторенного ответа как n-граммы - начала предложения
    ; и как n-граммы из середины предложения
    (define (is-time-to-stop? response pred-word-list)
        (let ((begin-n-gram-frequency (get-begin-n-gram-frequency table (my-list-take response N-1))))
            (if begin-n-gram-frequency
                (<= 0
                    (random
                        (- (sum-weights pred-word-list))
                        (+ begin-n-gram-frequency (inexact->exact (round (* 0.3 (length response)))))
                    )
                )
                #f
            )
        )
    )
    (let ((pred-word-list (if n-gram-struct (get-n-gram-pred-word-list n-gram-struct) '())))
        (cond
            ((null? pred-word-list) null)
            ((is-time-to-stop? response pred-word-list) null)
            (else (pick-random-with-weight pred-word-list))
        )
    )
)

; построить ответ по данной n-грамме, добавляя слова в начало предложения
(define (build-back-n-gram-answer table n-gram-struct)
    (let loop ((response (get-n-gram n-gram-struct)) (n-gram-struct n-gram-struct))
        (let ((pred-word (n-gram-answer-get-pred-word response n-gram-struct table)))
            (if (null? pred-word)
                response
                (loop
                    (cons pred-word response)
                    (find-middle-n-gram table (my-list-take (cons pred-word response) N-1))
                )
            )
        )
    )
)

; основная функция стратегии
; случайным образом с учётом весов выбирается n-грамма - конец предложения и достраивается ответ
(define (back-n-gram-answer table)
    (let ((n-gram-struct (pick-random-with-weight (get-end-n-gram-list table))))
        (build-back-n-gram-answer table n-gram-struct)
    )
)

; ------------------------------------------------------------------------------
; 7й способ генерации ответной реплики -- построение ответа в обоих направлениях
; ------------------------------------------------------------------------------

; по данной n-грамме достроить ответ в обоих направлениях
(define (build-back-straight-answer table n-gram-struct)
    (let ((resp-back (build-back-n-gram-answer table n-gram-struct))
          (resp-straigth (build-straight-n-gram-answer table n-gram-struct))
         )
         (append resp-back (my-list-tail resp-straigth N-1))
    )
)

; построить ответ на основе n-граммы из реплики пациента,
; в которой произведена замена лиц в местоимениях и глаголах
(define (mixed-answer-with-sublist user-response table)
    (let loop ((sublist (my-list-take user-response N-1)) (lst user-response))
        (if (or (not sublist) (null? sublist))
            '()
            (if (not (contains-punctuation? sublist))
                (let ((n-gram-struct (find-middle-n-gram table (change-person sublist))))
                    (if n-gram-struct
                        (build-back-straight-answer table n-gram-struct)
                        (loop (my-list-take (cdr lst) N-1) (cdr lst))
                    )
                )
                (loop (my-list-take (cdr lst) N-1) (cdr lst))
            )
        )
    )
)

; построить ответ на основе n-граммы, в которой присутствует одно из самых длинных слов в реплике пациента
(define (mixed-answer-with-longest-word user-response table)
    (let ((word (get-longest-word user-response)))
        (let ((n-gram-struct (find-middle-n-gram-with-word table word)))
            (if (not n-gram-struct)
                '()
                (build-back-straight-answer table n-gram-struct)
            )
       )
   )
)

; основная функция стратегии
; строит ответ в двух направлениях на основе n-граммы из реплики пациента,
; n-граммы, содержащей самое длинное слово из реплики пациента
; или случайной n-граммы - середины предложения
(define (mixed-n-gram-answer table user-response)
    (or
        (let ((ret (mixed-answer-with-sublist user-response table)))
            (if (null? ret) #f ret)
        )
        (let ((ret (mixed-answer-with-longest-word user-response table)))
            (if (null? ret) #f ret)
        )
        (let ((n-gram-struct (pick-random-with-weight (get-middle-n-gram-list table))))
            (build-back-straight-answer table n-gram-struct)
        )
    )
)

; -----------------------------------------------------------------
; блок функций для работы со стратегиями построения ответов Доктора
; -----------------------------------------------------------------

; функция-конструктор структуры стратегии
; структура стратегии -- это вектор, хранящий
;     функцию-предикат проверки применимости стратегий,
;     вес стратегии,
;     основную функцию -- постоение ответа по данной стратегии,
;     список требуемых аргементов для функции, возможные элементы: 'user-response и 'history
(define (make-strategy-structure predicat weight main-function arguments)
    (vector predicat weight main-function arguments)
)

; функции-селекторы для структуры стратегии
(define (strategy-predicat strategy)
     (vector-ref strategy 0)
)

(define (strategy-weight strategy)
     (vector-ref strategy 1)
)

(define (strategy-main-function strategy)
     (vector-ref strategy 2)
)

(define (strategy-arguments strategy)
     (vector-ref strategy 3)
)

; структура данных со всеми стратегиями ответа Доктора -- вектор из структур стратегий
(define Strategies
    (vector
        (make-strategy-structure (lambda (response history table) #t) 1 hedge '())
        (make-strategy-structure (lambda (response history table) (not (equal? #() history))) 2 history-answer (list 'history))
        (make-strategy-structure (lambda (response history table) #t) 3 qualifier-answer (list 'user-response))
        (make-strategy-structure (lambda (response history table) (has-keywords? response)) 3 keyword-answer (list 'user-response))
        (make-strategy-structure (lambda (response history table)
                                   (not (null? (get-begin-n-gram-list table))))
                                 5
                                 straight-n-gram-answer (list 'table)
        )
        (make-strategy-structure (lambda (response history table)
                                   (not (null? (get-end-n-gram-list table))))
                                 5
                                 back-n-gram-answer (list 'table)
        )
        (make-strategy-structure (lambda (response history table)
                                   (not (null? (get-middle-n-gram-list table))))
                                 10
                                 mixed-n-gram-answer (list 'table 'user-response)
        )
    )
)


; параметр - длина n-грамм
(define N-1 3)

; таблица для хранения n-грамм
(define Dataset (make-n-gram-table))

; обучить Доктора на подготовленном тексте
;(train! "covid_impact.txt" Dataset)

; сохранить результат обучения в файл
; (safe-train-data "train_data_saved.txt" Dataset)

; прочитать данные с результатом обучения из файла
; (read-train-data! "train_data_saved.txt" Dataset)

; вызвать основную функцию Доктора, стоп слово - stop, число посетителей - 3
; (visit-doctor-v2 'stop 3)
