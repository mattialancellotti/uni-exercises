#lang racket

;;; Controlla se la parola data e' maschile
(define maschile?
  (lambda (parola) (let* ([len (string-length parola)]
                          [idx (substring parola (sub1 len))])
                     (or (string=? idx "i")
                         (string=? idx "o")))))

;;; Controlla se la parola data e' plurale
(define plurale?
  (lambda (parola) (let* ([len (string-length parola)]
                          [idx (substring parola (sub1 len))])
                     (or (string=? idx "i")
                         (string=? idx "e")))))

;;; Sceglie l'articolo corretto per ogni parola data
(define scegli-articolo
  (lambda (parola) (let ([sex (maschile? parola)]
                         [plr (plurale? parola)])
                     (cond
                       [(false? sex) (if (false? plr) "la" "le")]
                       [else (if (false? plr) "il" "i")]))))

;;; Coniuga correttamente il verbo con il plurale/singolare dato
(define crea-verbo
  (lambda (prefisso parola singolare plurale)
    (if (plurale? parola)
      (string-append prefisso plurale)
      (string-append prefisso singolare))))

(define coniuga-verbo
  (lambda (parola verbo) (let* ([lunghezza (string-length verbo)]
                                [suffisso (substring verbo (- lunghezza 3))]
                                [prefisso (substring verbo 0 (- lunghezza 3))])
                           ;; Coniuga il verbo semplicemente controllando se la
                           ;; parola data e' plurale oppure singolare.
                           (cond
                             [(string=? suffisso "are")
                              (crea-verbo prefisso parola "a" "ano")]
                             [else
                              (crea-verbo prefisso parola "e" "ono")]))))

(define frase
  (lambda (soggetto predicato oggetto)
    (string-join
      (list (scegli-articolo soggetto) soggetto
            (coniuga-verbo soggetto predicato)
            (scegli-articolo oggetto) oggetto))))

(scegli-articolo "gatto")
(coniuga-verbo "gatto" "mangiare")
(frase "gatto" "mangiare" "cibo")
(frase "cuoco" "friggere" "patate")
