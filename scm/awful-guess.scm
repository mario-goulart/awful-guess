(define (awful-guess game-id base-path data-file #!key (awful-settings (lambda (_) (_))))

  (define base-str-path (string-intersperse base-path "/"))

  (define passwords/tips
    (with-input-from-file data-file read-file))

  (define (random-password/tips)
    (let ((pw/tips-len (length passwords/tips))
          (past-challenges ($session 'past-challenges '())))
      (when (= (length past-challenges) pw/tips-len)
        ($session-set! 'past-challenges '())
        (set! past-challenges '()))
      (let loop ()
        (let* ((random-pw-idx (random pw/tips-len))
               (attempt (list-ref passwords/tips random-pw-idx)))
          (if (memq random-pw-idx past-challenges)
              (loop)
              random-pw-idx)))))

  (define (tips password/tips)
    (cdr password/tips))

  (define (password password/tips)
    (car password/tips))

  (define (password-game)
    (add-javascript
     "$(document).ajaxStart(function(){$('#ajax-busy').show();}).ajaxStop(function(){$('#ajax-busy').hide();});"
     "$('#guess').focus();")

    (define (new-round!)
      (let ((round ($session 'round)))
        (unless (= round -1)
          ($session-set! 'history
                         (cons
                          (list round
                                ($session 'password)
                                ($session 'past-tips)
                                ($session 'attempts))
                          ($session 'history))))
        (let* ((random-pw-idx (random-password/tips))
               (current-challenge (list-ref passwords/tips random-pw-idx)))
          ($session-set! 'round (add1 round))
          ($session-set! 'past-challenges (cons random-pw-idx ($session 'past-challenges '())))
          ($session-set! 'password (password current-challenge))
          ($session-set! 'tips (tips current-challenge))
          ($session-set! 'attempts '())
          ($session-set! 'past-tips '()))))

    (define (format-history history round score)
      (if (null? history)
          ""
          `((p (span (@ (id "score")) ,score)
               " acertos em "
               (span (@ (id "round") ,round)) " rodadas.")
            (table (@ (id "history-table"))
                   (tr ,@(map (lambda (i)
                                `(th ,i))
                              `("Rodada" "Senha" "Dicas" "Tentativas")))
                   ,@(map (lambda (history-item)
                            (let ((round (car history-item))
                                  (password (cadr history-item))
                                  (tips (caddr history-item))
                                  (attempts (cadddr history-item)))
                              `(tr (td ,round)
                                   (td ,password)
                                   (td ,(string-intersperse tips ", "))
                                   (td ,(let ((match? (member password attempts)))
                                          `(span (@ (id ,(if match?
                                                             "right-attempt"
                                                             "wrong-attempt")))
                                                 ,(string-intersperse attempts ", ")))))))
                          history)))))

    (define (challenge score tips round chances attempts past-tips history)
      `((div (@ (id "left"))
             (p "Dica: "
                ,(if (null? past-tips)
                     ""
                     `(span (@ (id "past-tips"))
                            ,(string-intersperse past-tips " > ") " > "))
                " "
                (span (@ (id "tip")) ,(car tips))
                (span (@ (id "status"))))
             (p "Senha: " (input (@ (type "text") (id "guess"))))
             (p "Restam " ,chances " tentativas "
                ,(if (null? attempts)
                     ""
                     `("(" (span (@ (id "attempts"))
                                 ,(string-intersperse attempts ", ")) ")")))
             ,(link base-str-path "Reiniciar o jogo"))
        (div (@ (id "right"))
             (div (@ (id "history"))
                  ,(format-history history round score)))))

    (ajax (make-pathname base-str-path "try") 'guess 'change
          (lambda ()
            (let ((password ($session 'password))
                  (tips ($session 'tips))
                  (score ($session 'score))
                  (round ($session 'round))
                  (guess ($ 'guess)))
              ($session-set! 'past-tips (append ($session 'past-tips) (list (car tips))))
              ($session-set! 'attempts (append ($session 'attempts) (list guess)))
              (if (equal? password (string-downcase guess))
                  (begin
                    ($session-set! 'score (add1 score))
                    (new-round!))
                  (if (null? (cdr tips))
                      (new-round!)
                      ($session-set! 'tips (cdr tips))))
              (challenge ($session 'score)
                         ($session 'tips)
                         ($session 'round)
                         (length ($session 'tips))
                         ($session 'attempts)
                         ($session 'past-tips)
                         ($session 'history))))
          live: #t
          success: "$('#challenge').html(response); setTimeout(function(){$('#guess').focus();}, 300);" ;; FIXME?
          arguments: `((guess . "$('#guess').val()")))

    ($session-set! '((score . 0)
                     (round . -1)
                     (history . ())))
    (new-round!)
    `((div (@ (id "ajax-busy")))
      (div (@ (id "contents"))
           (h1 "Adivinhe a senha")
           (div (@ (id "challenge"))
                ,(challenge ($session 'score)
                            ($session 'tips)
                            ($session 'round)
                            3
                            '()
                            ($session 'past-tips)
                            ($session 'history))))
      (div (@ (id "awful"))
           (a (@ (href "http://chicken.wiki.br/egg/awful"))
              (img (@ (border "0")
                      (src "/img/thats-awful.png")))))))



  (define-app awful-guess
    matcher: base-path
    handler-hook: (lambda (handler)
                    (parameterize ((enable-sxml #t)
                                   (app-root-path base-str-path)
                                   (page-charset "UTF-8")
                                   (session-cookie-name (conc "awful-guess-" game-id))
                                   (page-doctype"<!DOCTYPE html>")
                                   (page-css "/css/awful-guess.css"))
                      (awful-settings handler)))

    (define-session-page base-str-path
      password-game
      use-ajax: #t)

  )) ;; end module
