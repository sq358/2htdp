; Exercise 31
> (write-file
   'stdout
   (letter "Matthew" "Fisler" "Felleisen"))
Dear Matthew,

We have discovered that all people with the
last name Fisler have won our lottery. So,
Matthew, hurry and pick up your prize.

Sincerely,

Felleisen
'stdout

(define (main in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))

; executed in DrRacket's interaction area
> (write-file "firstname.data" "John")
"firstname.data"
> (write-file "lastname.dat" "Smith")
"lastname.dat"
> (write-file "signature.dat" "Lottery Corp.")
"signature.dat"

> (main "firstname.dat" "lastname.dat" "signature.dat" "out.dat")
"out.dat"
> (read-file "out.dat")
"Dear John,\n\nWe have discovered that all people with the\nlast name Smith have won our lottery. So, \nJohn, hurry and pick up your prize.\n\nSincerely,\n\nLottery Corp."
>
