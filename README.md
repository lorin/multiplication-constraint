# Multiplication constraint problem

My son was given the following math problem as a rising sixth grader.

In the following multiplication, A, B, C and D represent different digits. What
is the answer for the multiplication?

````
ABCD
×  D
----
DCBA
````

I thought this type of constraint satisfaction problem would be a good excuse
to practice with programming languages I don't often use.

## Alloy

I thought Alloy would be the easiest one, and that I'd be able to do something
like:

```alloy
fact MultiplicatinProblem {
    Digit = {0,1,2,3,4,5,6,7,8,9}
    A in Digit
    B in Digit
    C in Digit
    D in Digit
    A !=B ; A!=C ; A!= D; B!=C; B!=D; C!=D
    (1000*A + 100*B + 10*C + D)*D = 1000*D + 100*C + 10*B + A
}
```

However, Alloy requires that you explicitly specify the size of the integers.
You need to specify a very wide range for something like the above to work, and
that makes the search space too large.

Instead, I implemented multi-digit multiplication, like this:

```alloy
one sig S {
	A : Int,
	B : Int,
	C : Int,
	D:  Int
} {
	SingleDigit[A]
	SingleDigit[B]
	SingleDigit[C]
	SingleDigit[D]

	let first = integer/mul[D,D],
		second = integer/mul[C,D] + carry[first],
		third = integer/mul[B,D] + carry[second],
		fourth = integer/mul[A,D] + carry[third] |
		digit[first] = A and
		digit[second] = B and
		digit[third] = C and
		digit[fourth] = D
}

fact AllDifferent {
	A != B
	A != C
	A != D
	B != C
	B != D
	C != D
}

// digit, carry, SingleDigit helper functions not shown her


run {} for 1 S, 8 Int
```

The complete source is [multiplication.als](multiplication.als)

## Haskell

Haskell was the most straightforward, albeit using a brute-force approach.
I used the List monad, which looks quite elegant to me.

```haskell
solution :: [Int]
solution = do
    let digit = [0..9]
    a <- digit
    b <- digit
    c <- digit
    d <- digit
    guard $ a/=b
    guard $ a/=c
    guard $ a/=d
    guard $ b/=c
    guard $ b/=d
    guard $ (1000*a+100*b+10*c+d)*d == 1000*d+100*c+10*b+a
    [a,b,c,d]
```

The complete source is [multiplication.hs](multiplication.hs)

## MiniKanren (Racket)

MiniKanren is a logic programming language, like Prolog. This is the
first time I've written a MiniKanren program.

Here's the main implementation, without the helper functions.

```scheme
; a,b,c,d -> 1000*a + 100b + 10c + d
(define from-digits 
  (lambda (a b c d abcd)
    (let ((ten (build-num 10))
          (hundred (build-num 100))
          (thousand (build-num 1000)))
      (fresh (a000 b00 c0 ab00 cd)
        (*o thousand a a000)
        (*o hundred b b00)
        (*o ten c c0)
        (pluso a000 b00 ab00)
        (pluso c0 d cd)
        (pluso ab00 cd abcd)))))

; This solves it
(define solution
  (let ((digits (map build-num '(0 1 2 3 4 5 6 7 8 9))))
  (run 1 (q)
    (fresh (a b c d)
      (membero a digits)
      (membero b digits)
      (membero c digits)
      (membero d digits)
      (=/= a b)
      (=/= a c)
      (=/= a d)
      (=/= b c)
      (=/= b d)
      (=/= c d)
      (fresh (abcd dcba abcdtd)
        (from-digits a b c d abcd)
        (from-digits d c b a dcba)
        (*o abcd d abcdtd)
        (== abcdtd dcba)
        (== q abcd))))))

```

The complete source is [multiplication.rkt](multiplication.rkt)
