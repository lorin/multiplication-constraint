# Multiplication constraint problem

My son was given several math problem over the summer as a rising sixth grader.
this was one of them:

*In the following multiplication, A, B, C and D represent different digits. What
is the answer for the multiplication?*

````
ABCD
×  D
----
DCBA
````

I thought this type of constraint satisfaction problem would be a good excuse
to practice with programming languages I don't often use.

I solved them in [Alloy], Haskell, and [MiniKanren].

[Alloy]: http://alloytools.org
[MiniKanren]: http://minikanren.org

## Alloy

I thought Alloy would be the easiest one, and that I'd be able to do something
like:

```alloy
fact MultiplicatinProblem {
	IsSingleDigit[A]
	IsSingleDigit[B]
	IsSingleDigit[C]
	IsSingleDigit[D]
    A !=B ; A!=C ; A!= D; B!=C; B!=D; C!=D

    // ABCD * D = DCBA
    mul[D, mul[1000,A] + mul[100,B] + mul[10,C] + D] = mul[1000,D] + mul[100,C] + mul[10,B] + A
}
```

However, I gave up waiting for the solver to find a solution.

To speed things up, I constrained the possible integers to a much smaller space
(8 bit integers instead of 15 bit) and implemented multi-digit multiplication explicitly in Alloy:

```alloy
one sig S {
	A : Int,
	B : Int,
	C : Int,
	D:  Int
} {
	IsSingleDigit[A]
	IsSingleDigit[B]
	IsSingleDigit[C]
	IsSingleDigit[D]

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

// digit, carry, IsSingleDigit helper functions not shown here

run {} for 1 S, 8 Int
```

The complete source is [multiplication.als](multiplication.als)

## Haskell

I found the Haskell implementation to be the most straightforward to implement,
using the "do" syntax.

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
