/*
    ABCD
    *  D
    ----
    DCBA
*/

open util/integer as integer

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

	let first = mul[D,D], 
		second = mul[C,D] + carry[first],
		third = mul[B,D] + carry[second],
		fourth = mul[A,D] + carry[third] |
		digit[first] = A and 
		digit[second] = B and
		digit[third] = C and
		digit[fourth] = D
}

fun digit[x : Int]: Int {
	integer/rem[x, 10]
}

fun carry[x : Int] : Int {
	integer/div[x,10]
}

pred SingleDigit[d : Int] {
	d>=0 and d<10
}

fact AllDifferent {
	A != B
	A != C
	A != D
	B != C
	B != D
	C != D
}




run {} for 1 S, 8 Int
