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

	// ABCD * D = DCBA
	let first = mul[D,D], 
		  second = mul[C,D] + carry[first],
			third = mul[B,D] + carry[second] |
		digit[first]= A and 
		digit[second] = B and
		digit[third] = C
}

fun digit[x : Int]: Int {
	rem[x, 10]
}

fun carry[x : Int] : Int {
	div[x,10]
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


pred show() {
	one S
}

run show for 8 Int
