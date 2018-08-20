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
	let first = mul[D,D] |
		digit[first]= A
		rem[mul[D,C] + div[mul[D,D],10],10] = B
}

fun digit[x : Int]: Int {
	rem[x, 10]
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

run show for 7 Int
