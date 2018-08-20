open util/natural as nat

sig Digit extends nat/Natural {}

sig A,B,C,D extends Digit {}

fact DigitLessThanTen {
	all d : Digit | nat/lt[d,10]
}

fact Singletons {
	one A
	one B
	one C
	one D
}


pred show() {}

run show for 4
