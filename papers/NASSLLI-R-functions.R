#for convenience, defining curried versions of some numerical operators.
eq = function(n) {
	return(function(x) {return(x == n)})
}
gtr = function(min){
	return(function(x) {return(x > min)})
}
geq = function(min){
	return(function(x) {return(x >= min)})
}
lt = function(max){
	return(function(x) {return(x < max)})
}
leq = function(max){
	return(function(x) {return(x <= max)})
}

#some counting functions.
howmany = function(vec,property){
	return(length(which(property(vec))))
}
#fxn has to be a function of one variable that can apply to each element of vec.
prop = function(vec,property){
	return(howmany(vec,property)/length(vec))
}