`splitMatrix` <-
function(x, col=1) {
	ncol <- ncol(x)
	l <- split(x, x[,col])
	lapply(l, function(subM) matrix(subM, ncol=ncol))
}

