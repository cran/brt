# vim: set noexpandtab tabstop=2:
#' Sum of Numbers in Log-Scale
#'
#' Sum of Numbers in Log-Scale
#'
#' @param x a numeric vector
#' @keywords math
logsumexp=function(x) {
	mx=max(x)
	log(sum(exp(x-mx)))+mx
}
