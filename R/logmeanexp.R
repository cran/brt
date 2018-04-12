# vim: set noexpandtab tabstop=2:
#' Mean of Numbers in Log-Scale
#'
#' Mean of Numbers in Log-Scale
#'
#' @param x a numeric vector
#' @keywords math
logmeanexp=function(x) {
	mx=max(x)
	log(mean(exp(x-mx)))+mx
}
