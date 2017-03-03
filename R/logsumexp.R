# vim: set noexpandtab tabstop=2:
#' Sum of Numbers in Log-Scale
#'
#' Sum of Numbers in Log-Scale
#'
#' @param x a numeric vector
#' @keywords math
#' @export
#' @examples
#' log(mean(exp(seq_len(3))))
#' logmeanexp(seq_len(3))
logsumexp=function(x) {
	mx=max(x)
	log(sum(exp(x-mx)))+mx
}
