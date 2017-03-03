# vim: set noexpandtab tabstop=2:
#' The P-value of a t Test Base on a t-statistic.
#'
#' The P-value of a t Test Base on a t-statistic.
#'
#' @param x a t statistic
#' @param df degrees of freedom
#' @param log the probability is in log-scale
#' @keywords distribution
#' @export
#' @import stats
#' @examples
#' tpval(1, df=3)
#' exp(tpval(1, df=3, log=TRUE))
#' tpval(Inf, df=3)
#' tpval(0, df=3)
tpval=function(x, df, log=FALSE) {
	if(log) {
		log(2)+pt(-abs(x), df=df, log.p=TRUE)
	} else {
		2*pt(-abs(x), df=df)
	}
}
