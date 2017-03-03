# vim: set noexpandtab tabstop=2:
#' Hypothesis testing using the Student t Distribution with H0: abs(mu) <= delta
#'
#' @param coefficients a vector
#' @param delta a postive cutoff
#' @param se standard error
#' @param df degrees of freedom
#' @param log the probability is in log-scale
#' @keywords htest
#' @export
#' @import ggplot2 stats
#' @examples
#' x=seq(from=-30, to=30, length.out=100)
#' 
#' data=do.call(
#'   rbind
#'   , lapply(
#'     seq_len(10)
#'     , function(delta)
#'       rbind(
#'         data.frame(x, pval=tpvaltreat(x, delta=delta, se=1, df=3), delta=delta)
#'         )
#'     )
#'   )
#' 
#' ggplot2::qplot(x, pval, data=data, color=as.factor(delta), linetype=as.factor(delta), geom='line')
tpvaltreat=function(coefficients, delta, se, df, log=FALSE) {
	n = length(coefficients)
	rep0 = rep(0L, n)
	delta = delta + rep0
	se = se + rep0
	df = df + rep0

	abs_coefficients = abs(coefficients)

	if(log) {
		tmp1=pt(
			(abs_coefficients + delta)/se
			, df=df
			, lower.tail=FALSE
			, log.p=TRUE
			)
		tmp2=pt(
			(abs_coefficients - delta)/se
			, df=df
			, lower.tail=FALSE
			, log.p=TRUE
			)

		apply(
			cbind(tmp1, tmp2)
			, 1
			, logsumexp
			)
	} else {
		pt(
			(abs_coefficients + delta)/se
			, df=df
			, lower.tail=FALSE
			) +
		pt(
			(abs_coefficients - delta)/se
			, df=df
			, lower.tail=FALSE
			)
	}
}
