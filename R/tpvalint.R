# vim: set noexpandtab tabstop=2:
#' Hypothesis testing using the Student t Distribution with H0: lo <= mu <= hi
#'
#' Hypothesis testing using the Student t Distribution with H0: lo <= mu <= hi
#'
#' @param coefficients a vector
#' @param lo lower bound
#' @param hi upper bound
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
#'     , function(cutoff)
#'       rbind(
#'         data.frame(x, pval=tpvalint(x, lo=-cutoff, hi=cutoff, se=1, df=3), cutoff=cutoff)
#'         )
#'     )
#'   )
#' 
#' ggplot2::qplot(x, pval, data=data, color=as.factor(cutoff), linetype=as.factor(cutoff), geom='line')
tpvalint=function(coefficients, hi, lo=-hi, se, df, log=FALSE) {
	n = length(coefficients)
	rep0 = rep(0L, n)
	lo = lo + rep0
	hi = hi + rep0
	se = se + rep0
	df = df + rep0

	hi_i = coefficients > hi
	lo_i = coefficients < lo
	if(log) {
		p = rep0
		p[hi_i] = log(2) + pt(
			abs((coefficients[hi_i]-hi[hi_i])/se[hi_i])
			, df=df[hi_i]
			, lower.tail=FALSE
			, log.p=TRUE
			)
		p[lo_i] = log(2) + pt(
			abs((coefficients[lo_i]-lo[lo_i])/se[lo_i])
			, df=df[lo_i]
			, lower.tail=FALSE
			, log.p=TRUE
			)
	}
	else {
		p = 1+rep0
		p[hi_i] = 2 * pt(
			abs((coefficients[hi_i]-hi[hi_i])/se[hi_i])
			, df=df[hi_i]
			, lower.tail=FALSE
			)
		p[lo_i] = 2 * pt(
			abs((coefficients[lo_i]-lo[lo_i])/se[lo_i])
			, df=df[lo_i]
			, lower.tail=FALSE
			)
	}
	p
}
