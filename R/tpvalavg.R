# vim: set noexpandtab tabstop=2:
#' Average of The Student t Distribution
#'
#' Average of The Student t Distribution
#'
#' @param coefficients a vector
#' @param se standard error
#' @param df degrees of freedom
#' @param hi upper bound of the shift range
#' @param lo lower bound of the shift range
#' @param n the number of bins for interpolation
#' @param log the probability is in log-scale
#' @keywords distribution
#' @export
#' @import ggplot2 stats
#' @examples
#' x=seq(from=0, to=30, length.out=100)
#' 
#' data=do.call(
#'   rbind
#'   , lapply(
#'     seq_len(10)
#'     , function(cutoff)
#'       rbind(
#'         data.frame(x, pval=tpvalavg(x, hi=1, se=1, df=3), cutoff=cutoff)
#'         )
#'     )
#'   )
#'
#' ggplot2::qplot(x, log(pval), data=data, color=as.factor(cutoff), 
#' 	linetype=as.factor(cutoff), geom='line')
#' tpvalavg(1, hi=1, se=1, df=3)
#' exp(tpvalavg(1, hi=1, se=1, df=3, log=TRUE))
tpvalavg=function(coefficients, hi, lo=-hi
	, se
	, df
	, n=as.integer(ceiling(abs(hi-lo)*10))
	, log=FALSE
	) {
	if(log) {
		log(2) + ptavg(-abs(coefficients-(hi+lo)/2)/se, df, hi=hi/se, lo=lo/se, log=TRUE)
	} else {
		2*ptavg(-abs(coefficients-(hi+lo)/2)/se, df, hi=hi/se, lo=lo/se)
	}
}
