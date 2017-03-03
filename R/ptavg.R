# vim: set noexpandtab tabstop=2:
#' Average of the Student t Distribution
#'
#' Average of the Student t Distribution
#'
#' @param x a vector
#' @param df degrees of freedom
#' @param hi upper bound of the shift range
#' @param lo lower bound of the shift range
#' @param n the number of bins for interpolation
#' @param lower.tail use lower tail probablity
#' @param log the probability is in log-scale
#' @keywords distribution
#' @rdname tavg
#' @export
#' @import ggplot2 stats
#' @examples
#' x=seq(from=-10, to=10, length.out=100)
#' ggplot2::qplot(x, ptavg(x, df=3, hi=3), geom='line')
ptavg=function(x, df
	, hi=1
	, lo=-hi
	, n=as.integer(ceiling(abs(hi-lo)*10))
	, lower.tail=TRUE
	, log=FALSE
	) {
	m=length(x)
	rep0=rep(0L, m)
	df = rep0 + df
	hi = rep0 + hi
	lo = rep0 + lo
	n = rep0 + n
	if(log) {
		sapply(
			seq_len(m)
			, function(i) {
				logmeanexp(
					pt(
						x[[i]]-seq(from=lo[[i]], to=hi[[i]], length.out=n[[i]])
						, df[[i]]
						, lower.tail = lower.tail
						, log.p=TRUE
						)
					)
			}
			)
	} else {
		sapply(
			seq_len(m)
			, function(i) {
				mean(
					pt(
						x[[i]]-seq(from=lo[[i]], to=hi[[i]], length.out=n[[i]])
						, df[[i]]
						, lower.tail = lower.tail
						)
					)
			}
			)
	}
}
