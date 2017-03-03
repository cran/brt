# vim: set noexpandtab tabstop=2:
#' @rdname tavg
#' @export
#' @import ggplot2 stats
#' @examples
#' q=seq(from=-10, to=10, length.out=100)
#' ggplot2::qplot(q, dtavg(q, df=3, hi=3), geom='line')
dtavg=function(x
	, df
	, hi=1
	, lo=-hi
	, n=as.integer(ceiling(abs(hi-lo)*10))
	, log=FALSE
	) {
	m=length(x)
	rep0 = rep(0L, m)
	df = rep0 + df
	hi = rep0 + hi
	lo = rep0 + lo
	n = rep0 + n
	if(log) {
		sapply(
			seq_len(m)
			, function(i) {
				logmeanexp(
					dt(
						x[[i]]-seq(from=lo[[i]], to=hi[[i]], length.out=n[[i]])
						, df[[i]]
						, log = TRUE
						)
					)
			}
			)
	} else {
		sapply(
			seq_len(m)
			, function(i) {
				mean(
					dt(
						x[[i]]-seq(from=lo[[i]], to=hi[[i]], length.out=n[[i]])
						, df[[i]]
						)
					)
			}
			)
	}
}
