# vim: set noexpandtab tabstop=2
#' BRT test
#'
#' BRT test
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param hi upper bound of the shift range (i.e. signficant if outside the range)
#' @param lo lower bound of the shift range (i.e. if hi=lo=0, return t.test)
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.If TRUE then the pooled variance is used to estimate the variance otherwise the Satterhwaite approximation to the degrees of freedom is used.
#' @param log_pvalue brt.value is returned in log scale.
#' @export
#' @import stats
#' @examples
#' x=rnorm(10, 0, 1)
#' y=rnorm(10, 8, 2)
#' brt.test(x, y, hi=3)

brt.test = 
function(x, y, hi, lo=-hi, var.equal=T, log_pvalue=F){
  if(all(hi==0, lo==0)){
    out=t.test(x=x, y=y, var.equal=var.equal)
  }else{
    t1 = x[!is.na(x)]; t2 = y[!is.na(y)]
    var_x = var(t1); var_y = var(t2)
    mu_x = mean(t1); mu_y = mean(t2)
    n_x = length(t1); n_y = length(t2)
    if(var.equal){
      s_pool = sqrt( ((n_x-1)*var_x + (n_y-1)*var_y) / (n_x+n_y-2) )
      se_pool = s_pool * sqrt(1/n_x+1/n_y)
      df = n_x+n_y-2
    }else{
      tmp_var = (var_x/n_x) + (var_y/n_y)
      se_pool = sqrt(tmp_var)
      df_t1 = tmp_var^2
      df_t2 = ( ((var_x/n_x)^2/(n_x-1)) + ((var_y/n_y)^2/(n_y-1)) )
      df = df_t1 / df_t2
    }
    out = data.frame(mu_x=mu_x
      , mu_y=mu_y
      , se=se_pool
      , df=df
      , brt.pvalue=tpvalavg(mu_x-mu_y, hi=hi, lo=lo, se=se_pool, df=df, log=log_pvalue)
      )
  }
  return(out)
}
