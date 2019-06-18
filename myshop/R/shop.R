#' @param k a scalar determining the factorials.
#' @return a scalar equivalent to \eqn{n!}.
#' @export
#' @seealso
#' @references
#'importFrom("graphics", "hist", "par")
#'importFrom("stats", "median" , "sample")
#' @examples
#' ## a trivial example
#' eats(what)

eats = function(what)
{
  sample(shop$shop_name , 1)
}

#' @param k a scalar determining the factorials.
#' @return a scalar equivalent to \eqn{n!}.
#' @export
#' @seealso
#' @references
#'importFrom("graphics", "hist", "par")
#'importFrom("stats", "median")
#' @examples
#' ## a trivial example
#' score("The Old Factory Restaurant" , 20)
score=function(name , n)
{

  row = which(shop$shop_name == name)
  MEAN=MEDIAN=rep(NA,500)
  for(i in 1:500)
  {
    x=sample(1:5,size=n,replace=T,
             prob=c(shop[row,3] , shop[row,4] , shop[row,5] , shop[row,6] , shop[row,7]))
    MEAN[i]=mean(x)
    MEDIAN[i]=median(x)
  }
  par(mfrow=c(1,1))
  hist(MEAN , col = "red")
  print(paste0("mean = ",mean(MEAN)))
  print(paste0("median = ",median(MEDIAN)))
}
