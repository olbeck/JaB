#' Calculate given influence/outlier statistic on a linear model object
#'
#' This function is primarily used as an internal function in \link{jab_lm}{JaB}.
#'
#' @param mod An `lm` object. Output from \link{lm}{stats}.
#' @param func.name A character string of the function name used to calculate the desired centrality statistic.
#' @param package.name A character string of the name of the package that `func.name` function is in. If left as `NULL` if the function will be called as loaded in the users environment.
#' @param args A named list of additional arguments the `func.name` function may need beyond the `mod` object.
#' @returns A numeric vector of the influence/outlier statistic as calculated by `func.name`.
#' @seealso [jab_lm()]
#' @examples
#' library(stats)
#' data("LifeCycleSavings")
#'
#' mod <- lm(sr ~ ., data = LifeCycleSavings)
#'
#' # defined functions in stats
#' get_infl(mod, "dffits")
#' get_infl(mod, "rstudent")
#'
#'
#' # define the likelihood distance as influence statistic (Cook, 1986)
#' infl_like <- function(mod){
#'
#'   n <- length(mod$fitted.values)
#'   p <- length(mod$coefficients)
#'
#'   ti <- rstudent(mod)
#'   h <- hatvalues(mod)
#'
#'   p1 <- log( (n/(n-1)) * ((n-p-1) / (ti^2 +n-p-1)) )
#'   p2 <- ti^2 * (n-1) / (1-h) / (n-p-1)
#'
#'   return(n*p1 + p2 - 1)
#' }
#'
#' infl_like(mod)
#' get_infl(mod, "infl_like")
#' @export
get_infl <- function(mod, func.name, package.name = NULL, args = NULL ){

  if(!is.null(package.name)){
    func <- getExportedValue(package.name, func.name)
  }else(
    func <- func.name
  )

  if(!is.null(args)){
    func.args <- list(mod, args)
  }else{
    func.args <- list(mod)
  }

  #call the function
  infl.stat <- do.call(
    what = func,
    args = func.args
  )

  return(infl.stat)

}


#' Jackknife-after-Bootstrap for Linear Regression
#'
#' Conduct Jackknife-after-Bootstrap (JaB) algorithm for linear regression \insertCite{martin-roberts-2013}{JaB}.
#'
#'
#' @details
#'
#' The JaB algorithm, proposed by \insertCite{martin-roberts-2013;textual}{JaB}
#' and further described by \insertCite{beyaztas-alin-2013;textual}{JaB}, detects
#' influential/outlier points in linear regression models. The algorithm is as follows:
#'
#' 1. Let \eqn{\gamma} (`stat`) be the diagnostic statistic of interest. Fit the
#' model (`mod`) and calculate \eqn{\gamma_i} for \eqn{i=1,…,n}.
#'
#' 2.  Construct \eqn{B} bootstrap samples, with replacement, from the original data set.
#'
#' 3.  For \eqn{i = 1,…,n},
#' \itemize{
#'    \item{3.1: }{Let \eqn{B_{(i)}} be the set of all bootstrap samples that did not contain data point \eqn{i}.}
#'    \item{3.2: }{For each sample in \eqn{B_{(i)}}, fit the regression model then calculate the \eqn{n} values of \eqn{\gamma_{i, (b)}}. Aggregate them into one vector \eqn{\Gamma_i}.}
#'    \item{3.1: }{Calculate suitable quantiles of \eqn{\Gamma_i} (`quant.lower` and `quant.upper`). If \eqn{\gamma_i} is outside of this range, flag point \eqn{i} as influential.}
#'  }
#'
#' The default for `quant.lower` and `quant.upper` is 0.05 and 0.95, respectively.
#' This means that if \eqn{\gamma_i} is in the center 90% of \eqn{\Gamma_i}, it
#' will not be flagged as influential.
#'
#' Some influence statistics, such as the likelihood distance from \insertCite{cook-1986;textual}{JaB},
#' are only positive values and large values imply influence. In these scenarios,
#' it is appropriate to set `quant.lower` to 0 and `quant.upper` to some suitable
#' quantile (say 0.90) so that point \eqn{i} is only flagged when \eqn{\gamma_i}
#' is in the upper quantiles of \eqn{\Gamma_i}.
#'
#' As described in \insertCite{martin-roberts-2013;textual}{JaB}, to have approximately
#' 1000 bootstrap samples in \eqn{B_{(i)}}, we need \eqn{Be^{1}\approx 3000} bootstrap
#' samples.
#'
#' See `vignette("jab-regression")` for more details and examples.
#'
#'
#' @param mod An [`lm`] object. Output from [stats::lm()].
#' @param stat A character string of the function name used to calculate the desired centrality statistic. The function must input an lm model object as is first argument and output a length \eqn{n} vector of the statistic of interest.
#' @param quant.lower A numeric between 0 and 1 used as the lower cutoff in the JaB algorithm. Default is 0.05. Must be smaller than `quant.upper`.
#' @param quant.upper A numeric between 0 and 1 used as the upper cutoff in the JaB algorithm. Default is 0.95. Must be larger than `quant.lower`.
#' @param B Number of bootstrap samples. Default is 3100
#' @param package.name A character string of the name of the package that `func.name` function is in. If left as `NULL` if the function will be called as loaded in the users environment.
#' @param stat.args A named list of additional arguments the `func.name` function may need beyond the `mod` object.
#' @returns A data frame with 5 columns.
#'  \itemize{
#'    \item{"row.ID": }{Row Number of the observation in the data set used for `mod`.}
#'    \item{"lower": }{Lower quantile cutoff as determined by `quant.lower`. }
#'    \item{"upper": }{Upper quantile cutoff as determined by `quant.upper`. }
#'    \item{"orig": }{The original influence/outlier statistic calculated by `stat`.}
#'    \item{"influential": }{Logical flagging if the observation is influential or not. TRUE if `orig` < `lower` or `orig` > `upper`. FALSE otherwise.}
#'  }
#'
#' @references \insertAllCited{}
#'
#' @examples
#' library(stats)
#' data("LifeCycleSavings")
#'
#' mod <- lm(sr ~ ., data = LifeCycleSavings)
#'
#' # JaB with DFFITS
#' result1 <- jab_lm(mod,
#'                   stat = "dffits",
#'                   quant.lower = 0.025,
#'                   quant.upper = 0.975,
#'                   B = 3100)
#' result1[result1$influential, ]
#'
#'
#' # define the likelihood distance as influence statistic (Cook, 1986)
#' infl_like <- function(mod){
#'
#'   n <- length(mod$fitted.values)
#'   p <- length(mod$coefficients)
#'
#'   ti <- rstudent(mod)
#'   h <- hatvalues(mod)
#'
#'   p1 <- log( (n/(n-1)) * ((n-p-1) / (ti^2 +n-p-1)) )
#'   p2 <- ti^2 * (n-1) / (1-h) / (n-p-1)
#'
#'   return(n*p1 + p2 - 1)
#' }
#'
#' # JaB with Likelihood Distance
#' result2 <- jab_lm(mod,
#'                   stat = "infl_like",
#'                   quant.lower = 0.00,
#'                   quant.upper = 0.95,
#'                   B = 3100)
#' result2[result2$influential, ]
#' @export
jab_lm <- function(mod,
                stat = "rstudent",
                quant.lower = 0.05,
                quant.upper = 0.95,
                B = 3100,
                package.name = NULL,
                stat.args = NULL){


  if(!(any(class(mod) =="lm"))){
    stop("mod must be an lm object")
  }
  if(quant.lower < 0 | quant.lower >= quant.upper | quant.upper > 1 ){
    stop("It must be the case that 0 < quant.lower < quant.upper < 1.")
  }
  if(B != as.integer(B) | B<1){
    stop("B must be an integer greater than 0.")
  }
  if(!existsFunction(stat)){
    stop(paste(c("The function", stat, "does not exist in your environment.")))
  }

  n <- length(mod$fitted.values)
  dat <- mod$model
  infl <- get_infl(mod, stat, package.name)
  form <- mod$call[[2]]

  #Bootstrap
  boot_index <- matrix(NA, nrow = n, ncol = B)
  t_star_boot <- matrix(NA, nrow = n, ncol = B)

  for(b in 1:B){
    index <- sample(1:n, n, replace = T)
    boot_index[ ,b] <- index
    dat.b <- dat[index, ]
    mod.b <- lm(form, data = dat.b)
    t_star_boot[, b] <- get_infl(mod.b, stat, package.name)
  }

  ## Jacknife after
  jab_ret <- matrix(NA, nrow = n, ncol = 2)

  for(i in 1:n){
    keep <- (1:B)[apply(boot_index, MARGIN = 2,
                        FUN = function(k){!any(k==i)})]
    t_star_keep <- c(t_star_boot[, keep])
    jab_ret[i , ] <- quantile(t_star_keep, c(quant.lower, quant.upper))
  }


  #check if X_i in inside jab cutoffs
  check <- data.frame(row.ID = 1:n, jab_ret, orig= infl,
                      influential = rep(FALSE, n))
  colnames(check)[2:3] <- c("lower", "upper")

  low <- check$orig < check$lower
  up <- check$orig > check$upper
  check$influential <- low | up

  return(check)
}
