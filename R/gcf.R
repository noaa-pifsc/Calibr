



#' @name gcf
#' @title Gear Calibration Factor
#'
#' @description Sets the geat Calibration Factor
#'
#' @param SET Coral Reef Species Dataset
#' @param min_obs Minimum limit for the number of observations.
#'
#' @import data.table
#' @import stats
#' @importFrom MASS mvrnorm
#'
#' @export
gcf <- function (SET, min_obs=10) {


  if(class(SET) == "list"){
    stop("Parameter SET found as a list object.")
  }


  #positive-only (Eq. 3, Nadon, et al.)
  POS <- SET[SET$DENSITY>0,]
  glm.pos  <- supressWarnings(glm(log(DENSITY)~METHOD+BLOCK,  data=POS ))
  if(nrow(POS) < min_obs){
    warning("Number of Postive-only Observations below minimum limit of ", min_obs," .")
    return(NA)
  }
  glm.pos  <- suppressWarnings(glm(log(DENSITY)~METHOD+BLOCK,  data=POS ))
  #presnce/absence (Eq. 4, Nadon et al. )
  glm.pres <- suppressWarnings(glm(PRESENCE~METHOD+BLOCK, family=binomial(link="logit"), data=SET ))


  # Coefficients and variance-covariance matrix with the (intercept) and METHOD1
  mu.pos  <-c(coef(glm.pos)[1],coef(glm.pos)[2])
  mu.pres <-c(coef(glm.pres)[1],coef(glm.pres)[2])
  vcov.pos <- vcov(glm.pos)[1:2,1:2]
  vcov.pres <- vcov(glm.pres)[1:2,1:2]
  sigma.pos <- sigma(glm.pos)

  # Monte Carlo to obtain Gear Calibration Factors with conf. intervals
  out.pos <- data.table( mvrnorm(n=10000, mu.pos, vcov.pos)     )
  colnames(out.pos)<- c("B0","B1")
  out.pos$METHODA <- exp(out.pos$B0+out.pos$B1+sigma.pos^2/2)
  out.pos$METHODB <- exp(out.pos$B0-out.pos$B1+sigma.pos^2/2)

  out.pres <- data.table(   mvrnorm(n=10000,mu.pres,vcov.pres)     )
  colnames(out.pres)<- c("B0","B1")
  out.pres$METHODA <- 1/(1+exp(-(out.pres$B0+out.pres$B1)))
  out.pres$METHODB <- 1/(1+exp(-(out.pres$B0-out.pres$B1)))

  #Observations Per Unit Effort.
  OPUE_A <- out.pos$METHODA*out.pres$METHODA
  OPUE_B <- out.pos$METHODB*out.pres$METHODB

  #GCF: GEAR Calibration Factor
  return( data.table(OPUE_A/OPUE_B ))

}
