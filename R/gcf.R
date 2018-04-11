
#' Gear Calibration Factor summary table
#'
#' Sets summary table for the Gear Calibration Factor and Observed Effort Per Unit per species group.
#'
#' @param SET Survey Dataset
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

  POS <- SET[SET$DENSITY>0,]

  if(nrow(POS) <= min_obs){
    stop("Number of Postive-only Observations below minimum limit of ", min_obs,".")

  }
  if(length(unique(POS$METHOD)) != 2){
    nmethods <- length(unique(POS$METHOD))
    stop("Postive-only GLM require 2 unique gear methods: ", nmethods, " gear method(s) found.")

  }

  SET$METHOD <- as.factor(SET$METHOD)
  SET$BLOCK  <- as.factor(SET$BLOCK)

  contrasts(SET$METHOD) <- c(1,-1)
  contrasts(SET$BLOCK)  <- "contr.sum"

  #positive-only (Eq. 3, Nadon, et al.)
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
  gcf_df <- data.table(OPUE_B/OPUE_A )
  gcf_quantile <- format(quantile(gcf_df$V1,c(0.5,.025,0.975),na.rm=T), digits=4)
  gcf_mean <- format(mean(gcf_df$V1,na.rm=T), digits=4)


  OPUE_POSA <- exp(mu.pos[1]+mu.pos[2])
  OPUE_POSB <- exp(mu.pos[1]-mu.pos[2])

  OPUE_PRESA<- 1/(1+exp(-(mu.pres[1]+mu.pres[2])))
  OPUE_PRESB<- 1/(1+exp(-(mu.pres[1]-mu.pres[2])))

  group_lstats <- list()
  group_lstats[["GROUP"]] <- unique(SET$GROUP)

  group_lstats[["GCF"]] <- unname(format(gcf_quantile["50%"], digits=4))
  group_lstats[["GCF_2.5"]] <- unname(format(gcf_quantile["2.5%"], digits=4))
  group_lstats[["GCF_95"]] <- unname(format(gcf_quantile["97.5%"], digits=4))

  group_lstats[["OPUE_A"]] <- format(median(OPUE_A,na.rm=T), digits=4)
  group_lstats[["OPUE_B"]] <- format(median(OPUE_B,na.rm=T), digits=4)

  group_lstats[["OPUE_POSA"]] <- format(median(OPUE_POSA), digits=4)
  group_lstats[["OPUE_POSB"]] <- format(median(OPUE_POSB), digits=4)

  group_lstats[["OPUE_PRESA"]] <- format(median(OPUE_PRESA), digits=4)
  group_lstats[["OPUE_PRESB"]] <- format(median(OPUE_PRESB), digits=4)

  return(as.data.table(group_lstats))

}
