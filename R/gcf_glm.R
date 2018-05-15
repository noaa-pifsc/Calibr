#Bindings for "global variables"
if(getRversion() >= "2.15.1")  {
   utils::globalVariables(c("METHOD", "PRESENCE", "DENSITY"))
}

#' Gear Calibration Factor summary table
#'
#' Sets summary table for the Gear Calibration Factor and Observed Effort Per Unit per species group.
#'
#' @param SET Survey Dataset
#' @param std_method Denotes Survey dataset METHOD string as the Standard METHOD
#' @param min_obs Minimum limit for the number of observations.
#'
#' @import data.table
#' @import stats
#' @importFrom MASS mvrnorm
#' @importFrom boot inv.logit logit
#'
#' @export
gcf_glm <- function (SET, std_method, min_obs=10) {


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
  contrasts(SET$METHOD) <- c(0,1)
  contrasts(SET$BLOCK)  <- "contr.sum"

  POS$METHOD <- as.factor(POS$METHOD)
  POS$BLOCK  <- as.factor(POS$BLOCK)
  contrasts(POS$METHOD) <- c(0,1)
  contrasts(POS$BLOCK)  <- "contr.sum"


  #positive-only (Eq. 3, Nadon, et al.)
  glm.pos  <- suppressWarnings(glm(log(DENSITY)~METHOD+BLOCK,  data=POS ))
  #presnce/absence (Eq. 4, Nadon et al. )
  glm.pres <- suppressWarnings(glm(PRESENCE~METHOD+BLOCK, family=binomial(link="logit"), data=SET ))


  # Coefficients and Monte Carlo to obtain Gear Calibration Factors with conf. intervals
  mu.pres.method    <- glm.pres$coefficients[2]
  SD.pres.method    <- summary(glm.pres)$coefficients[2,2]
  GCF.pres          <- mu.pres.method
  pres.method       <- rnorm(n=1000,mean=mu.pres.method,sd=SD.pres.method)
  GCF.pres.dist     <- pres.method # Additive method effect in logit space
  GCF.pres.quantile <- format(quantile(GCF.pres.dist,c(0.5,.025,0.975),na.rm=T), digits=4)

  mu.pos.method     <- glm.pos$coefficients[2]
  SD.pos.method     <- summary(glm.pos)$coefficients[2,2]
  GCF.pos           <- exp(mu.pos.method)
  pos.method        <- rnorm(n=1000,mean=mu.pos.method,sd=SD.pos.method)
  GCF.pos.dist      <- exp(pos.method)
  GCF.pos.quantile  <- format(quantile(GCF.pos.dist,c(0.5,.025,0.975),na.rm=T), digits=4)

  # Convert original dataset using the GCFs to confirm that model worked correctly
  SET <- data.table(SET)
  POS <- data.table(POS)

  SET <- SET[order(METHOD)]
  POS <- POS[order(METHOD)]

  site_pres  <- SET[,list(PRES=mean(PRESENCE)),by=list(METHOD,BLOCK)]
  site_pres  <- site_pres[,list(PRES=mean(PRES)),by=list(METHOD)]

  site_pos   <- POS[,list(POS=mean(DENSITY)),by=list(METHOD,BLOCK)]
  site_pos   <- site_pos[,list(POS=mean(POS)),by=list(METHOD)]

  site_dt       <- merge(site_pres,site_pos)
  site_dt$OPUE  <- site_dt$PRES*site_dt$POS

  site_dt$PRES.CAL <- site_dt$PRES
  site_dt$POS.CAL  <- site_dt$POS
  site_dt$OPUE.CAL <- site_dt$OPUE


  site_dt[METHOD!=std_method]$PRES.CAL <- inv.logit(logit(site_dt[METHOD!=std_method]$PRES)-GCF.pres )
  site_dt[METHOD!=std_method]$POS.CAL  <- site_dt[METHOD!=std_method]$POS/GCF.pos
  site_dt[METHOD!=std_method]$OPUE.CAL <- site_dt[METHOD!=std_method]$PRES.CAL*site_dt[METHOD!=std_method]$POS.CAL

  site_dt <- cbind(site_dt,t(GCF.pres.quantile))
  colnames(site_dt)[8:10]  <- c("GCF.PRES","GCF.PRES_2.5","GCF.PRES_95")
  site_dt <- cbind(site_dt,t(GCF.pos.quantile))
  colnames(site_dt)[11:13] <- c("GCF.POS","GCF.POS_2.5","GCF.POS_95")

  # Populate output list
  group_lstats <- list()
  group_lstats[["GROUP"]]  <- unique(SET$GROUP)
  group_lstats[["METHOD"]] <- unique(SET$METHOD)

  #group_lstats[["GCF.PRES"]]    <- unname(format(GCF.pres.quantile["50%"], digits=2))
  group_lstats[["GCF.PRES"]]     <- unname(format(GCF.pres,digits=2)) # Base case instead of mean
  group_lstats[["GCF.PRES_2.5"]] <- unname(format(GCF.pres.quantile["2.5%"], digits=2))
  group_lstats[["GCF.PRES_95"]]  <- unname(format(GCF.pres.quantile["97.5%"], digits=2))

  #group_lstats[["GCF.POS"]]    <- unname(format(GCF.pos.quantile["50%"], digits=2))
  group_lstats[["GCF.POS"]]     <- unname(format(GCF.pos,digits=2)) # Base case instead of mean
  group_lstats[["GCF.POS_2.5"]] <- unname(format(GCF.pos.quantile["2.5%"], digits=2))
  group_lstats[["GCF.POS_95"]]  <- unname(format(GCF.pos.quantile["97.5%"], digits=2))

  group_lstats[["PRES"]]     <- format(site_dt$PRES, digits=2)
  group_lstats[["PRES.CAL"]] <- format(site_dt$PRES.CAL, digits=2)
  group_lstats[["POS"]]      <- format(site_dt$POS, digits=2)
  group_lstats[["POS.CAL"]]  <- format(site_dt$POS.CAL, digits=2)
  group_lstats[["OPUE"]]     <- format(site_dt$OPUE, digits=2)
  group_lstats[["OPUE.CAL"]] <- format(site_dt$OPUE.CAL, digits=2)

  return(as.data.table(group_lstats))

}
