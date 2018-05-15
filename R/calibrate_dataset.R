#Bindings for "global variables" used in calibrate_dataset
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("GCF.SOURCE",
                           "GROUP",
                           "OPUE",
                           "OPUE.CAL",
                           "POS",
                           "POS.CAL",
                           "POS.GCF",
                           "PRES",
                           "PRES.CAL",
                           "PRES.GCF"))
}

#' Convert original dataset using the calculated GCFs
#'
#' @param ORIG Original Survey Dataset
#' @param GCFs Gear calibration factors obtained from gcf_glmm function.
#' @param std_method Standard gear METHOD
#'
#' @import data.table
#' @importFrom boot inv.logit
#'
#' @export
calibrate_dataset <- function (ORIG,std_method, GCFs) {

  D <- data.table(ORIG)
  GCFs <- data.table(GCFs,keep.rownames=T)
  colnames(GCFs) <- c("GROUP","PRES.GCF","POS.GCF")

  S.pres  <- D[,list(PRES=mean(PRESENCE)),by=list(GROUP,METHOD,BLOCK)]
  S.pres  <- S.pres[,list(PRES=mean(PRES)),by=list(GROUP,METHOD)]

  S.pos   <- D[PRESENCE>0,list(POS=mean(DENSITY)),by=list(GROUP,METHOD,BLOCK)]
  S.pos   <- S.pos[,list(POS=mean(POS)),by=list(GROUP,METHOD)]

  S       <- merge(S.pres,S.pos)
  S$OPUE  <- S$PRES*S$POS

  S$PRES.CAL <- S$PRES
  S$POS.CAL  <- S$POS
  S$OPUE.CAL <- S$OPUE

  S <- merge(S,GCFs,by="GROUP",all.x=T)
  S <- data.table(S)

  # Fill GROUPs with no GCF with the Global ones
  S$GCF.SOURCE <-"Group-specific"
  S[is.na(PRES.GCF)]$GCF.SOURCE <- "Global"
  S[is.na(PRES.GCF)]$PRES.GCF <- GCFs[1,]$PRES.GCF
  S[is.na(POS.GCF)]$POS.GCF <- GCFs[1,]$POS.GCF

  S[METHOD!=std_method]$PRES.CAL <- inv.logit(S[METHOD!=std_method]$PRES.GCF+logit(S[METHOD!=std_method]$PRES ))
  S[METHOD!=std_method]$POS.CAL  <- S[METHOD!=std_method]$POS/S[METHOD!=std_method]$POS.GCF
  S[METHOD!=std_method]$OPUE.CAL <- S[METHOD!=std_method]$PRES.CAL*S[METHOD!=std_method]$POS.CAL

  # Re-organize
  S <- subset(S,select=c(GROUP,METHOD,GCF.SOURCE,PRES.GCF,POS.GCF,PRES,PRES.CAL,POS,POS.CAL,OPUE,OPUE.CAL))
  S[,4:ncol(S)] <- round(S[,4:ncol(S)],2)

  return(S)

}
