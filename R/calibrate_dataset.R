#' Convert original dataset using the calculated GCFs
#'
#' @param ORIG Original Survey Dataset
#' @param GCFs Gear calibration factors obtained from gcf_glmm function.
#' @param Standardard Standardard gear
#'
#' @import data.table
#' @importFrom boot inv.logit
#'
#' @export
calibrate_dataset <- function (ORIG,GCFs, Standard) {

  D <- data.table(ORIG)
  GCFs <- data.table(GCFs,keep.rownames=T)
  colnames(GCFs) <- c("GROUP","PRES.GCF","POS.GCF")

  S.pres  <- D[,list(PRES=mean(PRESENCE)),by=list(GROUP,METHOD)]
  S.pos   <- D[PRESENCE>0,list(POS=mean(DENSITY)),by=list(GROUP,METHOD)]
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

  S[METHOD!=Standard]$PRES.CAL <- inv.logit(S[METHOD!=Standard]$PRES.GCF+logit(S[METHOD!=Standard]$PRES ))
  S[METHOD!=Standard]$POS.CAL  <- S[METHOD!=Standard]$POS/S[METHOD!=Standard]$POS.GCF
  S[METHOD!=Standard]$OPUE.CAL <- S[METHOD!=Standard]$PRES.CAL*S[METHOD!=Standard]$POS.CAL

  # Re-organize
  S <- subset(S,select=c(GROUP,METHOD,GCF.SOURCE,PRES.GCF,POS.GCF,PRES,POS,OPUE,PRES.CAL,POS.CAL,OPUE.CAL))
  S[,4:ncol(S)] <- round(S[,4:ncol(S)],2)

  return(S)

}
