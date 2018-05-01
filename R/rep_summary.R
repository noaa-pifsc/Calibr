
#' Site summmary statistics per reef species group
#'
#' Calculates descriptive statistics for each GROUP
#'
#' @param SET Dataset
#' @param std_method Denotes Survey dataset METHOD string as the Standard METHOD
#' @param stat_type Type of Generalized Linear Model used
#'
#' @importFrom dplyr arrange_
#' @importFrom plyr daply
#'
#' @export
rep_summary <- function(SET, std_method, stat_type=c("GLM","GLMM")) {

  #validate value for 'stat_type' param
  stat_type <- match.arg(stat_type)

  BLOCK <- SET[["BLOCK"]]
  METHOD <- SET[["METHOD"]]

  if(class(SET) == "list"){
    stop("Parameter SET found as a list object.")
  }

  if(length(unique(SET$METHOD)) != 2){
    nmethods <- length(unique(SET$METHOD))
    stop("2 unique gear methods required: ", nmethods, " gear method(s) found.")
  }

  if(!any(unique(SET$METHOD) %in% std_method)){
    stop("Invalid method name: '", std_method, "' not found in dataset.")
  }else{
    #Set the other method as secondary.
    set_methods <- unique(SET$METHOD)
    secondary_method <- as.character(set_methods[!(set_methods %in% std_method)])
  }

  if(length(unique(SET$GROUP)) != 1){
    ngroups <- length(unique(SET$GROUP))
    stop("Single GROUP type required: ", ngroups, " GROUP types found.")
  }

  if(sum(SET[SET$METHOD==std_method,]$PRESENCE)==0|sum(SET[SET$METHOD!=std_method,]$PRESENCE)==0){
      stop("Data only available for 1 method.")
  }


  #Sort Input Dataframe by "BLOCK" and (Standard) METHOD. plyr functions auto-sorts their output.
  #SET <- dplyr::arrange_(SET,"BLOCK", "METHOD")
  SET <- plyr::arrange(SET, BLOCK, METHOD)

  #Make a vector of all BLOCKs where the GROUP was seen at least once by both METHODs.
  #require(plyr)
  #lblock <- plyr::daply(SET, "BLOCK", function(X, m){ unique(X["METHOD"]) %in% m } , m=unique(SET["METHOD"]))
  lblock <- plyr::daply(SET, "BLOCK", function(X, m){
    plyr::arrange(unique(X["METHOD"]),METHOD) %in% m } , m=plyr::arrange(unique(SET["METHOD"]),METHOD))

  lblock <- names(lblock)[lblock & complete.cases(lblock)] #complete.cases ~ no NA's

  #From this BLOCK list, create a REP list (all REPs where that GROUP could potentially have been seen)
  setblock_2method <- SET[SET$BLOCK %in% lblock,]

  seengroup_pos <- setblock_2method[setblock_2method$DENSITY >0,]

  #From setblock_2method, calculate the number of REP <------ this is the number of REPs where the GROUP could have
  #been seen, for each METHOD.
  #From seengroup_pos, calculate how many REP have a positive (i.e. not a zero) observation for each METHOD.


  return(c(GROUP=unique(SET$GROUP),
           NREP_TOTAL=nrow(setblock_2method["REP"]),
           NREP_STD_METHOD=nrow(setblock_2method[setblock_2method["METHOD"] == std_method,]["REP"]),
           NREP_SEC_METHOD=nrow(setblock_2method[setblock_2method["METHOD"] == secondary_method,]["REP"]),
           POSREP_TOTAL=nrow(seengroup_pos["REP"]),
           POSREP_STD_METHOD=nrow(seengroup_pos[seengroup_pos["METHOD"] == std_method,]["REP"]),
           POSREP_SEC_METHOD=nrow(seengroup_pos[seengroup_pos["METHOD"] == secondary_method,]["REP"]),
           use.names=TRUE))
}
