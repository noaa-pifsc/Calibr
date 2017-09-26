
#' Site summmary statistics per reef species group
#'
#' Calculates descriptive statistics for each GROUP
#'
#' @param SET Dataset
#' @param std_method Establishes the standard METHOD of the Reef dataset
#'
#' @importFrom dplyr arrange_
#' @importFrom plyr daply
#'
rep_summary <- function(SET, std_method) {

  if(class(SET) == "list"){
    stop("Parameter SET found as a list object.")
  }

  if(length(unique(SET$METHOD)) != 2){
    nmethods <- length(unique(SET$METHOD))
    stop("2 unique gear methods required: ", nmethods, " gear method(s) found.")
  }

  if(!any(std_method %in% unique(SET$METHOD))){
    stop("Invalid method name: '", std_method, "' not found in dataset.")
  }else{
    #Set the other method as secondary.
    secondary_method <- as.character(set_methods[!(set_methods %in% std_method)])
  }

  if(length(unique(SET$GROUP)) != 1){
    ngroups <- length(unique(SET$GROUP))
    stop("Single GROUP type required: ", ngroups, " GROUP types found.")
  }

  SET <- dplyr::arrange_(SET,"BLOCK") #Sort Input Dataframe by "BLOCK". plyr functions auto-sorts their output.

  #Make a vector of all BLOCKs where the GROUP was seen at least once by both METHODs.
  #require(plyr)
  lblock <- plyr::daply(SET, "BLOCK", function(X, m){ unique(X["METHOD"]) %in% m } , m=unique(SET["METHOD"]))
  lblock <- names(lblock)[lblock & complete.cases(lblock)] #complete.cases ~ no NA's

  #From this BLOCK list, create a REP list (all REPs where that GROUP could potentially have been seen)
  setblock_2method <- SET[SET$BLOCK %in% lblock,]

  #From this REP list, calculate the number of REP <------ this is the number of REPs where the GROUP could have been seen.
  rep_seengroup <- unlist(setblock_2method["REP"], use.names=FALSE)
  nrep_seengroup <- length(rep_seengroup)


  #From the same REP list, calculate how many REP have a positive (i.e. not a zero) observation for each METHOD.
  rep_seengroup_pos <- unlist(setblock_2method[setblock_2method$DENSITY >0,]["REP"], use.names=FALSE)
  nrep_seengroup_pos <- length(rep_seengroup_pos)

  return(c(GROUP=unique(SET$GROUP), NUM_REP=nrep_seengroup, NUM_REP_POS=nrep_seengroup_pos, use.names=TRUE))
}
