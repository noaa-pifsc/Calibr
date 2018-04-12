
#' Survey dataset by species group and summaries
#'
#' Using the input survey dataset, the function does three major tasks. First, it splits
#' the input dataset by its \code{GROUP} value. The split dataset is used for the
#' remaining tasks: Summarizing the Gear Calibration Vaules (GCF) and Observed Per Unit
#' Effort (OPUE) by species group with positive observations, plus giving a summary of
#' total REP for each species group.
#'
#' @param SET Survey Dataset
#' @param std_method Denotes Survey dataset METHOD string as the Standard METHOD
#'
#' @return List object with three items:
#' \itemize{
#'   \item A list containing the Survey dataset split by species group.
#'   \item A data.table object with the GCF and OPUE summaries by species group with positive observations.
#'   \item Data frame summation of number of \code{REP} by species group.
#' }
#'
#' @export
#'
get_reef_datalist<- function(SET, std_method){

  # Insure that the input dataset is of the format data.table to prevent errors
  SET <- data.frame(SET)

  #If std_method doesn't match any of the two methods, then throw an error
  set_methods <- unique(SET$METHOD)

  if(!any(std_method %in% set_methods)){
    stop("Invalid method name: '", std_method, "' not found in dataset.")
  }else{
    message("Setting Standard Catch METHOD ...")
    #Set the other method as secondary.
    secondary <- as.character(set_methods[!(set_methods %in% std_method)])

    #Based on renamed METHOD numerical values,
    #Standard method will be the first factor level when METHOD is factorized
    SET[SET$METHOD==std_method,]$METHOD <- paste0("1_",std_method)
    SET[SET$METHOD==secondary,]$METHOD <- paste0("2_",secondary)

  }

  #Presume that the 1st level of METHOD factor will always be the standard method.
  std_method_factorname <- paste0("1_",std_method)

  message("Spliting dataset by GROUP value ... ")
  #Split the reef dataset into a list of smaller sets by GROUP value.
  reeffish_datalist <- split(SET, SET$GROUP)

  message( "===============================================================================\n",
           "Calculating the proportion of total number of REP w/ positive observations ... \n",
           "===============================================================================")

  #Lapply gcf function for all reef species
  lgroup_gcf <- lapply(reeffish_datalist,function(X){
    message("Group: ", unique(X$GROUP))
    tryCatch(
      gcf(SET=X,Standard=std_method_factorname),
      error=function(cond){
        message(unique(X$GROUP) , ": ", trimws(cond), " Returning NA.")
        return(NA)
      },
      warning=function(cond){
        message(unique(X$GROUP) , ": " , trimws(cond))
      }
    )

  })

  message("===========================================\n",
          "Calculating number of REP per Group  ...   \n",
          "===========================================")

  #REP stats by GROUP
  rep_stats <- sapply(reeffish_datalist,function(X,m){
    tryCatch(
      {
        vec_stats <- rep_summary(X,m)
        X <- data.frame(t(vec_stats))
      },
      error=function(cond){
        message("With ", unique(X$GROUP) , ": ", trimws(cond), " Returning NA")
        return(NA)
      },
      warning=function(cond){
        message("In ", unique(X$GROUP), ": ", trimws(cond))
      }
    )
  },
  m=std_method_factorname)

#  rep_stats <- rep_summary(reeffish_datalist[[1]],"1_nSPC")
#  for(i in 2:length(reeffish_datalist)){
#
#    aStat <- rep_summary(reeffish_datalist[[i]],"1_nSPC")

#    astat <- data.frame(t(aStat))
#    rep_stats <- rbind(rep_stats,aStat)
#  }

#  test <- reeffish_datalist[[3]]
#  if(sum(SET[SET$METHOD==std_method,]$PRESENCE)==0&sum(SET[SET$METHOD!=std_method,]$PRESENCE)==0){
#    stop("Data only available for 1 method.")
# }

  #Remove GROUP objects that have null(NA) data
  lgroup_calibrated <- lgroup_gcf[!(is.na(lgroup_gcf))]
  #Summary descriptive statistics for each GROUP
  lgroup_summary <- suppressMessages(Reduce(function(...)merge(...,all=TRUE),lgroup_calibrated))

  #REP Summary statistics table for each GROUP
  rep_stats_table <- suppressMessages(
    Reduce(function(...)merge(...,all=TRUE), rep_stats[!is.na(rep_stats)]))

  message("Returning Grouped Data and Summaries in a list ...")
  #Return grouped datalist and summary table in a list
  calibr_results <- list(LGROUP=reeffish_datalist,SUMMARY=lgroup_summary,REP_SUMMARY=rep_stats_table)
  message("Done.")

  message("\n---")
  message("Number of GROUPS: ", length(reeffish_datalist))
  message("Number of sufficent REPs: ", length(lgroup_calibrated))
  message("")

  return(calibr_results)
}
