
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
#' @param stat_model Type of Generalized Linear Model used.
#' @param n_sample Number of Samples
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
run_calibr <- function(SET, std_method, stat_model=c("GLM","GLMM"), n_sample=5){

  stat_model <- match.arg(stat_model)

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

  #Split the dataset into a list of smaller sets by GROUP value.
  message( "====================================================================\n",
           "Splitting dataset by GROUP value ... \n",
           "====================================================================")
  group_datalist <- split(SET, SET$GROUP)


  message( "====================================================================\n",
           "Applying ", stat_model, " Gear Calibration Factor to dataset ... \n",
           "====================================================================")

  if(stat_model=="GLM"){

    # #Lapply gcf function for all species
    # lgroup_gcf <- lapply(fish_datalist,function(X){
    #   message("Group: ", unique(X$GROUP))
    #   tryCatch(
    #     if(stat_model=="GLM"){
    #       gcf_glm(SET=X, std_method=std_method_factorname)
    #     }
    #     ,#End of code expresssion
    #     error=function(cond){
    #       message(unique(X$GROUP) , ": ", trimws(cond), " Returning NA.")
    #       return(NA)
    #     },
    #     warning=function(cond){
    #       message(unique(X$GROUP) , ": " , trimws(cond))
    #     }
    #   )
    #
    # })
    #
    # #Remove GROUP objects that have null(NA) data
    # lgroup_calibrated <- lgroup_gcf[!(is.na(lgroup_gcf))]
    # #Summary descriptive statistics for each GROUP
    # gcf_summary <- suppressMessages(Reduce(function(...)merge(...,all=TRUE),lgroup_calibrated))
    gcf_summary <- gcf_glm(SET, std_method = std_method_factorname, n_sample = n_sample)
    n_sufficentREPS <- nrow(gcf_summary)

  }else if(stat_model=="GLMM"){

    gcf_results <- gcf_glmm(ORIG=SET, std_method=std_method_factorname, n_sample=n_sample)
    n_sufficentREPS <- nrow(gcf_results$SUMMARY)
    gcf_summary <- gcf_results$SUMMARY
  }
  message("====================================================================\n",
          "Calculating number of REP per Group  ...   \n",
          "====================================================================")

  #REP stats by GROUP
  rep_stats <- sapply(group_datalist,function(X,m){
    tryCatch(
      {
        vec_stats <- rep_summary(X,m)
        X <- data.frame(as.list(vec_stats))
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
  m=std_method_factorname, simplify = FALSE)




  #REP Summary statistics table for each GROUP
  rep_stats_table <- suppressMessages(
    Reduce(function(...)merge(...,all=TRUE), rep_stats[!is.na(rep_stats)]))

  message("Returning Grouped Data and Summaries in a list ...")
  #Return grouped datalist and summary table in a list
  calibr_results <- list(LGROUP=group_datalist,SUMMARY=gcf_summary,REP_SUMMARY=rep_stats_table)
  message("Done.")

  message("\n---")
  message("Number of GROUPS: ", length(group_datalist))
  message("Number of sufficent REPs: ", n_sufficentREPS)
  message("")



  return(calibr_results)
}
