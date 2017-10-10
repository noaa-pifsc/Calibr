
#' Export reef data and summaries to data files.
#'
#' Option to export grouped reeffish datasets, Reefish Group Summary Table, and REP summaries in
#' seperate data files.
#'
#' @param datalist Calibr datalist
#' @param outdir Location to export data and summaries to. Default location is user's \code{HOME} directory
#'
#' @importFrom utils write.csv
#'
#' @export
export_reef_datalist <- function(datalist,outdir=Sys.getenv("HOME")){

  #Check datalist is a list
  if(class(datalist) != "list"){
    stop("Parameter datalist not a list object.")
  }

  #Check datalist contents have the appropriate data structures.
  str.datalist <- list("list",
                       c("data.table","data.frame"),
                       "data.frame")
  if(!all(sapply(datalist,class) %in% str.datalist)){
    index_mismatch_classes <- !(sapply(datalist,class) %in% str.datalist)
    mismatched_classes <- sapply(datalist,class)[index_mismatch_classes]
    count_mismatches<- length(which(index_mismatch_classes))

    mismatch_datalist_obj <- names(datalist)[index_mismatch_classes]
    stop("Datalist object(s) ", paste(mismatch_datalist_obj, collapse=", ")
         , "has an invalid object class:", paste(mismatched_classes,collapse=", "))

  }

  #Check if datalist has the names "LGROUP", "SUMMARY", and "REP_SUMMARY" in datalist
  if(!all(names(datalist) %in% c("LGROUP", "SUMMARY", "REP_SUMMARY" ))){
     missing_listNames <-names(datalist)[!(names(datalist) %in% c("LGROUP", "SUMMARY", "REP_SUMMARY" ))]
     stop("Invalid reef datalist fields: ", paste(missing_listNames,collapse = ", "))
  }

  #If dir is default, create an output subdirectory from the user's HOME location.
  if(outdir == Sys.getenv("HOME")){
    outdir <- gsub("\\\\","",tempfile(pattern="CalibrRun_",tmpdir= file.path(Sys.getenv("R_USER"),"\\")))
    message("Creating output directory: ", outdir ," ...")
    dir.create(outdir)
  }else{
    #check if outdir exists in user's system. If not return error message
    if(!(dir.exists(outdir))){
      stop("Location of output directory was not found or inaccessible : \n",outdir)
    }
    message("Output Directory: ", outdir)
  }
  message("Exporting grouped data and summary tables to output directory ...")

  #LGROUP
  message("Saving Grouped Coral Reef Data ...")
  reeffish_grouped_datalist <- datalist[["LGROUP"]]
  saveRDS(reeffish_grouped_datalist, file = file.path(outdir,"reeffish_grouped.Rds"))

  #SUMMARY
  message("Saving GCF and OPUE Summaries to file ...")
  write.csv(datalist[["SUMMARY"]], file = file.path(outdir,"summary_table.csv"), row.names = FALSE)

  #REP_SUMMARY
  message("Saving REP Summary Table to file ...")
  write.csv(datalist[["REP_SUMMARY"]], file = file.path(outdir,"REP_summary.csv"), row.names = FALSE)

  message("Done.")
  message("Grouped data and summary tables have been to:\n", outdir)

}
