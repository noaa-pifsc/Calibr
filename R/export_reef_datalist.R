
#' Export reef data and summaries to data files.
#'
#' Option to export grouped reeffish datasets, Reefish Group Summary Table, and REP summaries in
#' seperate data files.
#'
#' @param datalist Calibr datalist
#'
#' @export
export_reef_datalist <- function(datalist){

  if(class(datalist) != "list"){
    stop("Parameter datalist not a list object.")
  }

  #Check if datalist has the names "LGROUP", "SUMMARY", and "REP_SUMMARY" in datalist
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




}
