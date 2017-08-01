

#' @name load_reef_dataset
#' @title Loads reef survey dataset-
#'
#' @description
load_reef_dataset<- function (file){

  #Read csv file
  #Set 'as.is' param to FALSE to set METHOD and BLOCK as factors. Convert GROUP column in later step.
  SET <- read.csv(file, as.is = TRUE)

  #Check if columns are present
  colnames_reef_dataset <- c("BLOCK","REP","GROUP","METHOD","DENSITY","PRESENCE")
  if(!(all(names(SET) %in% colnames_reef_dataset))){
    invaild_colnames <- names(SET)[!(names(SET) %in% colnames_reef_dataset)]
    stop("Invaild column names found: ", invalid_colnames)
  }

  #Check METHOD
  if(length(unique(SET$METHOD)) != 2) {
    stop("Number of unique methods in METHODS column is not 2")
  }



  return(SET)
}
