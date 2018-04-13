

#' Create an input dataset object from csv file.
#'
#' Loads a survey dataset from an exising survey dataset \code{*.csv} (comma seperated values) file.
#' This funcion will throw an error if the input dataset does not have the survey
#' dataset column names (\code{BLOCK}, \code{REP}, \code{GROUP}, \code{METHOD}, \code{DENSITY}, \code{PRESENCE}),
#' or it does not not find exactly 2 unique \code{METHOD} values in the input dataset.
#'
#' @param file Location of survey dataset
#'
#' @return The function returns a data.frame table, should validation checks are met.
#'
#' @export
load_dataset<- function (file){

  #Read csv file
  #Set 'as.is' param to FALSE to set METHOD and BLOCK as factors. Convert GROUP column in later step.
  SET <- read.csv(file, as.is = TRUE)

  #Check if columns are present
  colnames_reef_dataset <- c("BLOCK","REP","GROUP","METHOD","DENSITY","PRESENCE")
  invalid_colnames <- character(0)
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
