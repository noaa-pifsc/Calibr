

#' @name load_reef_dataset
#' @title Returns the reef survey dataset object from csv file.
#'
#' @description Loads the Coral Reef Reef Survey dataset from comma seperated values formatted file.
#' This funcion will throw an error if either the input dataset does not have the NOAA-CRED Pacific-wide
#' dataset column names ("BLOCK","REP","GROUP","METHOD","DENSITY","PRESENCE"), or it does not not find
#' exactly 2 unique METHOD values in the input dataset.
#'
#' @param file Reef survey dataet filename
#'
#' @export
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
