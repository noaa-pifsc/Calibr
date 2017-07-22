

#' @name get_species_dataset
#' @title Returns species specifc data from the reef fish dataset.
#'
#' @description Loads data and splits the observaion table
#'
#' @param file Reef Fish Dataset Filename
#' @param species Coral Reef species code
#' @param std_method establishes the standard METHOD of the Reef dataset
#'
#' @export
get_species_dataset<- function(SET, species, std_method){

  SET <- SET[SET$GROUP %in% species]

  SET <- SET[order(SET$METHOD)]

  #If std_method doesn't match any of the two methods, then throw an error
  set_methods <- unique(SET$METHOD)

  if(!any(std_method %in% set_methods)){
    stop("Invalid method name: '", std_method, "' not found in dataset.")
  }else{
    #Set the other method as secondary.
    secondary <- as.character(set_methods[!(set_methods %in% std_method)])

    #Based on renamed METHOD numerical values,
    #Standard method will be the first factor level when METHOD is factorized
    SET[METHOD==std_method]$METHOD <- paste0("1_",std_method)
    SET[METHOD==secondary]$METHOD <- paste0("2_",secondary)

  }

  SET$METHOD <- as.factor(SET$METHOD)
  SET$BLOCK  <- as.factor(SET$BLOCK)
  contrasts(SET$METHOD)<-c(1,-1)

  return(SET)
}
