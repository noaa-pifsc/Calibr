

#' @name calibur_results
#' @title Returns species specifc data from the reef fish dataset.
#'
#' @description Loads data and splits the observaion table
#'
#' @param SET Coral Reef Dataset
#' @param std_method establishes the standard METHOD of the Reef dataset
#'
#' @export
calibur_results<- function(SET, std_method){


  #If std_method doesn't match any of the two methods, then throw an error
  set_methods <- unique(SET$METHOD)

  if(!any(std_method %in% set_methods)){
    stop("Invalid method name: '", std_method, "' not found in dataset.")
  }else{
    #Set the other method as secondary.
    secondary <- as.character(set_methods[!(set_methods %in% std_method)])

    #Based on renamed METHOD numerical values,
    #Standard method will be the first factor level when METHOD is factorized
    SET[SET$METHOD==std_method,]$METHOD <- paste0("1_",std_method)
    SET[SET$METHOD==secondary,]$METHOD <- paste0("2_",secondary)

  }

  SET$METHOD <- as.factor(SET$METHOD)
  SET$BLOCK  <- as.factor(SET$BLOCK)
  contrasts(SET$METHOD)<-c(1,-1)

  #Split the reef dataset into a list of smaller sets by GROUP value.
  reeffish_datalist <- split(SET, SET$GROUP)

  #Lapply gcf function for all reef species
  reeffish_datalist <- lapply(reeffish_datalist,function(X){
    message("Group: ", unique(X$GROUP))
    tryCatch(
      gcf(X),
      error=function(cond){
        message(unique(X$GROUP) , ": ", trimws(cond), " Returning NA.")
        return(NA)
      },
      warning=function(cond){
        message(unique(X$GROUP) , ": " , trimws(cond))
      }
    )

  })

  calibrated_lgroups <- reeffish_datalist[!(is.na(reeffish_datalist))]
  calibr_results <- suppressMessages(Reduce(function(...)merge(...,all=TRUE),calibrated_lgroups))

  return(calibr_results)
}
