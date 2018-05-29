#Bindings for "global variables"
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("METHOD", "GROUP", "BLOCK", "DENSITY"))
}

#' Gear Calibration Factor summary table, hierarchical model version
#'
#' Sets summary table for the Gear Calibration Factor for all groups analyzed under a single hierarchical, mixed-effect model.
#'
#' @param ORIG Original survey dataset
#' @param std_method Denotes Survey dataset METHOD string as the Standard METHOD
#' @param min_obs Minimum limit for the number of observations.
#' @param n_sample Number of Samples
#'
#' @return List of objects containing the following:
#'
#' \describe{
#'  \item{M.Effect.Pres}{.}
#'  \item{M.Effect.Pos}{.}
#'  \item{GCF.Pres.Detail}{.}
#'  \item{GCF.Pos.Detail}{Data Frame summary of .}
#'  \item{GCFs}{Input fot Calibrate_dataset funct.}
#'  \item{SUMMARY}{Original survey dataset with the calibrated GCFs}
#'}
#'
#' @import data.table
#' @importFrom pbapply pblapply pboptions
#' @importFrom plyr ddply .
#' @importFrom parallel clusterEvalQ makeCluster stopCluster parLapply detectCores
#' @importFrom glmmTMB glmmTMB ranef
#' @importFrom boot inv.logit
#'
#' @export
gcf_glmm <- function (ORIG, std_method, min_obs=10, n_sample=5) {

  ORIG1 <- data.table(ORIG)

  if(n_sample<2){
    stop("Number of samples must be 2 or more.")
  }

  # Filter groups with small positive-observation numbers
  ORIG <- data.table(ORIG)
  Species.list <- table(ORIG[DENSITY>0]$GROUP)
  Species.list <- Species.list[Species.list>=min_obs]
  Species.list <- names(Species.list)
  ORIG <- subset(ORIG, GROUP %in% Species.list)

  #======== Data arrangements==============================================================
  Species.list <- unique(ORIG$GROUP)
  Species.list <- sort(Species.list)
  num_species  <- length(Species.list)
  List.method  <- unique(ORIG$METHOD)
  secondary_method <- List.method[List.method!=std_method]

  ORIG$GROUP  <- as.factor(ORIG$GROUP)
  ORIG$METHOD <- as.factor(ORIG$METHOD)
  ORIG$BLOCK  <- as.factor(ORIG$BLOCK)

  # glmmTMB appears to set it's own contrasts? The Methods contrasts default to -1,1 (can't change to 0,1)
  #contrasts(ORIG$METHOD)<-c(-1,1)
  #contrasts(ORIG$BLOCK)<-"contr.sum"
  #contrasts(ORIG$GROUP)<-"contr.sum"

  #==========Modeling======================================================================
  Results.pres <- data.frame(matrix(ncol=n_sample,nrow=num_species*2+2))
  colnames(Results.pres)       <- paste0("V",formatC(1:n_sample,width=2,flag="0"))
  row.names(Results.pres)[1:2] <- c(std_method,secondary_method)
  for(i in 1:num_species){
    row.names(Results.pres)[i*2+1]   <- paste0(Species.list[i],paste0(":",std_method))
    row.names(Results.pres)[i*2+2]   <- paste0(Species.list[i],paste0(":",secondary_method))
  }
  Results.pos <- data.frame(Results.pres)
  Effects.list <- row.names(Results.pres)

  # Bootstrap section, function for parallel processing
  RunBoot <- function(D){

    D <- data.table(D)

    #Presence and positive models
    lmer.pres <- glmmTMB(PRESENCE~(1|BLOCK/GROUP)+(1|METHOD/GROUP),family=binomial(link="logit"), data=D)
    lmer.pos  <- glmmTMB(log(DENSITY)~(1|BLOCK/GROUP)+(1|METHOD/GROUP), data=D[DENSITY>0])
    Effects.pres <- ranef(lmer.pres)$cond
    Effects.pos  <- ranef(lmer.pos)$cond

    # Store results
    Species.effect.pres  <- rbind(Effects.pres$METHOD, Effects.pres$`GROUP:METHOD`)
    Species.effect.pos   <- rbind(Effects.pos$METHOD, Effects.pos$`GROUP:METHOD`)

    return(list(PRES=Species.effect.pres,POS=Species.effect.pos))
  }

  # Parallel processing section
  pboptions(type= "txt")

  InputList <- list()
  message("Setting up base case ...")
  InputList[[1]] <- ORIG # Base case
  message("Bootstraping the base case ", n_sample-1," time(s) ...")
  for(i in 2:n_sample){
    InputList[[i]] <- ddply(ORIG,.(GROUP, BLOCK, METHOD),function(x) x[sample(nrow(x),replace=TRUE),])
  }

  message("Setting up parallel processing clusters ...")
  no_cores <- detectCores()-1
  cl <- makeCluster(no_cores)

  message("Applying species effects to presence and positive models ...")
  #start <- proc.time()
  #Out <- parLapply(cl,InputList,RunBoot)
  out_time <- system.time( Out <- pblapply(InputList,RunBoot,cl=cl) )
  message("Parallel processing times (in seconds):")
  print(out_time)
  message("Elapsed (per minute): ~", format(round(out_time[3]/60,4), nsmall=2), " min(s)")

  message("Done.")
  stopCluster(cl)

  #END Parallel Processing Section

  for(i in 1:n_sample){

    Results.pres[,i] <- Out[[i]]$PRES[,1][match(rownames(Results.pos), rownames(Out[[i]]$PRES))]
    Results.pos[,i]  <- Out[[i]]$POS[,1][match(rownames(Results.pos), rownames(Out[[i]]$POS))]
  }

  # Final result processing section
  odd_rows   <- seq(1,nrow(Results.pres),2)
  even_rows  <- seq(2,nrow(Results.pres),2)

  Final.pres <- (Results.pres[odd_rows,1:n_sample]-Results.pres[even_rows,1:n_sample])/2
  Final.pos  <- (Results.pos[odd_rows,1:n_sample]-Results.pos[even_rows,1:n_sample])/2

  row.names(Final.pres)[1] <- "Global"
  row.names(Final.pres)[2:(num_species+1)] <- gsub(paste0(":1_",std_method),"",row.names(Final.pres)[2:(num_species+1)])
  row.names(Final.pos)[1] <- "Global"
  row.names(Final.pos)[2:(num_species+1)] <- gsub(paste0(":1_",std_method),"",row.names(Final.pos)[2:(num_species+1)])

  # Sum global and species-specific effects
  for(i in 2:(num_species+1)){
    Final.pres[i,] <- Final.pres[1,] + Final.pres[i,]
    Final.pos[i,]  <- Final.pos[1,] + Final.pos[i,]
  }

  # calculate GCFs
  GCF.pres <- 2*Final.pres
  GCF.pos  <- exp(-Final.pos)/exp(Final.pos)

  # Calculate mean and sd for all parameters
  Sum.pres <- data.frame(matrix(ncol=2,nrow=num_species+1))
  colnames(Sum.pres) <- c("MEAN","SD")
  row.names(Sum.pres) <- row.names(Final.pres)
  Sum.pos      <- data.frame(Sum.pres)
  Sum.GCF.pres <- data.frame(Sum.pres)
  Sum.GCF.pos  <- data.frame(Sum.pres)

  #Sum.pres$MEAN    <- apply(Final.pres[,1:n_sample],1,mean,na.rm=T)
  Sum.pres$MEAN     <- Final.pres[,1] # Assign base case value (first column)
  Sum.pres$SD       <- apply(Final.pres[,1:n_sample],1,sd,na.rm=T)
  #Sum.pos$MEAN     <- apply(Final.pos[,1:n_sample],1,mean,na.rm=T)
  Sum.pos$MEAN      <- Final.pos[,1] # Assign base case value (first column)
  Sum.pos$SD        <- apply(Final.pos[,1:n_sample],1,sd,na.rm=T)
  #Sum.GCF.pres$MEAN<- apply(GCF.pres[,1:n_sample],1,mean,na.rm=T)
  Sum.GCF.pres$MEAN <- GCF.pres[,1] # Assign base case value (first column)
  Sum.GCF.pres$SD   <- apply(GCF.pres[,1:n_sample],1,sd,na.rm=T)
  #Sum.GCF.pos$MEAN <- apply(GCF.pos[,1:n_sample],1,mean,na.rm=T)
  Sum.GCF.pos$MEAN  <- GCF.pos[,1] # Assign base case value (first column)
  Sum.GCF.pos$SD    <- apply(GCF.pos[,1:n_sample],1,sd,na.rm=T)

  Final.GCF           <- cbind(Sum.GCF.pres$MEAN,Sum.GCF.pos$MEAN)
  colnames(Final.GCF) <- c("PRES","POS")
  rownames(Final.GCF) <- rownames(Sum.GCF.pres)

  Final.list <- list(Sum.pres,Sum.pos,Sum.GCF.pres,Sum.GCF.pos,Final.GCF)
  names(Final.list) <- c("M.effect.pres","M.effect.pos","GCF.pres.detail","GCF.pos.detail","GCFs")

  #Convert original dataset to confirm validity of GCFs results
  conversion  <- calibrate_dataset(ORIG1, std_method, Final.list$GCFs)
  conversion  <- data.frame(conversion)

  Final.list[["SUMMARY"]] <- conversion

  #Supress progressbar onexit
  pbo <- pboptions(type = "none")
  on.exit(pboptions(pbo), add = TRUE)

  return(Final.list)
}


