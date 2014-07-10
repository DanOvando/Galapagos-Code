LBSPR_SingleSpeciesAssessmentfun<-function(CatchatLength,AssessDir,CurrentDir, SpFile)
                         {
  
  ##############################
  # Read in Assumed Parameters #
  ##############################
  
  SpeciesName <- as.character(SpFile[1, 1 +1])
  assumedLinf  <- as.numeric(as.character(SpFile[2, 1 +1])) 
  assumedMK  <- as.numeric(as.character(SpFile[3, 1 +1]))
  genM    <-   0.1 # as.numeric(as.character(SpFile[4, 1 +1]))
  genLinf		<- as.numeric(as.character(SpFile[5, 1 +1]))
  genLinfCV	<- as.numeric(as.character(SpFile[6, 1 +1]))
  gent0		<- as.numeric(as.character(SpFile[7, 1 +1]))
  MatType		<- as.character(SpFile[8, 1 +1])
  MatL50		<- as.numeric(as.character(SpFile[9, 1 +1]))
  MatL95		<- as.numeric(as.character(SpFile[10, 1 +1]))
  Wbeta		<- as.numeric(as.character(SpFile[11, 1 +1]))
  genK    <- genM/assumedMK
  minLen  <- as.numeric(as.character(SpFile[17, 1 +1]))
  maxLen  <- as.numeric(as.character(SpFile[18, 1 +1]))
  
  #################################################################
  # Convert Data received to appropriate format        
  #################################################################
  
  LengthClasses <- seq(minLen,maxLen,by = LengthBins)
  LengthMids <- seq(LengthClasses[1] +((LengthClasses[2]-LengthClasses[1])/2), by=(LengthClasses[2]-LengthClasses[1]), length=length(LengthClasses)-1)
  LenFreq <- hist(CatchatLength,breaks=seq(minLen,maxLen,by=LengthBins),plot=FALSE)$counts
  LenProp <- as.vector(LenFreq/sum(LenFreq))
  
  ########################################
  # Set final params to pass to ADMB file#    
  ########################################
  
  MK <- assumedMK
  LinfTest <- assumedLinf
  LinfCV <- genLinfCV
  PercLeft <- 0.01
  NumAgClass <- 100
  
  ###########################################
  # Set working directory and run assessmemt#    
  ###########################################
  
  ##Set working directory to Assess File
  setwd(AssessDir)
  Vals <- c(0.5, 0.1, 0.5)   # starting values for fitting Sel50, Diff between Sel50 and Sel 95, F)
  ## Function that writes the .DAT file needed to run admb file
  WriteDat(MK, LinfTest, LinfCV, PercLeft, NumAgClass, LengthMids, LengthClasses, LenFreq, LenProp, AssessDir) 
  ## Function to pass starting values to admb file
  WritePin(AssessDir, Vals)
  ## Run the admb function
  run_admb("LBSPR_AssessFun") 
  
  #ADMBFile <- paste(AssessDir, "/LBSPR_AssessFun", sep="")
  #system(ADMBFile)
  
  ###########################################
  # Add in failure checks
  ###########################################
  CheckFileExists <- file.exists("admodel.cov")
  ModelFailed <- as.logical(1 - CheckFileExists)
  
  ##If ModelFailed == TRUE, Run with different starting values in a while loop until max 
  #iterations is reached or Model works
  MaxReached     <- FALSE
  counter   	<- 0
  while (ModelFailed & !MaxReached) {
    #MostRec <- max(which(!is.na(PreviousResults[,1])))
    TryEstimates <- c(rnorm(1, 0.5, 0.1), rnorm(1, 0.1, 0.05), rlnorm(1, log(1), 0.4)) 
    WritePin(AssessDir, TryEstimates)
    run_admb("LBSPR_AssessFun") 
    
    CheckFileExists <- file.exists("admodel.cov")
    ModelFailed <- as.logical(1 - CheckFileExists)
    estFMs <- exp(read.table("lbspr_assessfun.par")[3,1])
    if (estFMs < 0.01 | estFMs > 10) ModelFailed <- TRUE
    counter <- counter + 1
    print(counter)
    if (counter == 80) MaxReached <- TRUE
  }
  print(c("ModelFailed: ", ModelFailed))
  
  ###########################################
  # Unpack parameters#    
  ###########################################
  Outputs<-NULL
  Outputs$SelL50<-read.table("lbspr_assessfun.par")[1,1]*assumedLinf
  Outputs$SelL95<-read.table("lbspr_assessfun.par")[2,1]*assumedLinf + read.table("lbspr_assessfun.par")[1,1]*assumedLinf
  if (ModelFailed & MaxReached){
    Outputs$EstFM<- NA  #If F estimation never converged, No estimate for that parameter
  } else {
    Outputs$EstFM <- exp(read.table("lbspr_assessfun.par")[3,1])
  }
  Outputs$EstSPR <- EstimateSPR(estMK=assumedMK, estFM=Outputs$EstFM, genM, gent0, genSelL50=Outputs$SelL50/assumedLinf, 
                                genSelL95=Outputs$SelL95/assumedLinf, genLinf, genLinfCV, assMatType=MatType, 
                                genMatL50=MatL50/assumedLinf, genMatL95=MatL95/assumedLinf, assFecType=1,Wbeta)
  

  ######################################################
  # Cleanup Files and return to current directory  
  ######################################################
  DeleteFiles(AssessDir)
  setwd(CurrentDir)
  
  return(Outputs)
}