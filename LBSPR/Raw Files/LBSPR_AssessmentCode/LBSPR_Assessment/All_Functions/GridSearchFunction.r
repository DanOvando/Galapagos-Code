
RunGridSearchFunction <- function(LowerSelL50, UpperSelL50, LowerSelL95, UpperSelL95, InitialSelPrec, FinalSelPrec, LowerF_M, UpperF_M, InitialF_MPrec, FinalF_MPrec,
		ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV, genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, GridSearchResultPlot, LikePlot=FALSE)
{
  # The Grid Search is in two stages. (More stages could be added if needed)
  # Stage 1 searches over a wide range of parameter space, but with reasonably large distances between each parameter
  # Stage 2 searches over a small range of parameter space around the best fit values from Stage 1, but with a much smaller increment between parameter values
  ###########
  # Stage 1 #
  ###########  
  # Set up Stage 1 Search Grid
  IntiSel50Vec <- seq(from=LowerSelL50, to=UpperSelL50, by= InitialSelPrec)
  IntiSel95Vec <- seq(from=LowerSelL95, to=UpperSelL95, by= InitialSelPrec)
  InigenFVec   <- seq(from=LowerF_M * genM, to=UpperF_M * genM, by=InitialF_MPrec*genM)    
  
  Stage1Grid <- expand.grid(IntiSel50Vec, IntiSel95Vec, InigenFVec)
  RealisticSelL95 <- which(Stage1Grid[,2] > Stage1Grid[,1]) # Identify realistic SelL95. i.e. SelL95 > SelL50
  Stage1Grid <- Stage1Grid[RealisticSelL95,]
  
  Stage1Grid <- as.matrix(Stage1Grid) # Probably a much better way to convert it to be accepted by the sapply function, but this is my workaround for the moment
  NumRows <- nrow(Stage1Grid)
 
  Stage1Likes <- NULL
  bestLikes1 <- NULL
  stm <- proc.time()
  print("Stage 1 Started")
  print(paste("Stage 1 Grid. Number of rows =", NumRows, sep="")) 
  Stage1Likes <- sapply(1:NumRows, function (X) FindLikeFunction(ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV, genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, ParVec=Stage1Grid[X,], LikePlot)) 
									
  Stage1Time <- round((proc.time() - stm)[3],0)
  print("Stage 1 Completed")
  print(paste("Stage 1 duration: ", Stage1Time, " seconds", sep=""))
  
  # Set up results of Stage 1 in a matrix. Only best fit for each parameter is saved
  Stage1SelL50 <- unique(Stage1Grid[,1])
  Stage1SelL95 <- unique(Stage1Grid[,2])
  Stage1genF   <- unique(Stage1Grid[,3])
  Stage1SelL50Mat <- matrix(NA, length(Stage1SelL50), 2)
  Stage1SelL95Mat <- matrix(NA, length(Stage1SelL95), 2)
  Stage1genFMMat   <- matrix(NA, length(Stage1genF), 2)
  Stage1SelL50Mat[,1] <- Stage1SelL50
  Stage1SelL95Mat[,1] <- Stage1SelL95
  Stage1genFMMat[,1]  <- Stage1genF/genM
  
  for (j in seq_along(Stage1SelL50)) {
    temp <- which(Stage1Grid[,1]== Stage1SelL50[j]) 
    Stage1SelL50Mat[j,2] <- min(Stage1Likes[temp])
  }
   for (j in seq_along(Stage1SelL95)) {
    temp <- which(Stage1Grid[,2]== Stage1SelL95[j]) 
    Stage1SelL95Mat[j,2] <- min(Stage1Likes[temp])
  }
   for (j in seq_along(Stage1genF)) {
    temp <- which(Stage1Grid[,3]== Stage1genF[j]) 
    Stage1genFMMat[j,2] <- min(Stage1Likes[temp])
  }
  
   bestLikes1  <- min(Stage1genFMMat[,2])

	
  if (GridSearchResultPlot == TRUE) {
    par(mfrow=c(2,3))
    # YLim <- c(floor(min(Stage1Likes)), min(500, abs(100*min(Stage1Likes))))
	plot(Stage1SelL50Mat, bty="l", xlab="Sel L50", ylab="Likelihood", type="b")  
	plot(Stage1SelL95Mat, bty="l", xlab="Sel L95", ylab="Likelihood", type="b") 
	plot(Stage1genFMMat, bty="l", xlab="F/M", ylab="Likelihood", type="b") 
  }
  MinStage1Likes <- min(Stage1Likes)
  Stage1BestValues <- Stage1Grid[Stage1Likes==MinStage1Likes, ]
  Stage1BestValues[3] <- Stage1BestValues[3]/genM
  Stage1BestValues[4] <- bestLikes1  
  names(Stage1BestValues) <- c("SelL50 ", "SelL95 ", "F/M est", "Like")
  
  ###########
  # Stage 2	#
  ###########
  # Setup Stage 2 Grid
  SelRange <- InitialSelPrec
  FparRange <- InitialF_MPrec 

  Stage2Sel50Vec <- seq(from= max(round(Stage1BestValues[1],2) - SelRange,0.1), to= min(round(Stage1BestValues[1],2) + SelRange, 1), by=FinalSelPrec)
  Stage2Sel95Vec <- seq(from= max(round(Stage1BestValues[2],2) - SelRange,0.1), to= min(round(Stage1BestValues[2],2) + SelRange, 1), by=FinalSelPrec)
  Stage2genFVec  <- seq(from= max(round(Stage1BestValues[3],2) - FparRange, 0.1)* genM, to= min(round(Stage1BestValues[3],2) + FparRange, 30) * genM, by=FinalF_MPrec * genM)

  Stage2Grid <- expand.grid(Stage2Sel50Vec, Stage2Sel95Vec, Stage2genFVec)
  RealisticSelL95 <- which(Stage2Grid[,2] > Stage2Grid[,1]) # Identify realistic SelL95. i.e. SelL95 > SelL50
  Stage2Grid <- Stage2Grid[RealisticSelL95,]
  
  Stage2Grid <- as.matrix(Stage2Grid) # Probably a much better way to convert it to be accepted by the sapply function, but this is my workaround for the moment
  Stage2Grid <- as.array(Stage2Grid)
  
  Stage1Likes <- NULL
  bestLikes2 <- NULL
  stm <- proc.time()
  print("Stage 2 Started")
  print(paste("Stage 2 Grid. Number of rows =", nrow(Stage2Grid), sep="")) 
  Stage2Likes <- sapply(1:nrow(Stage2Grid), function (X) FindLikeFunction(ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV,genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, ParVec=Stage2Grid[X,], LikePlot))  	
  Stage2Time <- round((proc.time() - stm)[3],0)
  print("Stage 2 Completed")
  print(paste("Stage 2 duration: ", Stage2Time, " seconds", sep=""))
  
  # Set up results of Stage 2 in a matrix. Only best fit for each parameter is saved
  Stage2SelL50 <- unique(Stage2Grid[,1])
  Stage2SelL95 <- unique(Stage2Grid[,2])
  Stage2genF   <- unique(Stage2Grid[,3])
  Stage2SelL50Mat <- matrix(NA, length(Stage2SelL50), 2)
  Stage2SelL95Mat <- matrix(NA, length(Stage2SelL95), 2)
  Stage2genFMMat   <- matrix(NA, length(Stage2genF), 2)
  Stage2SelL50Mat[,1] <- Stage2SelL50
  Stage2SelL95Mat[,1] <- Stage2SelL95
  Stage2genFMMat[,1]   <- Stage2genF / genM
  
  for (j in seq_along(Stage2SelL50)) {
    temp <- which(Stage2Grid[,1]== Stage2SelL50[j]) 
    Stage2SelL50Mat[j,2] <- min(Stage2Likes[temp])
  }
   for (j in seq_along(Stage2SelL95)) {
    temp <- which(Stage2Grid[,2]== Stage2SelL95[j]) 
    Stage2SelL95Mat[j,2] <- min(Stage2Likes[temp])
  }
   for (j in seq_along(Stage2genF)) {
    temp <- which(Stage2Grid[,3]== Stage2genF[j]) 
    Stage2genFMMat[j,2] <- min(Stage2Likes[temp])
  }
  
   bestLikes2  <- min(Stage2genFMMat[,2])
    
  if (GridSearchResultPlot == TRUE) {
	plot(Stage2SelL50Mat, bty="l", xlab="Sel L50", ylab="Likelihood", type="b")  
	plot(Stage2SelL95Mat, bty="l", xlab="Sel L95", ylab="Likelihood", type="b") 
	plot(Stage2genFMMat, bty="l", xlab="F/M", ylab="Likelihood", type="b") 
  }
  MinStage2Likes <- min(Stage2Likes)
  tempNum <- which(Stage2Likes==MinStage2Likes)
  Stage2BestValues <- Stage2Grid[tempNum[1], ]
  
  Stage2BestValues[3] <- Stage2BestValues[3]/genM
  Stage2BestValues[4] <- bestLikes2
  names(Stage2BestValues) <- c("SelL50 ", "SelL95 ", "F/M est", "Like")

  ##################
  # Stage 3: Optim #
  ##################
  stm <- proc.time()
  print("Stage 3 Started")
  print(paste("Stage 3 = optim search from stage 2 value")) 

	RunLike <- function(ParVec,...) {
	  temp <- FindLikeFunction(ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV,genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, ParVec, LikePlot)
	return(temp)
	}
  stt <- Stage2Grid[Stage2Likes==MinStage2Likes, ]
  # res <- optim(stt, RunLike, hessian=T, method="L-BFGS-B", lower=c(0.01,0.02,0.001), upper=c(0.98,0.999,4)) # Changed method
  # Stage3BestValues <- c(res$par[1:2], res$par[3]/genM, res$value)
  res <- nlm(RunLike, p=stt,  hessian=T, steptol=1e-4, gradtol=1e-4)
  Stage3BestValues <- c(res$estimate[1:2], res$estimate[3]/genM, res$minimum)
  Stage3Time <- round((proc.time() - stm)[3],0)
  print("Stage 3 Completed")
  print(paste("Stage 3 duration: ", Stage3Time, " seconds", sep=""))
  
  Output <- NULL
  Output$Stage1$Sel50Mat   <- Stage1SelL50Mat
  Output$Stage1$Sel95Mat   <- Stage1SelL95Mat
  Output$Stage1$genFMat    <- Stage1genFMMat
  Output$Stage1$BestEstimates <- Stage1BestValues
  Output$Stage1$ModTime		<- Stage1Time
  
  Output$Stage2$Sel50Mat   <- Stage2SelL50Mat
  Output$Stage2$Sel95Mat   <- Stage2SelL95Mat
  Output$Stage2$genFMat    <- Stage2genFMMat
  Output$Stage2$BestEstimates <- Stage2BestValues
  Output$Stage2$ModTime		<- Stage2Time

  Output$Stage3$Sel50Mat   <- res$par[1]
  Output$Stage3$Sel95Mat   <- res$par[2]
  Output$Stage3$genFMat    <- res$par[3]/genM
  Output$Stage3$BestEstimates <- Stage3BestValues
  Output$Stage3$ModTime		<- Stage3Time
  Output$Stage3$Hessian     <- res$hessian
  
  return(Output)
}


