# Data Collection

SimLength_Sample <- function (CatchAge, minLen, maxLen, numClass, Lengths, LengthSD,  Linf, M, Fpar, Ages, K, t0, LinfCV, SelatAge, SampPerc, SelL50, SelL95) {
  
  # Sample from Catch Ages
  # set.seed(101)
  CatchAgeMat <- matrix(NA, length(Ages), 2)
  CatchAgeMat[,1] <- Ages
  CatchAgeMat[,2] <- round(CatchAge,0)
  CatchAgeVec <- list()
  for (x in seq_along(CatchAgeMat[,1])) {
    CatchAgeVec[[x]] <- rep(CatchAgeMat[x,1], CatchAgeMat[x,2])
  }
  CatchAgeVec 	<- unlist(CatchAgeVec)
  TotCatch	  	<- length(CatchAgeVec)
  
  n <- sum(CatchAge)*SampPerc
  print(paste("Sample size =", round(n,0), sep=""))

  SampCatchAge_KO <- sample(Ages, size=n, replace=TRUE, prob=CatchAge/sum(CatchAge)) 
  
  # par(mfrow=c(4,1),mar=c(2,2,2,2))
  # barplot(CatchAge, xlim=c(0,50),main="Barplot of catchatage (original)")
  # hist(CatchAgeVec, breaks=20, xlim=c(0,50),main="Histogram of catchatage with rounding")
  # hist(SampCatchAge, breaks=20, xlim=c(0,50),main="Histogram of sampled catchatage with rounding")
  # hist(SampCatchAge_KO, breaks=20, xlim=c(0,50),main="Histogram of sample catchatage withOUT rounding")
  

  # Get New Catch Age Frequency
  SampleCatchAgeFreq <- NULL
  
  SampleCatchAgeFreq_KO <- NULL
  for (x in seq_along(Ages)) {
	i <- x-1
    # SampleCatchAgeFreq[x]  <- sum(SampCatchAge==i) 
    SampleCatchAgeFreq_KO[x]  <- sum(SampCatchAge_KO==i) 
  }
  
  # par(mfrow=c(2,1))
  # barplot(CatchAge, xlim=c(0,70),main="Barplot of catchatage (original)")
   # barplot(SampleCatchAgeFreq_KO, xlim=c(0,70),main="Barplot of catchatage (original)")
   
  # Conditional probablities
  LengthVec <- seq(0, 3 * Linf, 1)
  ProbMat <- matrix(NA, nrow=length(LengthVec), ncol=length(Ages))
  for (i in seq_along(Ages)) {
	ProbMat[,i] <- dnorm(LengthVec, mean=Lengths[i], sd=LengthSD[i])
  }
  # # print(Lengths)
  SelectLengthClasses <- SelLengthFun(Shape=19, SelL50, SelL95, Lengths=LengthVec) 
  # print(which(is.na(ProbMat)==TRUE))
  # print(c(SelL50, SelL95))
  Prob2Mat <- ProbMat * SelectLengthClasses	# adjust probabilty of length at age for selectivity at length
   # print(which(is.na(Prob2Mat)==TRUE))

  StProb2Mat <- Prob2Mat
  ColSums <- apply(Prob2Mat, 2, sum)
  # print(ColSums)
  for (i in seq_along(Ages)) {
    StProb2Mat[,i] <- Prob2Mat[,i]/ColSums[i] # Normalise probabilities
  }

  # print(which(is.na(StProb2Mat)==TRUE))
  # Assign random lengths to catch at age
  LengthList <- rep(list(NA), length(Ages))
  LengthList_KO <- rep(list(NA), length(Ages))

  for (i in seq_along(Ages)) {
    # LengthList[[i]] <- sample(LengthVec, SampleCatchAgeFreq[i], replace=TRUE, prob=StProb2Mat[,i])  
   LengthList_KO[[i]] <- sample(LengthVec, SampleCatchAgeFreq_KO[i], replace=TRUE, prob=StProb2Mat[,i])  
  }

  # AllLengths  <- unlist(LengthList)
  AllLengths  <- unlist(LengthList_KO)
  
  # Calculate Frequencies
  LenClasses <- seq(minLen, maxLen, length=numClass)
  LenMids <- seq(LenClasses[1] +((LenClasses[2]-LenClasses[1])/2), by=(LenClasses[2]-LenClasses[1]), length=numClass-1)

  LengthFreqs <- cut(AllLengths, LenClasses)
  LengthFreqs <- table(LengthFreqs)
  
  CatchLengths <- LengthFreqs
  Pcatchlength <- LengthFreqs/sum(LengthFreqs)

	Output <- list(LengthMids=LenMids, LenClasses = LenClasses, LengthFreq = CatchLengths, LengthProp=Pcatchlength)
return(Output)
}












# Older code


# # Data Collection

# SimLength_Sample <- function (CatchAge, minLen, maxLen, numClass, Lengths, LengthSD,  Linf, M, Fpar, Ages, K, t0, LinfCV, SelatAge, SampPerc) {
  
  # # Sample from Catch Ages
  # CatchAgeMat <- matrix(NA, length(Ages), 2)
  # CatchAgeMat[,1] <- Ages
  # CatchAgeMat[,2] <- round(CatchAge,0)
  # CatchAgeVec <- list()
  # for (x in seq_along(CatchAgeMat[,1])) {
    # CatchAgeVec[[x]] <- rep(CatchAgeMat[x,1], CatchAgeMat[x,2])
  # }
  # CatchAgeVec 	<- unlist(CatchAgeVec)
  # TotCatch	  	<- length(CatchAgeVec)
  # PROB 			<- rep(0, length(Ages))
  # for (i in seq_along(Ages)) {
    # PROB[i] <- length(which(CatchAgeVec == Ages[i]))
  # }
  # PROB 			<- as.vector(PROB/sum(PROB))
  # PROB2			<- rep(0, length(CatchAgeVec))
  # Loc 	<- sapply(seq_along(Ages), function (i) which(CatchAgeVec==Ages[i]))
  # for (i in seq_along(Ages)) {
    # PROB2[Loc[[i]]] <- PROB[i] 
  # }
  # SampCatchAge <- sample(CatchAgeVec, TotCatch*SampPerc, replace=FALSE, prob=PROB2)
  
  # # Get New Catch Age Frequency
  # SampleCatchAgeFreq <- NULL
  # for (x in seq_along(Ages)) {
	# i <- x-1
    # SampleCatchAgeFreq[x]  <- sum(SampCatchAge==i) 
  # }
  
  # # Convert to length frequency for assessment
  # LenClasses <- seq(minLen, maxLen, length=numClass)
  # LenMids <- seq(LenClasses[1] +((LenClasses[2]-LenClasses[1])/2), by=(LenClasses[2]-LenClasses[1]), length=numClass-1)
  # Prob <- matrix(0, nrow=(numClass-1), ncol=length(Ages))
	# for (i in seq_along(Ages)) 
		# {
		  # for (lengthclass in seq_along(LenClasses)[-1])
			# {	
			  # length_cat <- lengthclass-1
			  # if(length_cat==1) Prob[length_cat,i] <- pnorm(LenClasses[lengthclass], Lengths[i], LengthSD[i]) 
			  # if(length_cat>1) Prob[length_cat,i] <- pnorm(LenClasses[lengthclass], Lengths[i], LengthSD[i]) - pnorm(LenClasses[lengthclass-1], Lengths[i], LengthSD[i]) 			
			# }
			# #Prob[,i] <- Prob[,i]/sum(Prob[,i])
		# }
	# SelectLengthClasses <- SelLengthFun(Shape=19, SelL50, SelL95, Lengths=LenMids) 
    # Prob2 <- Prob * SelectLengthClasses	# adjust probabilty of length at age for selectivity at length
	# #image.plot(Ages, 1:(numClass-1), t(Prob), ylab="Length category")
	# CatchLengths_mat <- SampleCatchAgeFreq*t(Prob2)
	# CatchLengths <- apply(CatchLengths_mat, 2, sum)
	# Pcatchlength <- apply(CatchLengths_mat, 2, sum)/sum(CatchLengths)

	# Output <- list(LengthMids=LenMids, LenClasses = LenClasses, LengthFreq = CatchLengths, LengthProp=Pcatchlength)
# return(Output)
# }
