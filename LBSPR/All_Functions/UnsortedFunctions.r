
# # Selectivity

# SelAgeFun <- function(Ages, type, Shape, Sel50, delta,...) 
	 # {
		 # SelAge <- rep(0, length(Ages))
		 # # Logistic selectivity at age: Ado version
			 # # Two parameters - age at 50% selectivity - Sel50
			 # # Shape - steepess parameter
			 # if (type=="Logistic_Adrian")
			 # {
				 # if (min(Ages)==0) Sel50 <- Sel50 + 1
				 # for (i in seq_along(Ages)) {
					 # SelAge[i] <- (i^Shape)/(Sel50^Shape + i^Shape)
				 # }
			
			 # }

		 # # Logistic selectivity at age: the way the West coast does
			 # # Two parameters - age at 50% selectivity - Sel50
			 # # "Shape" of the asymptotic part: difference between the age at 95% selectivity and the age at 50% selectivity
			 # if (type=="Logistic_Kot")
			 # {
				# SelAge <- 1/(1+exp(-log(19)*(Ages - Sel50)/Shape))
			 # }
			
		# # # Dome shape electivity: the exponential logistic (I didn't choose the double normal because too many parameters)
			# # # Three parameters - age at 50% selectivity - Sel50
			# # # "Shape" of the asymptotic part: difference between the age at 95% selectivity and the age at 50% selectivity
			# # # "delta" controls the degree at which the right tail of the curve descends
			# # if (type=="Dome")
			# # {
				# # SelAge <- (1/(1-delta))*((1-delta)/delta)^delta*exp(-delta*log(19)*(Ages - Sel50)/Shape)/(1+exp(-log(19)*(Ages - Sel50)/Shape))

			# # }		
	 # return(SelAge)	
	 # }
	 
	 




# EstimationFunction <- function(ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass,  StLenMids, MaxIts=1000, 
			# Plot=FALSE, Print=FALSE, StartVec)  {
	# # Estimation Function
	# # Finds parameters that best fit the data
		
	# # Check that number of length classes is the same in input data, and simulated data
	# if (length(ObservedFreqVector) != gennumClass - 1) {
		# return(print("Number of simualted length classes not equal to number of Observed length classes"))
	# }	
	
	
	# # Define empty variables
	# saveFpar <- NULL
	# saveLike <- NULL
	# savegenFM <- NULL
	# saveSelL50 <- NULL
	# saveSelL95 <- NULL
	# testLike <- NULL
	# genFpar <- NULL
	
	# # Find good starting estimate for genAge at 50% selectivity
	# FirstClassCaught <- min(which(ObservedPropVector > 0)) #first length class in catch sample with >0 catch
	# FirstClassLength <- StLenMids[FirstClassCaught +2] # standardised length of first length class

	# X <- 1
	# j <- 1
	
	# # Initial values
	# genSelL50 <- StartVec[1] #  
	# genSelL95 <- genSelL50 * StartVec[2]
	# genFpar   <- StartVec[3]
	# # print(paste("genFpar=", genFpar, sep=""))	
	
	# while(j < MaxIts)  {
			# if (j >= 0.5 * MaxIts) {
				# newgenSelL50 <- StartVec[1] #  
				# newgenSelL95 <- newgenSelL50 * StartVec[2]
				# newgenFpar   <- StartVec[3]
			# }	
			# if (X>1) {
				# Mode <- newgenSelL50
				# a <- 10
				# b <- (a-1)/Mode + 2 - a
				# genSelL50 <- rbeta(1, shape1=a, shape2=b)
				# # print(paste("a= ", a, sep=""))
				# # print(paste("Mode= ", Mode, sep=""))
				# # print(paste("b= ", b, sep=""))
				# # print(paste("genSelL50= ", genSelL50, sep=""))
				# Mode <- newgenSelL95
				# b <- (a-1)/Mode + 2 - a
				# genSelL95 <- rbeta(1, shape1=a, shape2=b)
				# # print(paste("genSelL95= ", genSelL95, sep=""))
				# # print("")
				# # print("")
				# while(round(genSelL95,2) <= round(genSelL50,2)) genSelL95 <- rbeta(1, shape1=a, shape2=b)
				# # print(paste("genSelL50=", genSelL50, sep=""))
				# # print(paste("genSelL95=", genSelL95, sep=""))
				# # print("")
			  # genFpar <- rlnorm(1, log(newgenFpar), 0.5) 
			  # if (genFpar <= 0.01) genFpar <- 0.01
			# } 		
			# genLengthFreqs.output <- GetSimulateLengthFreq(genM, genLinf, genLinfCV, genK, gent0, genAges, genLengths,genFpar, genSelL50, genSelL95, genShape=19, genminLen, genmaxLen, gennumClass)
			# genLengthFreqs.raw <- genLengthFreqs.output[[1]]
			# genLengthFreqs <- genLengthFreqs.output[[2]]
		
			# ##### Calculating the SSQ:
				# Pred <- as.numeric(genLengthFreqs+1e-6)
				# Pred <- Pred/sum(Pred)
				# #Pred.raw <- as.numeric(genLengthFreqs.raw)
				# Obs <- as.numeric(ObservedPropVector+1e-6)
				# Obs <- Obs/sum(Obs)
				# Obs.raw <- as.numeric(ObservedFreqVector)
				# Like <- sum(Obs.raw*log(Obs/Pred)) 
				# if (Like == "NaN") Like <- 1000				
			# ##### Calculating the Likelihood value:
				# # Pred <- as.numeric(genLengthFreqs+1e-6)
				# # Pred.raw <- as.numeric(genLengthFreqs.raw)
				# # Obs <- as.numeric(ObservedPropVector+1e-6)
				# # Obs.raw <- as.numeric(ObservedFreqVector)
				# # Lik <- dmultinom(Obs.raw, prob=Pred)		
				# # ifelse(Lik==0, Like <- 10000, Like <- -log(Lik))
				# # MLE <- NLL
			
			# genFM <- genFpar/genM # generic F/M
			# # print(paste("MLE=", MLE, sep=""))
						
			# testLike[X] <- Like
			# if (testLike[X] > min(testLike) ) j <- j +1
			# if (testLike[X] <= min(testLike) ) {
				# newgenSelL50 	<- genSelL50
				# newgenSelL95 	<- genSelL95
				# newgenFpar   	<- genFpar
				# savegenFM[X] 	<- genFM
				# saveSelL50[X]	<- genSelL50
				# saveSelL95[X] 	<- genSelL95
				# saveFpar[X] 	<- genFpar
				# saveLike[X] 	<- testLike[X]
				# X <- X + 1
				# j <- 0
				# if (Plot == TRUE) {
					# par(mfrow=c(1,1))
					# barplot(Obs)
					# barplot(Pred, add=TRUE, col=rgb(0.2, 0.5, 0.5,0.7))
				# }	
			# }
		# if (Print == TRUE) {
			# print(paste("Num Iterations =", j, sep=" "))
		# }
	# }	
	# smallest <- which(saveLike == min(saveLike)) # best fit parameters
	# Output <- NULL
	# Output$FMest <- savegenFM[smallest]
	# Output$Like  <- min(saveLike)
	# Output$genSelL50    <- saveSelL50[smallest]
	# Output$genSelL95    <- saveSelL95[smallest]
	# Output$genFpar	    <- saveFpar[smallest]
	# Output$genM		    <- genM
	# Output$genK         <- genK
	# Output$genMK	  	<- genM/genK
	# return(Output)
# }
	 
	 
	 
	 
	 
	 

# SimulateRealLengthFrequency <- function(True_MK, M, Linf, assumedLinf, LinfCV, K, t0, Fpar, SelL50, SelL95, Shape=19, minLen, maxLen, numClass, Plot=TRUE, PlotIndiv=TRUE) {

	# # Check that M / K == True_MK
	# if (round(M/K,2) != round(True_MK,2)) {
		# return(print("M/K not equal to True_MK"))
	# }

	# ##########################      Creating biological reality #############################																				
	# MaxAge <- Max.age.Fun(M=M, DefineMaxAge=0.1) # Define maximum age - functions below
	# Ages   <- seq(0, MaxAge, 1)
	# Lengths <- Length.at.Age.Fun(Linf=Linf, K=K, t0=t0, Age=Ages) # von Bert length
	# #------------------------------------------------------------------------------#
	# # Ratios
	# True_FM <- Fpar/M
	# # Selectivity 
	# RelLengths <- Lengths/Linf
	# SelectatSize <- SelLengthFun(Shape,SelL50, SelL95, RelLengths)
	# Linf.Var <- (Linf * LinfCV) ^2
	# Length.StDev  <- sqrt(Linf.Var*(1-exp(-K*(Ages))^2))
	# # Convert to Sel at Age
	# SelectAtAge <- ConvertSelLtoSelAge (Lengths, Length.StDev, Linf, Shape, SelL50, SelL95)
	# #-------------------------------------------------------------------------------#
	# # 	Simulate length frequency of catch fished using above parameters			#
	# #-------------------------------------------------------------------------------#
	# SimCatchLengths <- SimLengthFreq(minLen=minLen, maxLen=maxLen, numClass=numClass, 
					   # Lengths=Lengths, Linf=Linf, M=M, Fpar=Fpar, Ages=Ages, K=K, t0=t0,
					   # LinfCV=LinfCV, Select=SelectAtAge)
	# LengthClasses <- SimCatchLengths$LenClasses
	# LengthMids    <- SimCatchLengths$LengthMids
	# StLenMids	  <- LengthMids/assumedLinf
	# LengthFreqs  <- SimCatchLengths$LengthFreq
	# StLengthFreqs <- SimCatchLengths$LengthProp
	# #LengthFreqs <- table(cut(CatchLengths, LengthClasses)) # Catch length frequencies
	# #StLengthFreqs <- table(cut(CatchLengths/Linf, LengthClasses/Linf)) # Catch length frequencies standardised to Linf
	# #StLengthFreqs <- StLengthFreqs/sum(StLengthFreqs) # Standardised again to frequencies

	# if (Plot == TRUE) {
		# if (PlotIndiv == TRUE) {
			# windows()
			# par(mfrow=c(1,1), ask=TRUE)
			# plot(Ages, Lengths, xlab="Ages", ylab="Length", type="l", bty="l")
			# plot(Ages, SelectAtAge, xlab="Ages", ylab="Selectivity", type="l", bty="l")
			# plot(Ages, SelectAtAge, xlab="Ages", ylab="Selectivity", type="l", bty="l")
			# plot(Lengths, SelectAtAge, xlab="Length", ylab="Selectivity", type="l", bty="l")
			# barplot(LengthFreqs, xlab="Length")
			# barplot(StLengthFreqs, xlab="Relative Length")
		# }
		# else {
			# windows()
			# par(mfrow=c(3,2))
			# plot(Ages, Lengths, xlab="Ages", ylab="Length", type="l", bty="l")
			# plot(Ages, SelectAtAge, xlab="Ages", ylab="Selectivity", type="l", bty="l")
			# plot(Ages, SelectAtAge, xlab="Ages", ylab="Selectivity", type="l", bty="l")
			# plot(Lengths, SelectAtAge, xlab="Length", ylab="Selectivity", type="l", bty="l")
			# barplot(LengthFreqs, xlab="Length")
			# barplot(StLengthFreqs, xlab="Relative Length")
		# }	
	# }
	# Output <- NULL
	# Output$LengthMids <- LengthMids
	# Output$StLengthMids <- StLenMids
	# Output$LengthFreqs <- LengthFreqs
	# Output$StLengthFreqs <- StLengthFreqs
	# Output$LengthClasses <- LengthClasses
	# return(Output)
# }






# # Function to output simulated length frequencies from generic parameters
# GetSimulateLengthFreq <- function (genM, genLinf, genLinfCV, genK, gent0, genAges, genLengths,
							# genFpar, genSelL50, genSelL95, genShape=19, genminLen, genmaxLen,gennumClass)
	# { 
		# # Selectivity
		  # genSelectAtLength <- SelLengthFun(genShape, genSelL50, genSelL95, Lengths=genLengths/genLinf) 
		  # genLinf.Var <- (genLinf * genLinfCV) ^2
		  # genLength.StDev  <- sqrt(genLinf.Var*(1-exp(-genK*(genAges))^2))
		  # # Convert to Sel at Age
		  # genSelectAtAge <- ConvertSelLtoSelAge (genLengths, genLength.StDev, genLinf, genShape, genSelL50, genSelL95) 
		  
		  # genSimCatchLengths <- SimLengthFreq(minLen=genminLen, maxLen=genmaxLen, numClass=gennumClass, 
						   # Lengths=genLengths, Linf=genLinf, M=genM, Fpar=genFpar, Ages=genAges, 
						   # K=genK, t0=gent0, LinfCV=genLinfCV, Select=genSelectAtAge)
		  # genLengthClasses <- genSimCatchLengths$LenClasses
		  # genLengthMids    <- genSimCatchLengths$LengthMids
		  # genStLengthFreqs.raw  <- genSimCatchLengths$LengthFreq
		  # genStLengthFreqs <- genSimCatchLengths$LengthProp
		  # #genLengthFreqs   <- table(cut(genCatchLengths, genLengthClasses)) # Catch length frequencies
		# # raw frequency output  
		  # genStLengthFreqs.raw 
		# # std prob density function output
		  # genStLengthFreqs <- genStLengthFreqs/sum(genStLengthFreqs)
		
		# OUTPUT <- list(genStLengthFreqs.raw, genStLengthFreqs)
		# return(OUTPUT)

	# }
		 