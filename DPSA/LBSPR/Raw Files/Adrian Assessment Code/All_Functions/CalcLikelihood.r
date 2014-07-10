
FindLikeFunction <- function(ObservedFreqVector, ObservedPropVector, genM, genLinf, genLinfCV, genLengthSD,  genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass,   
			ParVec, LikePlot=TRUE)  
{
	# Check that number of length classes is the same in input data, and simulated data
	if (length(ObservedFreqVector) != gennumClass - 1) {
		return(print("Number of simulated length classes not equal to number of Observed length classes"))
	}	
	# Parameter Values
	genSelL50 <- ParVec[1]  
	genSelL95 <- ParVec[2]
	genFpar   <- ParVec[3]
	# print(c(genSelL50, genSelL95, genFpar) )
	
	if (genSelL95 <= genSelL50)   return(100000)
	if (genSelL50 <0.01)  		  return(100000)
	if (genSelL95 <0.05)  		  return(100000)
	if (genFpar < 0) 		      return(100000)
	
	genSelatLength 	<- SelLengthFun(Shape=19,genSelL50, genSelL95 , genLengths) 

	genSelatAge 	<- ConvertSelLtoSelAge(MeanLatAge=genLengths, LatAgeStDev=genLengthSD, genLinf, Shape=19, relSelL50=genSelL50, relSelL95=genSelL95) 

	genLengthFreqs.output 	<- SimLengthFreq(minLen=genminLen, genmaxLen, gennumClass, genLengths, LengthStDev=genLengthSD, genM, genFpar, genAges, SelectatAge=genSelatAge, genSelL50, genSelL95)
	genLengthFreqs.raw 		<- genLengthFreqs.output$LengthFreq
	genLengthFreqs 			<- genLengthFreqs.output$LengthProp
		
	##### Calculating the Likelihood:		# THIS CAN BE CHANGED TO WHATEVER METHOD WE LIKE
	Pred <- as.numeric(genLengthFreqs+1e-6)
	Pred <- Pred/sum(Pred)
	Obs <- as.numeric(ObservedPropVector+1e-6)
	Obs <- Obs/sum(Obs)
	Obs.raw <- as.numeric(ObservedFreqVector)
	Like <- sum(Obs.raw*log(Obs/Pred)) 
	if (Like == "NaN") Like <- 100000				
				
	genFM <- genFpar/genM # generic F/M
	if (LikePlot==TRUE) {
			temp <- barplot(Obs)
			# lines(temp, Pred, lty=2, lwd=3)
			barplot(Pred, add=TRUE, col=rgb(0.2, 0.5, 0.5, 0.7))
	}
	Output <- NULL
	Output <- Like
return(Output)
}
