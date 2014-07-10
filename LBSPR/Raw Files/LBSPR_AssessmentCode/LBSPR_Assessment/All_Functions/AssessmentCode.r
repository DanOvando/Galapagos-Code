

AssessFunction <- function(LengthData, CurrentDir, SpFile, SpNum, LikePlot) {

##############################
# Read in Assumed Parameters #
##############################

SpeciesName <- as.character(SpFile[1, SpNum +1])
assumedLinf	<- as.numeric(SpFile[2, SpNum +1])	
assumedMK	<- as.numeric(SpFile[3, SpNum +1])
genM		<- as.numeric(SpFile[4, SpNum +1])
genLinf		<- as.numeric(SpFile[5, SpNum +1])
genLinfCV	<- as.numeric(SpFile[6, SpNum +1])
gent0		<- as.numeric(SpFile[7, SpNum +1])
MatType		<- as.character(SpFile[8, SpNum +1])
MatL50		<- as.numeric(SpFile[9, SpNum +1])
MatL95		<- as.numeric(SpFile[10, SpNum +1])
Wbeta		<- as.numeric(SpFile[11, SpNum +1])
genK		<- genM/assumedMK

######################
# Set up Length Data #
######################
gennumClass <- nrow(LengthData)+1
minLen		<- min(LengthData[,1])
maxLen		<- max(LengthData[,1])
genminLen 	<- minLen/assumedLinf
genmaxLen 	<- maxLen/assumedLinf
SampObservedFreqVector <- LengthData[,2]
SampObservedPropVector <- LengthData[,2]/sum(LengthData[,2])

##########################
# Grid Search Parameters #			# These parameters can be used to set bounds around the estimated parameters.  Selectivity terms are relative, so 50% Selectivity/Linf etc
##########################
LowerSelL50 <- 0.1 		# Starting minimum for Sel L50
UpperSelL50 <- 0.8		# Starting maximum for Sel L50
LowerSelL95 <- 0.1		# Starting minimum for Sel L95
UpperSelL95 <- 0.8		# Starting maximum for Sel L95
InitialSelPrec <- 0.2	# Starting increment for Selectivity vectors
LowerF_M <- 0.5			# Starting minimum for F/M
UpperF_M <- 3	 		# Starting maximum for F/M
InitialF_MPrec <- 0.5	# Starting increment for F/M vector
FinalSelPrec <- 0.1		# Final increment for Selectivity vectors
FinalF_MPrec <- 0.2		# Final increment for F/M vector

################################
# Set up generic age structure #
################################
# Generic vectors
genMaxAge  <- Max.age.Fun(M=genM, DefineMaxAge=0.1) # Define maximum age - functions below
genAges    <- seq(0, genMaxAge, 1)
genLengths <- Length.at.Age.Fun(Linf=genLinf, K=genK, t0=gent0, Age=genAges) # von Bert length
genLengthSD <- CalcLengthStDev(Linf=1, genLinfCV,genK, genAges) 
# Grid Search parameters - determined by looking at catch histogram	
GridSearchResultPlot <- FALSE # Plot results

RunEstimation <- RunGridSearchFunction (LowerSelL50, UpperSelL50, LowerSelL95, UpperSelL95, InitialSelPrec, FinalSelPrec, LowerF_M, UpperF_M, InitialF_MPrec, FinalF_MPrec,
				SampObservedFreqVector, SampObservedPropVector, genM, genLinf, genLinfCV, genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, GridSearchResultPlot, LikePlot)

BestEstimates <- RunEstimation$Stage3$BestEstimates
if (BestEstimates[3] == 0.1 | BestEstimates[3] == 40) BestEstimates <- RunEstimation$Stage2$BestEstimates
EstSelL50 <- BestEstimates[1]
EstSel95  <- BestEstimates[2]
EstFM	  <- BestEstimates[3] 

EstSPR   <- EstimateSPR(estMK=assumedMK, estFM=EstFM, genM, gent0, genSelL50=EstSelL50, genSelL95=EstSel95, genLinf, 
					genLinfCV, assMatType=MatType, genMatL50=MatL50/assumedLinf, genMatL95=MatL95/assumedLinf, assFecType=1,Wbeta)

Output <- NULL
Output$EstSPR <- EstSPR
Output$BestEstimates <- BestEstimates					
return(Output) }	




				