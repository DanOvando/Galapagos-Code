
# This code runs the SPR@Size estimation model for a given set of length frequency data.
# Two length frequency datasets are in this Directory
# User to specify which species to run assessment on.

# All length data is for Females only, and has been kindly provided by Peter Coulson


# Set current directory
# Set CurrentDir to the directory that this file is in 
getwd()
CurrentDir <- getwd()
# OR
CurrentDir  <- "C:/Documents and Settings/30462947/My Documents/Dropbox/Work Stuff/Shared MSC SPR Project/Adrian Assessment Code"

###################################
# Initialise and source Functions #	  
###################################
Call.functions.Fun		<- function(dropboxDir) {

cd <- dropboxDir
File.Path	<- paste(cd,"\\All_Functions", sep="")
for (nm in list.files(File.Path, pattern = "\\.[RrSsQq]$")) {
	source(file.path(File.Path, nm))
}
}
Call.functions.Fun(CurrentDir)	

#############################
# Choose Species 			#	  
# 	Sea Sweep 	- Species 1 #
# 	Whiting 	- Species 2 #
#############################

SpNum  	<- 1 
GetSpecies <- function(SpNum) {
	switch(SpNum,
		"1" = "Sweep",
		"2" = "Whiting")
		}
Species <- GetSpecies(SpNum)

##############################
# Read in Female Length Data #
##############################
if (SpNum == 1) {
  LengthData <- read.csv(paste(CurrentDir, "/SweepFemaleLengths.csv", sep=""), header=F, as.is=TRUE)  
} else
if (SpNum == 2) {
  LengthData <- read.csv(paste(CurrentDir, "/Female_lengths.csv", sep=""), header=T, as.is=TRUE) 
}
LengthData <- as.matrix(LengthData) 

#####################################################################
# Exclude small individuals - not usually found in commerical catch #
# These were only caught due to scientific sampling 				#
#####################################################################

if (SpNum == 1) {
  LengthData <-  LengthData[LengthData>230]
} else
if (SpNum == 2) {
    LengthData <-  LengthData[LengthData>90]
}

#########################################	
# Create Length Frequency from Raw Data #
#########################################

minLength <- min(LengthData)
maxLength <- max(LengthData)

LengthInt <- 20 # Width of length class in mm
LengthBreaks <- seq(0, 1.2 * maxLength, by=LengthInt)
LengthMids <- seq(LengthBreaks[2] - 0.5*LengthInt, by=LengthInt, length=length(LengthBreaks)-1) 
		
LengthComp <- table(cut(LengthData, LengthBreaks))

####################
# Plot Length Data #
####################
barplot(LengthComp, names.arg =LengthMids)

######################
# Get Assumed Values #
######################
GetAssumedVals <- function(SpNum) {
  switch(SpNum,
	"1" = SweepVals(),
	"2" = WhitingVals())
}

SweepVals <- function() {
  assumedLinf 	<- 409.54
  assumedMK 		<- 0.06/0.18
  assumedLinfCV 	<- 0.05
  MatL50 			<- 362.38
  MatL95 			<- 404.7
return(c(assumedLinf, assumedMK, assumedLinfCV, MatL50, MatL95)) 
}

WhitingVals <- function() {
  assumedLinf 		<- 345.97
  assumedMK 		<- 0.88
  assumedLinfCV 	<- 0.05
  MatL50 			<- 210
  MatL95 			<- 300
return(c(assumedLinf, assumedMK, assumedLinfCV, MatL50, MatL95)) 
}

AssumeVals <- GetAssumedVals(SpNum)

##################################################
# Run Assessment Model - returns estimate of SPR #
##################################################

LikePlot <- TRUE # Set to TRUE to visualise fitting routine

assumedLinf		<- AssumeVals[1]
assumedMK		<- AssumeVals[2]
assumedLinfCV	<- AssumeVals[3]
assumedMatL50	<- AssumeVals[4]
assumedMatL95	<- AssumeVals[5]

genM		<- 0.1
genLinf		<- 1
genLinfCV	<- assumedLinfCV
gent0		<- 0
MatType		<- "Logistic"
MatL50		<- assumedMatL50
MatL95		<- assumedMatL95
Wbeta		<- 3
genK		<- genM/assumedMK

##########################
# Grid Search Parameters #	# These parameters can be used to set bounds around the estimated parameters.  
##########################  # Selectivity terms are relative, so 50% Selectivity/Linf etc
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


gennumClass <- nrow(LengthComp)+1
SampObservedFreqVector <- LengthComp
SampObservedPropVector <- LengthComp/sum(LengthComp)

################################
# Set up Generic Age Structure #
################################
genMaxAge  <- Max.age.Fun(M=genM, DefineMaxAge=0.1) # Define maximum age 
genAges    <- seq(0, genMaxAge, 1)
genLengths <- Length.at.Age.Fun(Linf=genLinf, K=genK, t0=gent0, Age=genAges) # von Bert length
genLengthSD <- CalcLengthStDev(Linf=1, genLinfCV,genK, genAges) 

genminLen <- min(LengthMids)/assumedLinf
genmaxLen <- max(LengthMids)/assumedLinf

########################
# Run Estimation Model #
########################
RunEstimation <- RunGridSearchFunction (LowerSelL50, UpperSelL50, LowerSelL95, UpperSelL95, InitialSelPrec, FinalSelPrec, LowerF_M, UpperF_M, InitialF_MPrec, FinalF_MPrec,
				SampObservedFreqVector, SampObservedPropVector, genM, genLinf, genLinfCV, genLengthSD, genK, gent0, genAges, genLengths, genminLen, genmaxLen, gennumClass, GridSearchResultPlot=FALSE, LikePlot)

BestEstimates <- RunEstimation$Stage3$BestEstimates


if (BestEstimates[3] == 0.1 | BestEstimates[3] == 40) {
  BestEstimates <- RunEstimation$Stage2$BestEstimates
  print("optim did not converge properly - defaulting to Best estimates from Stage 2 - Not accurate ")
}

EstSelL50 <- BestEstimates[1]
EstSel95  <- BestEstimates[2]
EstFM	  <- BestEstimates[3] 

EstSPR   <- EstimateSPR(estMK=assumedMK, estFM=EstFM, genM, gent0, genSelL50=EstSelL50, genSelL95=EstSel95, genLinf, 
					genLinfCV, assMatType=MatType, genMatL50=MatL50/assumedLinf, genMatL95=MatL95/assumedLinf, assFecType=1,Wbeta)
print(EstSPR)

BestEstimates[1] * assumedLinf	# Estimated SelL50 - assuming assumedLinf is true
BestEstimates[2] * assumedLinf # Estimated SelL95 - assuming assumedLinf is true
BestEstimates[3] 		# Estimated F/M
EstSPR 					# Estimated SPR



