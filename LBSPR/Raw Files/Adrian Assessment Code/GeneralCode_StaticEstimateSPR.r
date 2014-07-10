
# Set current directory
# Set CurrentDir to the directory that this file is in 
getwd()
CurrentDir <- getwd()
# OR
CurrentDir  <- "C:/Documents and Settings/30462947/My Documents/Dropbox/Work Stuff/Shared MSC SPR Project/Adrian Assessment Code"

SpNum  		<- 1  # Leave at 1.  Only Rockfish parameters in the AssumedParameters.csv file
SpFile 		<- read.csv(paste(CurrentDir, "/AssumedParameters.csv", sep=""), header=F, as.is=TRUE) 

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

#######################################################################################################
# Required Data: 																					  #
#				Assumed values of Linf, M/k, etc - currently read in from SpFile, for species # SpNum #
#				Length Data - either simulated or actual length frequency data (females)			  #
#######################################################################################################

##########################################################################
# Require Length Data in the form of a 2 column matrix.					 #
# Column 1 is Mid points of Length Bins									 #
# Largest Length Bin must be larger than Linf - even if it has zero data #
# Column 2 is corresponding Length Frequencies							 #
# More Length Bins - longer model takes to run							 #
##########################################################################

##############################################
# The model should run with the example data #
##############################################
## Example Test Data:
# LengthFrequencies <- c(0,0,1,2,2,5,25,80,75,150,180,225,200,195,170,150,100,50,5,0)
# LengthMids <- seq(from=5, to=55, length=length(LengthFrequencies))
# LengthData <- cbind(LengthMids, LengthFrequencies)

#############################################################################################
# Or:																					    #
# 	here we simulate length data based on biological parameters given in BiologicalPars.csv #
#	at the moment, assumed values are equal to true values that we are simulating data with #
#############################################################################################

##################################################
# Read in Species Biology and Fishing Parameters #
##################################################
# SpNum = Species Number - currently only 1 in csv file.  But more could be added.  
#		Assumed parameters in each column would have to match up with Biological parameters in corrosponding column
BioData <- read.csv(paste(CurrentDir, "/BiologicalPars.csv", sep=""), header=TRUE, stringsAsFactors=FALSE, as.is=TRUE) 
Linf 	<- as.numeric(BioData[1, SpNum + 1])
LinfCV	<- as.numeric(BioData[2, SpNum + 1])
Mpar	<- as.numeric(BioData[3, SpNum + 1])
Kpar	<- as.numeric(BioData[4, SpNum + 1])
t0		<- as.numeric(BioData[5, SpNum + 1])
MatType <- BioData[6, SpNum + 1]
MatL50  <- as.numeric(BioData[7, SpNum + 1])
MatL95	<- as.numeric(BioData[8, SpNum + 1])
SelL50  <- as.numeric(BioData[9, SpNum + 1])
SelL95	<- as.numeric(BioData[10, SpNum + 1])
Walpha	<- as.numeric(BioData[11, SpNum + 1])
Wbeta	<- as.numeric(BioData[12, SpNum + 1])

Fpar	<- as.numeric(BioData[13, SpNum +1])

######################## 
# Simulate "Real Data" #
########################
MaxAge 	<- Max.age.Fun(M=Mpar, DefineMaxAge=0.1) # Define maximum age 
Ages   	<- seq(0, MaxAge, 1) # "True" ages
Lengths <- Length.at.Age.Fun(Linf=Linf, K=Kpar, t0=t0, Age=Ages) # von Bert length
LengthSD <- CalcLengthStDev(Linf, LinfCV, Kpar, Ages) # standard deviation of length at each age
# Selectivity
SelatLength <- SelLengthFun(Shape=19, SelL50, SelL95 , Lengths) # selectivty at length
# Calculate selectivity at age, given selectivity at length and distribution of length at age
SelatAge 	<- ConvertSelLtoSelAge(MeanLatAge=Lengths, LatAgeStDev=LengthSD, Linf, Shape=19, relSelL50=SelL50/Linf, relSelL95=SelL95/Linf) 

########################
# Calculate "True" SPR #
########################
MatatLength <- Mat.Logistic.L95.Fun(Shape=19, MatL50, MatL95, Lengths)
# Calculate Maturity at age, given Maturity at length and distribution of length at age. Uses same function as Selectivity
MatatAge 	<- ConvertSelLtoSelAge(MeanLatAge=Lengths, LatAgeStDev=LengthSD, Linf, Shape=19, relSelL50=MatL50/Linf, relSelL95=MatL95/Linf) 

# Calc fecundity vector - I will be adding different fecundity functions later - currently assumed proportional to Weight
Fecundity 	<- MatatAge * Walpha * Lengths^Wbeta
ActualSPR 	<- SPR.F.Fun(Mpar=Mpar, Fpar, Ages, Fec.A=Fecundity, Sel.A=SelatAge)

# Calculate SPR versus F/M
SPRatFs <- NULL
FM_Range <- seq(0, 5, 0.1)
F_Range <- FM_Range * Mpar
SPRatFs <- sapply(seq_along(FM_Range), function (X) SPR.F.Fun(Mpar, Fpar=F_Range[X], Ages, Fecundity,  Sel.A=SelatAge))

# Plot SPR versus F/M given Selectivity 
plot(FM_Range, SPRatFs, type="l", xlab="F/M", ylab="SPR", bty="l", xaxs="i", yaxs="i", ylim=c(0,1))

###############################################################
# Simulate Length Composition of Catch - assuming all females #
###############################################################
R0 						<- 1e+06 # 
# Calculate Equilibrium Age Structure of Catch, given Selectivtity and Fishing Mortality
CatchatAge 				<- Fished.Equilibrium.Fun(R0, Mpar, Fpar, Sel.A=SelatAge, minAge=0, maxAge=NULL, Age.Vec=Ages)$Catch

# Parameters for length compostion
minLen 		<- 0 			# start length comp from 0
maxLen 		<- 1.5*Linf 	# allow for fish up to 1.5 times Linf
numClass 	<- 30			# number of length classes - relates to size of length bin
SampPerc    <- 1			# Proportion of catch that is sampled - no observation error

# Construct length compostion given Age Structure of Catch	
Sampled_LengthFreq  	<- SimLength_Sample(CatchAge=CatchatAge, minLen, maxLen, numClass, Lengths, LengthSD, Linf, M, Fpar, Ages, K, t0, LinfCV, SelatAge, SampPerc, SelL50, SelL95)
SampObservedFreqVector  <- Sampled_LengthFreq$LengthFreq
SampObservedPropVector  <- Sampled_LengthFreq$LengthProp
SampLengthMids			<- Sampled_LengthFreq$LengthMids
SampLengthClasses		<- Sampled_LengthFreq$LenClasses
SampStLengthMids 		<- SampLengthMids/Linf

# Length Data
LengthData <- cbind(LengthBins=SampLengthMids, LengthFrequency=SampObservedFreqVector)
View(LengthData) 

##################################################
# Run Assessment Model - returns estimate of SPR #
##################################################
LikePlot <- FALSE # Set to TRUE to visualise fitting routine

# The grid search parameters in the AssessFunction code can be changed if run the estimation faster.			
RunAssess <- AssessFunction(LengthData, CurrentDir, SpFile, SpNum, LikePlot)  # Run Assessment 
BestEstimates <- RunAssess$BestEstimates
EstSPR <- RunAssess$EstSPR # Estimated SPR

BestEstimates[1] * Linf	# Estimated SelL50 - assuming assumedLinf is true
BestEstimates[2] * Linf # Estimated SelL95 - assuming assumedLinf is true
BestEstimates[3] 		# Estimated F/M
Fpar/Mpar				# Actual F/M
EstSPR 					# Estimated SPR
ActualSPR				# Calculated SPR

################
# Control Rule #
################

# Set Target SPR
targSPR <- 0.45 # Target SPR 

pastRBC <- 4000 # pastRBC <- Catch[Year-1] This Needs to be the Catch from Last Year - Year that is being assessed.  
				# Control Rule will return next years recommended Catch
				
# RBC - Recommend Biological Catch
# NewRBC = quota for next year
# SPRestVec - this is a vector of estimated SPR from the last X years.  I have used 5 usually.
# In first few years I have used the estimates we have. e.g Year=2 SPRestVec <- rep(SPRestYr1, 5), Year=3 SPRestVec <- c(rep(SPRestYr1, 4), SPRestYr2), etc
SPRestVec 	<- c(0.2, 0.35, 0.13, 0.2, EstSPR) # Test values - set around EstSPR - change if Fpar is changed

# Make Plots
plot(1:5, SPRestVec, bty="l", ylim=c(0,1), ylab="SPR", xlab="Years")
Yrs <- 1:length(SPRestVec)
lines(c(1,5), c(targSPR, targSPR), lty=3)
Lm <- lm(SPRestVec~Yrs)
Coeff <- coef(Lm)
lines(Yrs, Coeff[2]*Yrs+Coeff[1], lty=2)

NewRBC 	<- RBCbasedControlRule(pastRBC, targSPR, SPRestVec, k1=1, k2=0.7, MinchangePerc=0.025, MaxchangePercUp=0.3, MaxchangePercDown=0.5, option=1)$NewRBC
pastRBC
NewRBC
