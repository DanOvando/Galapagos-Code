######################################################################################
# Example: Assessing Surf Perch Size Structures      
# May 21, 2013
# Working with Kathryn Crane's preliminary data and parameter estimates to illustrate 
# Length-Based SPR Assessments
#######################################################################################
#rm(list=ls())
#set working directory
CurrentDir <- '/Users/danovando/Desktop/Bren/SFG Work/DPSA/LBSPR/Raw Files/LBSPR_AssessmentCode'  ## Set this to location of folder on your machine
setwd(CurrentDir)


###################################
# Source LBSPR function #    
#################################### 
AssessDir <- paste(CurrentDir,"LBSPR_Assessment",sep="/")
setwd(AssessDir)
source("LBSPRAssessmentFun_May21.R")
###################################
# Source all other functions needed#    
#################################### 

Call.functions.Fun    <- function(dropboxDir) {
  cd <- dropboxDir
  File.Path	<- paste(cd,"All_Functions", sep="/")
  for (nm in list.files(File.Path)) {
    source(file.path(File.Path, nm))
  }
}
Call.functions.Fun(AssessDir)	

###################################
# load R2ADMB and compile tpl file #    
###################################
#install.packages("R2admb")  #You will need to install the R2admb package the first time, and make sure 
#the path is set so that R and admb can communicate.
library("R2admb")
compile_admb("LBSPR_AssessFun",verbose=FALSE)
setwd(CurrentDir)  #changes directory back to main folder.

##Set Length bins
LengthBins <-10   #1cm

#Read in the Size data and Species parameter file
LengthDat <- read.csv("ExampleData.csv")
SpFile <- read.csv("AssumedParameters_Example.csv",header=FALSE)

## Convert lenth data into appropriate format-- vector of raw size data
CatchatLength <- LengthDat$Redtail.F


# Call the Assessment Function
Estimates <- LBSPR_SingleSpeciesAssessmentfun(CatchatLength,AssessDir, CurrentDir,SpFile)

# Sel50, Sel95, F/M, and SPR stored in Estimates.

