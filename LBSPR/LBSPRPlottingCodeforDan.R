### Code to read report file -- 

ObjVal <- scan(paste(ADMBDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), skip=2, nlines=1, quiet=TRUE)
estSPR <- scan(paste(ADMBDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), skip=3, nlines=1, quiet=TRUE)
ModelFit <- scan(paste(ADMBDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), nlines=1, quiet=TRUE) 

#AssessResults formatted as:
Output$est <- c(estSel50, estSel95, estFM, estSPR, ObjVal) 
Output$fit <- ModelFit


# Do Plot function
DoSinglePlot <- function(AssessResults, CountDat, LenMids, SaveFile=TRUE, FileName="ModelFit.png", ModFail) {
  
  par(mfrow=c(1,1))
  if (SaveFile) png(FileName)
  Max <- max(CountDat/sum(CountDat))
  xx <- barplot(CountDat/sum(CountDat), names.arg=round(LenMids,0), ylim=c(0, Max+Max*0.1))
  if (ModFail == FALSE) lines(xx, AssessResults$ModelFit, lwd=4)
  if (ModFail) text(xx[5], Max*0.8, "MODEL FAILED TO CONVERGE", cex=1.5, pos=4)
  mtext(side=1, "Length Classes", line=3, cex=1.5)
  mtext(side=2, "Proportion", line=2.5, cex=1.5)
  if (SaveFile) dev.off()
}

# Code to loop through multipe plots with feedback.
if (CheckFits) {
  DoSinglePlot(RunAssess, CurYrDat, LenMids, SaveFile=FALSE, FileName="", ModFail) 
  temp <- ""
  while (temp == "") {
    cat("\n")
    cat("\n")
    cat("Year:", YearVec[Yr], "\n")
    cat("Model Fit has been Plotted \n")
    Estimates <- AssessResults[Yr,2:6]
    print(as.data.frame(Estimates))
    cat("ACCEPT THIS ESTIMATE? y or n \n") 
    temp <- readline()
    if (temp == "y" | temp=="Y") {
      cat("Optional comment (press ENTER to continue) \n") 
      AssessResults[Yr, 8] <- readline()
    }  
    if (any(temp == "y" | temp=="Y" | temp=="n" | temp=="N") == FALSE) temp <- ""
  }
  if (temp =="n" | temp =="N") {
    AssessResults[Yr, 7] <- "NO"
    cat("Enter Comment why estimates were rejected, and press ENTER \n") 
    AssessResults[Yr, 8] <- readline()
  }
}