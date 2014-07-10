# Control Rule

RBCbasedControlRule <- function(pastRBC, targSPR, SPRestVec, k1, k2, MinchangePerc=0.025, MaxchangePercUp=0.3, MaxchangePercDown=0.5, option=1) {
  
  # Past SPR Trajectory
  Yrs 	<- 1:length(SPRestVec)
  LReg 	<- lm(SPRestVec~ Yrs)
  Coeff <- coef(LReg)
  Mod 	<- Coeff[2]*Yrs+Coeff[1]
  A		<- Mod[max(Yrs)] - Mod[min(Yrs)] # Difference between current year estimate and five years previous 
  if (option == 1) {
	B <- Mod[max(Yrs)] - targSPR # Difference between Current Estimate (from Linear Model) and Targ SPR
  }else
  if (option == 2) {
    B <- SPRestVec[length(SPRestVec)] - targSPR # Difference between Current Estimate (from Actual Estimate) and Targ SPR
  }	
  
  # Control Rule
  V 			<- A*k1 + B*k2   
  suggestChange <- pastRBC * (1+V)
  
  # Conditions 
  if (abs(suggestChange/pastRBC -1) <= MinchangePerc) {
    NewRBC <- pastRBC
  }else
  if (suggestChange/pastRBC -1 >=  MaxchangePercUp) { 
    NewRBC <- pastRBC + pastRBC * MaxchangePercUp
  }else
  if (-(suggestChange/pastRBC -1) >=  MaxchangePercDown) {
    NewRBC <- pastRBC + pastRBC * (-(MaxchangePercDown ))
  } else NewRBC <- suggestChange
 

Output <- NULL
Output$NewRBC <- NewRBC
Output$Mod  <- Mod
Output$V	<- V

 
return(Output)
}
#############
# Test Code #
#############

# setwd("E:/Dropbox/Work Stuff/PhD SPR Project/MSC Project/September 2012 meeting/Presentations/")

# pastRBC <- 5000
# targSPR <- 0.45
# SPRestVec <- c(0.55, 0.52, 0.68, 0.59, 0.7)
# k1 <- 0.8
# k2 <- 0.7
# MinchangePerc=0.025
# MaxchangePercUp=0.3
# MaxchangePercDown=0.5
# option=1

  # Yrs 	<- 1:length(SPRestVec)
  # LReg 	<- lm(SPRestVec~ Yrs)
  # Coeff <- coef(LReg)
  # Mod 	<- Coeff[2]*Yrs+Coeff[1]
  # A		<- Mod[max(Yrs)] - Mod[min(Yrs)] # Difference between current year estimate and five years previous 

# RBCbasedControlRule(pastRBC, targSPR, SPRestVec, k1, k2, MinchangePerc=0.025, MaxchangePercUp=0.3, MaxchangePercDown=0.5, option=1)

# jpeg("ControlRule.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# dev.off()

# jpeg("ControlRule1_5.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# lines(Yrs, Mod, lwd=2)
# dev.off()

# jpeg("ControlRule1.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# lines(c(-1,10), c(targSPR, targSPR), lty=3, lwd=2)
# lines(Yrs, Mod, lwd=2)
# dev.off()

# jpeg("ControlRule2.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# lines(c(-1,10), c(targSPR, targSPR), lty=3, lwd=2)
# lines(Yrs, Mod, lwd=2)
# arrows(5, targSPR, 5, Mod[5], code=3)
# lines(c(1,5), c(Mod[5], Mod[5]), lty=3)
# arrows(1, Mod[1], 1, Mod[5], code=3)
# text(1.3, Mod[1] + (Mod[5]-Mod[1])/2, "A", cex=1.5)
# text(4.7, targSPR + (Mod[5]-targSPR)/2, "B", cex=1.5)
# dev.off()

# jpeg("ControlRule3.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# lines(c(-1,10), c(targSPR, targSPR), lty=3, lwd=2)
# lines(Yrs, Mod, lwd=2)
# arrows(5, targSPR, 5, Mod[5], code=3)
# lines(c(1,5), c(Mod[5], Mod[5]), lty=3)
# arrows(1, Mod[1], 1, Mod[5], code=3)
# text(1.4, Mod[1] + (Mod[5]-Mod[1])/2, Mod[5]-Mod[1],cex=1.5)
# text(4.7, targSPR + (Mod[5]-targSPR)/2, Mod[5] - targSPR,cex=1.5)
# dev.off()





# pastRBC <- 5000
# targSPR <- 0.45
# SPRestVec <- c(0.42, 0.35, 0.42, 0.27, 0.24)
# k1 <- 0.8
# k2 <- 0.7
# MinchangePerc=0.025
# MaxchangePercUp=0.3
# MaxchangePercDown=0.5
# option=1

  # Yrs 	<- 1:length(SPRestVec)
  # LReg 	<- lm(SPRestVec~ Yrs)
  # Coeff <- coef(LReg)
  # Mod 	<- Coeff[2]*Yrs+Coeff[1]
  # A		<- Mod[max(Yrs)] - Mod[min(Yrs)] # Difference between current year estimate and five years previous 

# RBCbasedControlRule(pastRBC, targSPR, SPRestVec, k1, k2, MinchangePerc=0.025, MaxchangePercUp=0.3, MaxchangePercDown=0.5, option=1)


# jpeg("ControlRule4.jpeg", quality=100)
# par(oma=c(2,2,0,0))
# plot (Yrs, SPRestVec, ylim=c(0,1), xlab="", ylab="", bty="l",  yaxs="i", cex.axis=1.5)
# mtext(side=1, "Years", cex=2, line=3)
# mtext(side=2, "SPR", cex=2, line=3)
# lines(c(-1,10), c(targSPR, targSPR), lty=3, lwd=2)
# lines(Yrs, Mod, lwd=2)
# arrows(5, targSPR, 5, Mod[5], code=3)
# lines(c(1,5), c(Mod[5], Mod[5]), lty=3)
# arrows(1, Mod[1], 1, Mod[5], code=3)
# text(1.4, Mod[1] + (Mod[5]-Mod[1])/2, Mod[5]-Mod[1],cex=1.5)
# text(4.7, targSPR + (Mod[5]-targSPR)/2, Mod[5] - targSPR,cex=1.5)
# dev.off()







