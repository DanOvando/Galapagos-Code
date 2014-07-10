# 
# SPR.F.Fun 	<- function(Mpar, Fpar, Ages,Fec.A, Sel.A) { 
# # Calculate SPR of population as per equation from Walters and Martell 
# 
# SurvUnF 	<- rep(0,length(Ages))
# SurvUnF[1]	<- 1
# SurvF 		<- rep(0,length(Ages))
# SurvF		<- 1
# 
# SurvUnF <- 1*exp(-Mpar*Ages)
# SurvF	<- 1*exp(-(Mpar +(Fpar*Sel.A))*Ages)
# 
# for (x in 2:length(Ages)) {
#   SurvF[x]  <- SurvF[x-1] * exp(-(Mpar+(Fpar*Sel.A[x-1])))
# }
# 
# SPR	<- sum(Fec.A*SurvF)/(sum(Fec.A*SurvUnF))
# 
# return(SPR)
# }


SPR.F.Fun   <- function(Mpar, Fpar, Ages,Fec.A, Sel.A) { 
  # Calculate SPR of population as per equation from Walters and Martell 
  
  SurvUnF <- 1*exp(-Mpar*Ages)
  SurvF	<- rep(0, length(SurvUnF))
  SurvF[1]<- 1*exp(-Mpar*Ages[1])
  
  for (x in 2:length(Ages)) {
    SurvF[x]  <- SurvF[x-1] * exp(-(Mpar+(Fpar*Sel.A[x-1])))
  }
  
  SPR	<- sum(Fec.A*SurvF)/(sum(Fec.A*SurvUnF))
  
  return(SPR)
}