#################################################################
# Function to convert the catch at age to catch at size distribution
# Mar 28, 2013 -- Updated ADMB file to internalize entire LBSPR fun
#################################################################

CatchatSize.Fun <- function(CatchatAge,minLen,maxLen,LengthBins=1,sel50,sel95,MSL,maxAge,LengthAtAge,LengthSDVec){
  
  #Calculate the probability of falling into 1mm length bins
  LengthVec <- seq(minLen, maxLen, by= LengthBins)   #changed to make 1cm length bins
  LenMids <- seq(LengthVec[1] +((LengthVec[2]-LengthVec[1])/2), by=(LengthVec[2]-LengthVec[1]), length=length(LengthVec))
  AgeLenProbMat <- matrix(0, nrow=length(LengthVec), ncol=maxAge) 
  for (i in seq_along(1:maxAge)) 
  {
    for (lengthclass in seq_along(LengthVec)[-1])
    {  
      length_cat <- lengthclass-1
      if(length_cat==1) AgeLenProbMat[length_cat,i] <- pnorm(LengthVec[lengthclass], LengthAtAge[i], LengthSDVec[i]) 
      if(length_cat>1) AgeLenProbMat[length_cat,i] <- pnorm(LengthVec[lengthclass], LengthAtAge[i], LengthSDVec[i]) - pnorm(LengthVec[lengthclass-1], LengthAtAge[i], LengthSDVec[i])     	
    }
    AgeLenProbMat[,i] <- AgeLenProbMat[,i]/sum(AgeLenProbMat[,i])
  }
  
  ##Calculate the selectivity at length for each length bin
  if (is.na(MSL) & is.na(sel50)){
    SelLengthVec <- rep(1,length(LenMids))
  }else{
    if (!is.na(MSL)){
      sel50 <- MSL
      sel95 <- MSL+0.01
    } 
    SelLengthVec <- plogis(LenMids, location=sel50, scale=(sel95-sel50)/log(19))
  }
  SelLengthMat <- matrix(SelLengthVec,nrow=length(LenMids), ncol=maxAge, byrow=FALSE)
  
  ###Create matrix of normalized probabilities of being above the size and selected.
  Prob2Mat <- AgeLenProbMat* SelLengthMat
  NormProbMat <- Prob2Mat
  ColSums <- apply(Prob2Mat, 2, sum)
  for (i in 1:fishPop$fish$maxAge) {
    NormProbMat[,i] <- Prob2Mat[,i]/ColSums[i] # Normalise probabilities
  }
  
  CatchatLengthMat <-  CatchatAge * t(NormProbMat)
  CatchatLength <- apply(CatchatLengthMat, 2, sum)
  
  return(CatchatLength)
  
}