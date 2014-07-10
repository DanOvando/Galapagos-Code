
# Function to estimate SD of length at age - calculated from Sainsbury 1980
CalcLengthStDev <- function (Linf, LinfCV, K, Ages) 
{
  LinfVar <- (Linf * LinfCV) ^2
  LengthStDev  <- sqrt(LinfVar*(1-exp(-K*Ages))^2)
  if (Ages[1] == 0) {
    LengthStDev[1] <- LengthStDev[2] * 0.5 # Fix for Age = 0
  }	
return(LengthStDev)
}


