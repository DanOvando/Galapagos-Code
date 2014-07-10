# Standard 3 parameter Von Bertalanffy growth model 
# Arguments: 	
#			Linf, K, t0 	- VB growth parameters
# 			minAge,maxAge 	- first and maximum age group respectively
#			Age				- Vector of ages - overrides minAge and maxAge.  
# Author: 	Adrian Hordyk
# Date:		July 2011

Length.at.Age.Fun	<- function(Linf, K, t0, minAge=0, maxAge=NULL, Age=NULL) 
	{ 
		if(length(Age)==0){ 
		  Age <- seq(minAge, maxAge, 1)
		}
		Mean.Lth <- Linf *(1-exp(-K*(Age-t0)))
		Mean.Lth[Mean.Lth<0] <- 0
		return (Mean.Lth)
	}
