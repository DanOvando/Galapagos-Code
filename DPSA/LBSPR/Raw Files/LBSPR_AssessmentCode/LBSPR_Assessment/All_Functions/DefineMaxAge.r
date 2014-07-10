# Function that determines max age as age when cohort declines to DefineMaxAge% of initial size
# Returns age class when number is equal or slightly greater than DefineMaxAge% of
# initial cohort size
# Two arguements - M - which is coefficient of instantaneous natural mortality
#				 - DefineMaxAge - percentage that cohort declines to from original abundance
# Author: 	Adrian Hordyk
# Date:		July 2011

Max.age.Fun	<- function(M, DefineMaxAge=0.1) 
	{  
		Tt		<- c(0:500)
		N0		<- 100
		N		<- N0*exp(-(M*Tt))
		MaxAge	<- max((N >= DefineMaxAge)*Tt)
		# MaxAge <- -log(DefineMaxAge/N0)/M
		return(MaxAge)
	}
	
