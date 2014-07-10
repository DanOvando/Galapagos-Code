# Simulate population at fished equilibrium
# This version NO plus group - Age.Vec goes to maximum age

Fished.Equilibrium.Fun 	<- function(R0, Mpar, Fpar, Sel.A, minAge=0, maxAge=NULL, Age.Vec=NULL)	
{ 
	Z.Vector <- Mpar + (Sel.A * Fpar)
	F.Vector <- Fpar * Sel.A
	Catch    <- NULL
	Pop	   <- NULL
	Output   <- NULL
	for (Age in seq_along(Age.Vec)) 
		{
	      if (Age == 1) {
		   Pop[Age] <- R0
		  } else
		  if (Age > 1 ) {
			Pop[Age] <- Pop[Age-1]*exp(-Z.Vector[Age-1])
		  } 
		  Catch[Age] <- (F.Vector[Age]/Z.Vector[Age]) * Pop[Age]*(1-exp(-Z.Vector[Age]))				
		}	
	Output <- list(Pop=Pop, Catch=Catch)
return(Output)
}
	
	
# # This version includes Plus group.  Not used.

# Fished.Equilibrium.Fun 	<- function(R0, Mpar, Fpar, Sel.A, minAge=0, maxAge=NULL,  Age.Vec=NULL)	
	# {
		# Mpar <- Mpar 
		# Fpar <- Fpar 
		# Z.Vector <- Mpar + (Sel.A * Fpar)
		# F.Vector <- Fpar * Sel.A
		# Catch    <- NULL
		# Pop		 <- NULL
		# Output	 <- NULL
			# for (Age in seq_along(Age.Vec)) 
			# {
				# if (Age == 1) {
					# Pop[Age] <- R0
				# } else
				# if (Age > 1 & Age < (length(Age.Vec))) {
					# Pop[Age] <- Pop[Age-1]*exp(-Z.Vector[Age-1])
				# } 
				# if (Age == (length(Age.Vec))) {
					# Pop[Age] <- Pop[Age-1]*exp(-Z.Vector[Age-1])/(1-exp(-Z.Vector[Age-1]))					
				# }
				# Catch[Age] <- (F.Vector[Age]/Z.Vector[Age]) * Pop[Age]*(1-exp(-Z.Vector[Age]))				
			# }	
			
		# Output <- list(Pop=Pop, Catch=Catch)
		# return(Output)
	# }	