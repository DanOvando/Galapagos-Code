# Function to simulate length frequency of catch
# Arguments:	minLen - minimum size for length frequency - default to 0
#				maxLen - maximum length for length frequency - usually 1.5 or more of Linf
#				numClass - number of length classes for length frequency histogram
#				Lengths - vector of mean lengths at age
#				LengthStDev - standard deviation of length at age
#				M - natural mortality
#				Fpar - fishing mortality
#				Ages - vector of ages (0 - max age)
#				SelectatAge - vector of selectivity at age
#				SelL50 - parameter for selectivity at length function
#				SelL95 - parameter for selectivity at length function

SimLengthFreq <- function(minLen=0, maxLen, numClass, Lengths, LengthStDev, M, Fpar, Ages, SelectatAge, SelL50, SelL95) 
{
	# Set up length classes
	LenClasses <- seq(minLen, maxLen, length=numClass)
	LenMids <- seq(LenClasses[1] +((LenClasses[2]-LenClasses[1])/2), by=(LenClasses[2]-LenClasses[1]), length=numClass-1)
	# Calculate age structured fished equilibrium
	Fished.Equlib <- Fished.Equilibrium.Fun(500000, M, Fpar, Sel.A= SelectatAge, Age=Ages) # Changed to 500 0000
	# Catch
	Fished.EqulibCatch <- Fished.Equlib$Catch
	Fished.EqulibCatch[Fished.EqulibCatch<1] <- 0
	# Assign length according to normal probability distribution
	Prob <- matrix(0, nrow=(numClass-1), ncol=length(Ages))
	for (i in seq_along(Ages)) 
		{
		  for (lengthclass in seq_along(LenClasses)[-1])
			{	
			  length_cat <- lengthclass-1
			  if(length_cat==1) Prob[length_cat,i] <- pnorm(LenClasses[lengthclass], Lengths[i], LengthStDev[i]) 
			  if(length_cat>1) Prob[length_cat,i] <- pnorm(LenClasses[lengthclass], Lengths[i], LengthStDev[i]) - pnorm(LenClasses[lengthclass-1], Lengths[i], LengthStDev[i]) 			
			}
			#Prob[,i] <- Prob[,i]/sum(Prob[,i])
		}
	SelectLengthClasses <- SelLengthFun(Shape=19, SelL50, SelL95, Lengths=LenMids) 
    Prob2 <- Prob * SelectLengthClasses	# adjust probabilty of length at age for selectivity at length
	#image.plot(Ages, 1:(numClass-1), t(Prob), ylab="Length category")
	CatchLengths_mat <- Fished.EqulibCatch*t(Prob2)
	CatchLengths <- apply(CatchLengths_mat, 2, sum)
	Pcatchlength <- apply(CatchLengths_mat, 2, sum)/sum(CatchLengths)
	Output <- list(LengthMids=LenMids, LenClasses = LenClasses, LengthFreq = CatchLengths, LengthProp=Pcatchlength)
return(Output)
}

