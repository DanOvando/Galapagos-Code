# Function to calculate logistic selectivity at length
# Arguments: 	Shape - must remain 19
#				SelL50 - length at 50% selected
#				SelL95 - length at 95% selected
#				Lengths - vector of lengths

SelLengthFun <- function(Shape=19,SelL50, SelL95, Lengths) 
{
  Shape=19
  SelLength	<- plogis(Lengths, location=SelL50, scale=(SelL95-SelL50)/log(Shape))
  return(SelLength)
}	

