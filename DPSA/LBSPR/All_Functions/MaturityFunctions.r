
Mat.Logistic.L95.Fun <- function(Shape=19,MatL50, MatL95, RelLengths) {
Maturity		<- 1-(1/(1+exp(log(Shape)*((RelLengths-MatL50)/(MatL95-MatL50)))))
return(Maturity)
}	

Mat.Length.KnifeEdge <- function(MatL50, Lengths) {				
Mat.Length		<- rep(0, length(Lengths))
L50Length 		<- min(which(Lengths >= MatL50))
Mat.Length[L50Length:length(Lengths)] <- 1	
return (Mat.Length)
}