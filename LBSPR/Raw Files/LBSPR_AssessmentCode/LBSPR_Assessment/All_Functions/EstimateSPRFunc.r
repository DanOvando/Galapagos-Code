#***************************************************#	
# Generic Function to Estimate SPR from M/K, F/M, Selectivity, and Maturity
EstimateSPR <- function (estMK, estFM, genM, gent0, genSelL50, genSelL95, genLinf, genLinfCV, assMatType, genMatL50=NULL, genMatL95=NULL, assFecType=1, assW.beta=3 ) 
{
	Shape = 19
	genMaxAge <- Max.age.Fun(M=genM, DefineMaxAge=0.1) # 
	genAges   <- seq(0, genMaxAge, 1)
	genK <- genM/estMK
	genLengths <- Length.at.Age.Fun(Linf=genLinf, K=genK, t0=gent0, Age=genAges) # von Bert length
	genF <- genM * estFM
	# Selectivity at Length
	SelectatSize <- SelLengthFun(Shape=19, genSelL50, genSelL95, Lengths=genLengths)
	genLinf.Var <- (genLinf * genLinfCV) ^2
	genLength.StDev  <- sqrt(genLinf.Var*(1-exp(-genK*(genAges))^2))
	# Convert to Sel at Age
	SelectAtAge <- ConvertSelLtoSelAge (genLengths, genLength.StDev, genLinf, Shape, genSelL50, genSelL95)	
	
	# Maturity at mean Length - assumed to be the same as Maturity at Age
	# if (assMatType == "Logistic") {
	LenMaturity <- ConvertSelLtoSelAge (genLengths, genLength.StDev, genLinf, Shape, genMatL50, genMatL95)	
		#LenMaturity <- Mat.Logistic.L95.Fun	(Shape=19, genMatL50, genMatL95, RelLengths=genLengths)
	# } else
	# if (assMatType == "KnifeEdge") {
		# LenMaturity <- Mat.Length.KnifeEdge(genMatL50, Lengths=genLengths)
	# }
	
	# Fecundity at length - assumed to be the same as Fecundity at Age
	if (assFecType==1) {
		genFecundity <- (genLengths ^ assW.beta) * LenMaturity
	} else
	if (assFecType==2) {
		genFecundity <- rep(1, length(genLengths)) * LenMaturity
	}
	estSPR <- SPR.F.Fun(Mpar=genM, Fpar=genF, Ages=genAges, Fec.A=genFecundity, Sel.A=SelectAtAge) 

return(estSPR)
}	

