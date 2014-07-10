#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

Assessments<- c('LBAR','CatchCurve','DensityRatio','LBSPR')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.31

DefaultSD<- .01

Fish$Linf<- 44.2

Fish$LengthError<- DefaultSD

Fish$t0<- 0

Fish$WeightA <- .021

Fish$WeightB<- 2.996

Fish$MaxAge<- 21

Fish$M<- .56

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 1.2

Fish$Mat95<- 1.236675475

Fish$PLD<- 54


Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1



### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(1.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf