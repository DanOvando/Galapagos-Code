#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

Assessments<- c('LBAR', 'DensityRatio','CatchCurve','LBSPR')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.1805

DefaultSD<- .01

Fish$Linf<- 72.38

Fish$LengthError<- DefaultSD

Fish$t0<- -1.9685

Fish$WeightA <- .0162

Fish$WeightB<- 3.01

Fish$MaxAge<- 55

Fish$M<- 0.428790936

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 5.4

Fish$Mat95<- 5.425

Fish$PLD<- 31.4


Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1



### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(1.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf