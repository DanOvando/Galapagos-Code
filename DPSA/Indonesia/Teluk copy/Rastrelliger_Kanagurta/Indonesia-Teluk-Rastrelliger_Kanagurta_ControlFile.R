#### ControlFile ###

AvailableData<- c("LengthData")

Assessments<- c('LBAR','LBSPR')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.72

DefaultSD<- .01

Fish$Linf<- 34.85

Fish$LengthError<- DefaultSD

Fish$t0<- 0

Fish$WeightA <- .0178

Fish$WeightB<- 2.79675

Fish$MaxAge<- 5.97

Fish$M<- .18

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 1.4

Fish$Mat95<- 1.5

Fish$PLD<- 1.2


Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1



### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(1.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf