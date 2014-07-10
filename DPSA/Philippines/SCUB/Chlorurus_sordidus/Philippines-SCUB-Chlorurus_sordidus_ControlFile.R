#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

Assessments<- c('LBAR','DensityRatio','LBSPR','CatchCurve')

### Life History ###

Fish<- NULL

Fish$vbk<- 1.125

DefaultSD<- .05

Fish$Linf<- 17.541 #ish, 120mm SL

Fish$LengthError<- 0.41

Fish$t0<- 0

Fish$WeightA <- 0.0182

Fish$WeightB<- 3.15

Fish$M<- .35

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 14

Fish$Mat95<- 14.2962413

Fish$PLD<- 30

Fish$VBSD<- Fish$LengthError

Fish$VBErrorSlope<- 0.1


### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(0.01,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf