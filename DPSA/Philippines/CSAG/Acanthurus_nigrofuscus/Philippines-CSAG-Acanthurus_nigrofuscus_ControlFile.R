#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

# Assessments<- c('CatchCurve')

 Assessments<- c('LBAR','CatchCurve','DensityRatio','LBSPR')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.2

DefaultSD<- 0.05

Fish$Linf<- 16 #ish, 120mm SL

Fish$LengthError<- DefaultSD

Fish$t0<- -2.26

Fish$WeightA <- 2.38e-2

Fish$WeightB<- 2.97

Fish$M<- .1

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 8

Fish$Mat95<- 10

Fish$PLD<- 31


Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1



### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(1.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf