#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

Assessments<- c('DensityRatio')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.32

DefaultSD<- 0.05

Fish$Linf<- 62.2

Fish$LengthError<- DefaultSD

Fish$t0<- -0.42

Fish$WeightA <- 0.01374

Fish$WeightB<- 3

Fish$M<- 0.89

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 34

Fish$Mat95<- 34.1

Fish$PLD<- 35

Fish$VBSD<- Fish$LengthError

Fish$VBErrorSlope<- 0.1


### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf