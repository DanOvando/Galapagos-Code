#### ControlFile ###

AvailableData<- c("LengthData",'DensityData')

Assessments<- c('LBAR','DensityRatio','LBSPR','CatchCurve')

### Life History ###

Fish<- NULL

Fish$vbk<- 0.32

DefaultSD<- .05

Fish$Linf<- 62.2



Fish$LengthError<- DefaultSD

Fish$t0<- -.42

Fish$WeightA <- .01374

Fish$WeightB<- 3

Fish$MaxAge<- 9

Fish$M<- .59

Fish$MortalityError<- DefaultSD

Fish$Mat50<- 2

Fish$Mat95<- 2.06

Fish$PLD<- 35


Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1



### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- LengthAtAge(.1,Fish,0)

Fleet$MaxSizeCaught<- .95*Fish$Linf