#### ControlFile ###

AvailableData<- c("LengthData")

 Assessments<- c('LBAR','CatchCurve','LBSPR')

# Assessments<- c('CatchCurve')

### Life History ###

Fish<- NULL

DefaultSD<- 0.05

Fish$vbk<- 0.194

Fish$LengthError<- DefaultSD

Fish$Linf<- 46 #ish, 120mm SL

Fish$t0<- 0

Fish$WeightA <- 9.37e-6*1000

Fish$WeightB<- 3.172

# Fish$MaxAge<- 25

Fish$M<- 0.17

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- 0.05

Fish$Mat50<- 11

Fish$Mat95<- 12

Fish$PLD<- 31

Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1


### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- 20

Fleet$MaxSizeCaught<- Fish$Linf-0.5