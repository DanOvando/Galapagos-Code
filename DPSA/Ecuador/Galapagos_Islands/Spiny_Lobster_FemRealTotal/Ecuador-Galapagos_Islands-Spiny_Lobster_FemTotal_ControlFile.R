#### ControlFile ###

AvailableData<- c("LengthData")

 Assessments<- c('LBSPR','LBAR','CatchCurve')

# Assessments<- c('CatchCurve')

### Life History ###

Fish<- NULL

DefaultSD<- 0.05

Fish$vbk<- 0.21625                        ##according to Informe final   0.21625    error 0.004M and 0.06H
# Fish$vbk<- .05

Fish$LengthError<- 0.06

Fish$Linf<- 40.14                        ##41.9M and 40.14H averaged from Norahs values

# Fish$Linf<- 32                        ##41.9M and 40.14H averaged from Norahs values


Fish$t0<- 0

Fish$WeightA <- 9.37e-6*1000 

Fish$WeightB<- 3.172

# Fish$MaxAge<- 25

Fish$M<- 0.342                   ## average mortality according to Matt Kay and literature review, Hearn 2008 use 0.348M and 0.378H

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- 0.05                  ## test error, can change to accomodate more error

Fish$Mat50<- 24.05                        ## according to Toral and Hearn 

Fish$Mat95<- 26                              ## according to Toral and Hearn

Fish$PLD<- 31

Fish$VBSD<- 0.03

Fish$VBErrorSlope<- 0.1


### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- 26                        ##minimum landing size for Galapagos      (was 20 before?)

Fleet$MaxSizeCaught<- Fish$Linf-0.5