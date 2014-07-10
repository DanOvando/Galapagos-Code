#### ControlFile ###

AvailableData<- c("LengthData")

 Assessments<- c('LBSPR')

# Assessments<- c('CatchCurve')

### Life History ###

Fish<- NULL


Fish$SciName<- 'P.Gracilatis'
Fish$CommName<- 'Spiny Lobster'


DefaultSD<- 0.05 #if I'm reading the lower comment right?

Fish$LHITol<- 0.1 # The average % deviation from LHI allowed

Fish$MvK<-1.6

Fish$vbk<- 0.21625                        ##according to Informe final   0.21625    error 0.004M and 0.06H #male, female,

Fish$LengthError<- 0.05

Fish$Linf<- 40.1                 ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)

Fish$t0<- 0

Fish$WeightA <- 0.0340/1000 #From D. Viana's summary of lit, "Lobster Life History..."

Fish$WeightB<- 3 #From D. Viana's summary of lit, "Lobster Life History..."

# Fish$M<- 0.378  

Fish$M<- 0.166 

Fish$MaxAge<- log(.01)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed

Fish$MortalityError<- 0.1                  ## test error, can change to accomodate more error

Fish$Mat50<- 24.05                        ## 24.05 according to Toral and Hearn (tail only = 13.96)

Fish$Mat95<- 26                               ## 26 according to Toral and Hearn (tail only = 15.24)

Fish$PLD<- 31

Fish$VBSD<- 0.05

Fish$VBErrorSlope<- 0.1

Fish$res<- 'High'

### Fleet Parameters ###

Fleet<- NULL

Fleet$MinSizeCaught<- 26                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 

Fleet$MaxSizeCaught<- Fish$Linf-0.5