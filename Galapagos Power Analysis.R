rm(list = ls())
###### Data Poor Stock Assessment (DPSA)Module ######

### Setup Working Environment ###
library(lattice)
source("AssessmentModules.R") #Pull in assessment modules
source("SubFunctions.R") #Pull in helper functions for assessment modules

### Pull in Assessment Data ###

  Country<- 'Ecuador'

  Site<- 'Galapagos_Islands'

  Species<- 'Spiny_Lobster_MaleRealExtrapTotal'

Fishery <- paste(Country,Site,Species,sep='-')

Directory<- paste(Country, "/", Site,'/',Species,'/',sep='')

source(paste(Country, "/", Site,'/',Species,'/',Fishery,"_ControlFile.R", sep = ""))

dir.create(paste(Directory,'Results',sep=''))

dir.create(paste(Directory,'Figures',sep=''))

FigureFolder<- paste(Directory,'Figures/',sep='')

ResultFolder<- paste(Directory,'Results/',sep='')

for (d in 1:length(AvailableData)) #Read in available data
{
	eval(parse(text=paste(AvailableData[d],'<- read.csv(',"'",Directory,Fishery,'_',AvailableData[d],'.csv',"'",')',sep='')))
	
	eval(parse(text=paste('Plot',AvailableData[d],'(',AvailableData[d],')',sep='')))
	
}

### Run Assessments ###

AssessmentResults<- as.data.frame(matrix(NA,nrow=length(Assessments)*10,ncol=9))

colnames(AssessmentResults)<- c('Year','Method','SampleSize','Value','LowerCI','UpperCI','SD','Metric','Flag')

AssessmentResults$Year<- as.numeric(AssessmentResults$Year)

Count<-0
Fish$LHITol<- 0.6

# LengthData<- LengthData[LengthData$Year>2006,]

source('Bootstrap_CatchCurve.R')

SampleSize<- seq(from=.01,to=1,length.out=20)

LengthData<- LengthData[LengthData$Year==2000 |LengthData$Year==2012,]

for (s in 1:length(SampleSize)) #Loop over possible assessments, store in Assessment results. Many assessments have more detailed outputs than can also be accessed 
{
	

		Temp<- BootStrap_CatchCurve(LengthData,'AgeBased',1,2007,NA,1,100,1,0,1,SampleSize[s])$Output

		DataLength<- dim(Temp)[1]

		AssessmentResults[(Count+1):(Count+DataLength),]<- Temp	
		
		Count<- Count+DataLength	
		
}

Data2000<- AssessmentResults[AssessmentResults$Year==2000,]

Data2012<- AssessmentResults[AssessmentResults$Year==2012,]



AssessmentResults<- AssessmentResults[is.na(AssessmentResults$Year)==F,]
AssessmentResults$Year<- as.numeric(AssessmentResults$Year)
AssessmentResults$Value<- as.numeric(AssessmentResults$Value)
AssessmentResults$LowerCI<- as.numeric(AssessmentResults$LowerCI)
AssessmentResults$UpperCI<- as.numeric(AssessmentResults$UpperCI)
AssessmentResults$SD<- as.numeric(AssessmentResults$SD)

AssessmentResults[,4:7]<- round(AssessmentResults[,4:7],2)

YLimits<- c(0,1.1*max(AssessmentResults$SD))

pdf(file='Galapagos Bootstrap Analysis.pdf')
par(mfrow=c(2,1))
plot(Data2000$SD~Data2000$SampleSize,type='b',xlab='Lobsters Measured',ylab='Standard Deviation',main='2000 Data',ylim=YLimits)
plot(Data2012$SD~Data2012$SampleSize,type='b',xlab='Lobsters Measured',ylab='Standard Deviation',main='2012 Data',ylim=YLimits)
dev.off()

pdf(file='Galapagos Bootstrap Analysis No Limits.pdf')
par(mfrow=c(2,1))
plot(Data2000$SD~Data2000$SampleSize,type='b',xlab='Lobsters Measured',ylab='Standard Deviation',main='2000 Data')
plot(Data2012$SD~Data2012$SampleSize,type='b',xlab='Lobsters Measured',ylab='Standard Deviation',main='2012 Data')
dev.off()


show(AssessmentResults)
save.image(file=paste(ResultFolder,Fishery,'_Settings.RData',sep='')) #Save settings used to produce current results
write.csv(file=paste(ResultFolder,Fishery,'_BootStrapResults.csv',sep=''),AssessmentResults) #Save current results