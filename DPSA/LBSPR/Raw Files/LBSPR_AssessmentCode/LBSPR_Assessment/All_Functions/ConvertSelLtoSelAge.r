
# I have changed this function a bit 
# it was giving me major problems with upper = 1000
# also found that it sometimes gives problems because SelL50 is greater than SelL95 in the optim search

# 19/06 - changed rel.tol=1.e-10 to rel.tol=1.e-12 - still doesn't fix problem

ConvertSelLtoSelAge <- function(MeanLatAge, LatAgeStDev, Linf, Shape=19, SelL50, SelL95) {

if (SelL50 >= SelL95) {
 SelatAge <- rep(1, length(MeanLatAge))
 print(SelL50)
 print(SelL95)
 print(SelatAge)
return(SelatAge)
}

integrand <- function(L,...)
{
	dnorm(L, mean=MeanLatAge[age], sd=LatAgeStDev[age]) * plogis(L, location=SelL50, scale=(SelL95-SelL50)/log(Shape))
}

SelatAge <- NULL
error <- NULL

res <- try(
for (age in seq_along(MeanLatAge))
{
# print(SelL50)
 # print(SelL95)
 # print(SelatAge)
 # print(MeanLatAge)
 # print(" start ")
SelatAge[age] <- integrate(integrand, lower = 0, upper = 3 * Linf,  rel.tol=1.e-11)$value
# error[age] <- integrate(integrand, lower = 0, upper = 2 * Linf, rel.tol=1.e-13)$abs.error
 # print("end")
}
,silent=TRUE
)
if (class(res) == "try-error") {
  SelatAge <- rep(1, length(MeanLatAge))
  # print(paste("Length vec = ", MeanLatAge, sep=""))
  # print(paste("Linf = ", Linf, sep=""))
  # print(paste("LatAgeStDev = ", LatAgeStDev, sep=""))
  # print(paste("relSelL50 = ", relSelL50, sep=""))
  # print(paste("relSelL95 = ", relSelL95, sep=""))
 } 
 
return(SelatAge)
}
