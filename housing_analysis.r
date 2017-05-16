# Analysis of trends in Boulder CO housing costs by JLD
# 10 April 2017
# Workers of the world, unite!

# Data source:
# https://www.zillow.com/boulder-co/home-values/

# read in housing cost data from zillow
zdata <- read.table("boulder_housing_cost_perSqF.txt", sep='\t', header=T, stringsAsFactors=F)

# replicate Zillow's plot in R
plot(
	zdata$USD_sqF ~ zdata$months_since_Jan2012, 
	type="b", 
	pch=20,
	xlab="Months since Jan 2012", 
	ylab="Price per square foot (USD)", 
	main="Zillow's plot, replicated in R"
)

# Looks just like it does on the web site. So they aren't lying to me :)

# try a linear fit 
linearfit <- lm(zdata$USD_sqF ~ zdata$months_since_Jan2012)
summary.lm(linearfit)
# R2 = 0.9394, not really bad at all

# try an exponential fit
expfit <- lm(log(zdata$USD_sqF) ~ zdata$months_since_Jan2012)
summary.lm(expfit)
# R2 = 0.9596, it's a little better but not by a ton

# make a plot with both fits on it
plot(
	zdata$USD_sqF ~ zdata$months_since_Jan2012, 
	type="b", 
	pch=20,
	xlab="Months since Jan 2012", 
	ylab="Price per square foot (USD)", 
	main="Linear and exponential fits to Zillow's data"
)
abline(linearfit, col="red", lwd=2)
expfit_predictedcost <- exp(predict(expfit,list(zdata$months_since_Jan2012)))
lines(expfit_predictedcost ~ zdata$months_since_Jan2012, col="blue", lwd=2)

# calculate percent increase per year
years <- unique(zdata$year)
yearav <- rep(0, length(years))
for(i in 1:length(years)){
	yearav[i] <- mean(zdata$USD_sqF[zdata$year == years[i]])
}
A <- yearav[2:length(yearav)]
B <- yearav[1:(length(yearav) - 1)]
percentchange <- ((A - B) / B) * 100


# open PDF to write to
pdf("BoulderHousingIncreases.pdf")

# make a cool plot
plot(
	zdata$USD_sqF ~ zdata$months_since_Jan2012, 
	type="b", 
	pch=20,
	xlab="Months since Jan 2012", 
	ylab="Price per square foot (USD)", 
	main="Average percent increase is INCREASING"
)
abline(v=12*(1:5), lty=2, col="gray")
# year labels
text(x= 6 + 12*(0:4), y=410, labels=2012 + 0:4)
# average points
yearmids <- (6 + 12*(0:4))
points(yearav[1:5] ~ yearmids, pch=18, col="forestgreen", type="b", cex=4, lwd=5)
# percent change labels
for(i in 1:4){
	pclabel <- paste("+", round(percentchange[i], 2), "%", sep="")
	text(x=(12 * (i)), y=400, labels=pclabel, col="forestgreen")

}

dev.off()

