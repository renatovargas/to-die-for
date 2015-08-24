# 02_Exercise.R
#
# Deaths due to pollution in all regions
# Calculate death responsibility balances

# PREAMBLE

rm(list=ls())
setwd("/Users/renato/Documents/02Study/01_RM-RUG/Thesis/Data/wiodmatrices")

# IMPORTANT: CHANGE THESE THREE PARAMETERS========
# AND DON'T FORGET TO CHANGE THE YEARS AT THE END
# exercise number
ex <- as.matrix("02")
# exercise literal
el <- as.matrix("b")
# name of metric
metric <- as.matrix("polludeath")
# ================================================

# We are going to need the reshape package.
library(reshape)

# We load the equivalences tables
# To aggregate regions as global objects
regagg <- read.table(paste("regagg",ex[1],".csv", sep=""), header = TRUE, dec = ".", 
sep=",", stringsAsFactors=FALSE )
# ...and sectors
secagg <- read.table(paste("secagg",ex[1],".csv", sep=""), header = TRUE, dec = ".", 
sep=",", comment.char="", stringsAsFactors=FALSE)

# Years taken into account for this exercise.
years <-as.character(c("2004", "2008"))

# 	=====================================
# 	1. AGGREGATE MATRICES 
#	=====================================

for (i in 1:length(years)) {
	
	# 1.1. Direct requirements Matrix (Z)
	
	Z <- as.matrix(read.table(paste(getwd(),"/", years[i],"/Za_", years[i], 
	".csv", sep=""), header = TRUE, dec = ".", sep=",", row.names = 1))

	mZ <- melt(Z, id="1")
	
	# We need to convert back these two to class "character."
	# Otherwise, we can't split them. I give up, couldn't find an
	# elegant way to do it.

	mZ$X1 <- as.character(mZ$X1)
	mZ$X2 <- as.character(mZ$X2)

	selling <- strsplit((mZ$X1), "\\_")
	purchasing <- strsplit((mZ$X2), "\\_")

	sellRegion <- sapply(selling, "[", 1)
	sellSector <-sapply(selling, "[", 2)
	
	purchRegion <- sapply(purchasing, "[", 1)
	purchSector <-sapply(purchasing, "[", 2)
	
	mZ <- cbind(sellRegion, sellSector, purchRegion, purchSector, mZ)
	
	# We get rid of the variables from the "melt" command.
	myvars <-names(mZ) %in% c("X1", "X2")
	aggmZ <- mZ[!myvars]
	
	# The following code is a little complicated and comes from
	# "http://stackoverflow.com/questions/6543798/r-how-to-do
	# -fastest-replacement-in-r"; the last answer. It essentially
	# matches the old classification with the aggregated classification
	# using a table of equivalences given by the needs of the study
	# and data availability.
	
	aggmZ <- with(c(regagg,secagg,aggmZ), data.frame(sellRegion = 
	rbName[match(sellRegion,raName)], sellSector =
	sbCode[match(sellSector,saCode)], purchRegion = 
	rbName[match(purchRegion, raName)], purchSector=
	sbCode[match(purchSector,saCode)], value))
	
	# We order by selling region and then by sector
	aggmZ <- aggmZ[order(sellRegion,sellSector),]
	
	# We then concatenate two columns of strings:
	aggmZ$newSell <- paste(aggmZ$sellRegion, aggmZ$sellSector, sep="_")
	aggmZ$newPurch <- paste(aggmZ$purchRegion, aggmZ$purchSector, sep="_")
	
	# ...drop what's not needed and rearrange the data.frame
	aggmZ <- aggmZ[c("newSell", "newPurch", "value")]
	
	# Finally, we cast the new matrix and save it to Z to start calculations.

	Z <- as.matrix(cast(aggmZ, newSell~newPurch, sum), row.names="1")

	
	# 1.2. Multicountry final demand matrix (F). (in the same manner)
	
	# We choose FD instead of F in order not to overwrite the abbreviation
	# for "FALSE", which one can do in R.
	FD <- as.matrix(read.table(paste(getwd(),"/", years[i],"/aggFDa_",
	years[i], ".csv", sep=""), header = TRUE, dec = ".", sep=",", row.names = 1))

	# From here on variable names are the same as above to save some
	# time and recycle the code.
	mZ <- melt(FD, id="1")

	mZ$X1 <- as.character(mZ$X1)
	mZ$X2 <- as.character(mZ$X2)
	
	selling <- strsplit((mZ$X1), "\\_")
	purchRegion <- mZ$X2
	
	sellRegion <- sapply(selling, "[", 1)
	sellSector <-sapply(selling, "[", 2)
	
	mZ <- cbind(sellRegion, sellSector, purchRegion, mZ)
	
	myvars <-names(mZ) %in% c("X1", "X2")
	aggmZ <- mZ[!myvars]
    aggPR <- c("cat", "mouse", "dog")
	
	aggmZ <- with(c(regagg,secagg,aggmZ), data.frame(sellRegion = 
	rbName[match(sellRegion,raName)], sellSector = 
	sbCode[match(sellSector,saCode)], purchRegion = 
	rbName[match(purchRegion, raName)], value))
	
	aggmZ <- aggmZ[order(sellRegion,sellSector),]
	
	aggmZ$newSell <- paste(aggmZ$sellRegion, aggmZ$sellSector, sep="_")
	aggmZ <- aggmZ[c("newSell", "purchRegion", "value")]
	
	# And finally the Final Demand just like we need it
	FD <- as.matrix(cast(aggmZ, newSell~purchRegion, sum), row.names="1")
	
	# We get rid of auxiliary variables at this point
	rm(mZ, selling, purchasing, sellRegion, sellSector, purchRegion, 
	purchSector, myvars, aggmZ)

#	=====================================
# 	2. Exercise - Deaths due to pollution
#	=====================================
	
	# Deaths due to pollution.
	d <- as.matrix(read.table(paste(getwd(),"/",years[i],"/", metric, "_", years[i],".csv", sep=""),
	header= TRUE, dec=".", sep=",", row.names=1))
	x <- as.matrix(rowSums(Z) + rowSums(FD))
	colnames(x) <- "x"
	x_nozeros <- as.data.frame(x)
	x_nozeros$x[x_nozeros$x  == 0]     <- 1
	x_nozeros <- as.matrix(x_nozeros)
	
	# I would normaly create x "hat" and postmultiply Z by its inverse to
	# get A, but I realize you lose some accuracy, and so I do the same
	# doing a normal element-wise division, recycling the x vector.
	# xv <- as.vector(x_nozeros)
	# xhat <- diag(xv)
	A <- as.matrix(t(t(Z)/ as.vector(x_nozeros)))
	colnames(A) <- colnames(Z)
	I <- as.matrix(diag(length(as.vector(x))))
	L <- solve(I-A) 
			
	# This is where the exercise is different. Our "c" is not calculated
	# with the same data, but it is given by WHO fatalities.
	
	# Divide deaths by the output element-wise
	c <- as.matrix(as.matrix(d)) / as.matrix(x_nozeros)
	
	# On with the calculations, we multiply a diagonal vector of it by L
	
	H <- as.matrix(diag(as.vector(c))%*%L)
	
	# This is a matrix of the producer responsibilities.
	PR <- as.matrix(H %*% diag(rowSums(FD)))
	
	# And let's calculate the actual values
	
	prodres <- matrix(as.matrix(rowSums(PR)), dimnames= dimnames(x))
	
	# We aggregate by country to calculate responsibilities (with reshape).
		
	mprodres <- melt(prodres, id="1")
	mprodres$X1 <- as.character(mprodres$X1)
	# mprodres$X2 <- as.character(mprodres$X2)
	selling <- strsplit((mprodres$X1), "\\_")
	sellRegion <- sapply(selling, "[", 1)
	sellSector <- sapply(selling, "[", 2)
	mprodres <- cbind(sellRegion, sellSector, mprodres)
	# # Get rid of vars from melt
	myvars <- names(mprodres) %in% c("X1", "X2")
	mprodres <- mprodres[!myvars]
	
	# Unnecessary step, but in a hurry to change all recycled
	# code below, so just changed the name.
	aggPR <- mprodres
	
	# # We order by selling region and then by sector
	aggPR <- aggPR[order(sellRegion, sellSector),]
	
	# and a little sneaky trick to aggregate later
	aggPR$sellRegion1 <- aggPR$sellRegion
	
	# # ...drop what's not needed and rearrange the data.frame
	aggPR <- aggPR[c("sellRegion", "sellRegion1", "value")]
	
	aggPR$sellRegion <- as.character(aggPR$sellRegion)
	aggPR$sellRegion <- as.character(aggPR$sellRegion1)
	
	# # And cast
	aggPR <- as.matrix(cast(aggPR, sellRegion~sellRegion1,sum))
	aggPR <- as.matrix(rowSums(aggPR))
	# write.csv(aggPR, file = "aggPR.csv", fileEncoding = "macroman")
	colnames(aggPR) <- as.character(c(paste(years[i])))
	# ...and rename back
	prodres <- aggPR
	
	###########
	
#	write.csv(PR, file = "PRb.csv", fileEncoding = "macroman")
	write.csv(prodres, file = paste(getwd(),"/", years[i],
	"/prodres",el[1],"-",years[i],".csv", sep=""))
	
	# And this is a matrix of the consumer responsibilities.
	FDR <- as.matrix(H %*% FD)
	
#	fdresdb <- melt(FDR, id="1")
	
	# And its column sum is the actual region responsibility
	fdres <- as.matrix(colSums(FDR))
	
	write.csv(fdres, file = paste(getwd(),"/", years[i],
	"/fdres", el[1],"-",years[i],".csv", sep=""))
	
	write.csv(FDR, file = paste(getwd(),"/", years[i],
	"/FDR", el[1],"-",years[i],".csv", sep=""))
	
	write.csv(H, file = paste(getwd(),"/", years[i],
	"/H", el[1],"-",years[i],".csv", sep=""))
	
}

for (i in 1:length(years)) {
	assign(paste("fdres",years[i], sep=""), as.matrix(read.table(paste(getwd(),"/", years[i],"/fdres",el[1],"-", years[i], ".csv", sep=""), header = TRUE, dec = ".", sep=",", row.names = 1 )))
	assign(paste("prodres",years[i], sep=""),
	as.matrix(read.table(paste(getwd(),"/",years[i],"/prodres", el[1],"-",years[i],".csv", sep=""), header=TRUE, dec=".", sep=",", row.names = 1)))
}

fdresAY <- cbind(fdres2004, fdres2008)
colnames(fdresAY) <- as.character(c("2004", "2008"))
write.csv(fdresAY, file = paste(getwd(),"/results/EX",ex[1],"_findemAllYrs.csv", sep=""))

prodresAY <- cbind(prodres2004, prodres2008)
colnames(prodresAY) <- as.character(c("2004", "2008"))
write.csv(prodresAY, file=paste(getwd(), "/results/EX",ex[1],"_prodresAllYrs.csv", sep=""))

rm(list=ls())

