# 01_Exercise.R
#
# Health industry expenditure requirements of all regions
# Calculate health balances

rm(list=ls())
setwd("/Users/renato/Documents/02Study/01_RM-RUG/Thesis/Data/wiodmatrices")

library(reshape)

# Since we want to do this calculation for all years of the WIOD
# We create a variable with the years and just iterate over its
# elements performing the same calculations.

years <- as.character(c(1995:2009))

# And this is the "big" for loop

for (i in 1:length(years)) {

	# Obtain the original data from csv files created in Excel.
	
	Z <- as.matrix(read.table(paste(getwd(),"/", years[i],"/Za_", years[i],".csv", sep=""), header = TRUE, dec = ".", sep=",", comment.char="\"", row.names = 1))
	# Disagregated final demand per region.
	FD <- as.matrix(read.table(paste(getwd(),"/", years[i],"/aggFDa_", years[i], ".csv", sep=""), header= TRUE, dec=".", sep=",", row.names=1))
	
	# We calculate the output vector "x" from the data
	x <- as.matrix(rowSums(Z)+rowSums(FD))
	colnames(x) <- "x"
	
	# To avoid division by zero, we create an "x" vectors with 1s instead of 0s.
	x_nozeros <- as.matrix(x)
	x_nozeros[x_nozeros==0] <- 1
	
	# Normally, to follow matrix notation, we would
	# create a diagonal matrix out of x and then post-multiply
	# Z by its inverse in order to get the A matrix. However, in tests
	# and program help this has resulted in inaccuracies, due
	# to the fact that the program first conducts the solving of the
	# inverse and then multiplies the resulting rounded values with Z.
	# An example of the inaccuracy is if we have a 12 in the Z matrix
	# and a 3 in the output matrix. We would like to multiply 12 by 1/3
	# which is the same as (12*1)/3 which equals the integer 12/3 = 4.
	# The computer however, first solves 1/3 = 0.333... and then multiplies
	# that by 12 resulting in 12 * 0.3333 = 3.9996. We can easily see how
	# an error like that is prone to propagate in the closed system of the
	# input-output model.
	# The seemingly erroneous way would be done in the following manner:
	#
	# xv <- as.vector(x_nozeros)
	# xhat <- diag(xv)
	# A <- as.matrix(Z %*% solve(xhat))
	#
	# Luckily, R has something called the recycling rule.
	# If we divide a matrix by a vector, it will do so 
	# element by element, recycling the vector as many
	# times necessary, column by column. Which is just what we need.
	# We simply divide the transpose of Z by x and then transpose again.
	
	A <- as.matrix(t(t(Z) / as.vector(x_nozeros)))
	colnames(A) <- colnames(Z)
	
	# We create an identity matrix of adequate proportions
	I <- as.matrix(diag(length(as.vector(x))))
	
	# And we calculate our Leontief matrix.
	L <- solve(I-A) 
	
	# If needed, you can check the model calibration
	# by calculating the difference between the calculated output
	# and the original output, feeding the original final demand to
	# the model:
	#
	# f <- as.matrix(rowSums(FD))
	# colnames(f) <- "f"
	# x1 <- L %*% f
	# xdiff <- as.matrix(x1-x)
	# xcomp <- cbind(x,x1,xdiff)
	# colnames(xcomp) <- c("x", "x1", "x1 - x")
	# write.csv(xcomp, file = "xa_comp.csv", fileEncoding = "macroman")
	
	# =============================================================
	# Exercise 1. Purchases to the health sector
	# =============================================================
	
	# To obtain H
	
	# We extract the necessary rows by making a list of them.
	# A sequence that starts in 33 and ends in 1433 with interv-
	# als of 35.
	clist <- c(seq(33, 1433, 35))
	
	# ...and create a matrix of them by pulling them out of Z
	hexp <- as.matrix(Z[c(clist), ])
	
	# ...we adjust sectors for which we believe the value of interest
	# is less than than reported to correct for elements of the health
	# sector that are not related to human health. See 3.2.2 of the
	# paper. The code creates a list of the index numbers for the columns
	# that we need to adjust with a sequence and then applies a 
	# scaling coefficient (number between 0 and 1) to those columns.
	
	# Agriculture... Industry (1)
	dlist <- c(seq( 1, 1401, 35))
	hexp[,c(dlist)] <- hexp[,c(dlist)] * 0.15
	
	# Public administration. Industry (31)
	elist <- c(seq(31, 1431, 35))
	hexp[,c(elist)] <- hexp[,c(elist)] * 0.75
	
	# Health and Social Work. Industry (33)
	flist <- c(seq(33, 1433, 35))
	hexp[,c(flist)] <- hexp[,c(flist)] * 0.75
	
	# We add all the countries into one line and divide that line by the output
	# element-wise (we use the output that has no zeros)
	c <- as.matrix(as.matrix(colSums(hexp)) / as.matrix(x_nozeros))
	
	# On with the calculations, we multiply a diagonal vector of it by L
	
	H <- as.matrix(diag(as.vector(c))%*%L)
	
	# This is a matrix of the producer responsibilities.
	PR <- as.matrix(H %*% diag(rowSums(FD)))
	
	# And let's calculate the actual values
	
	prodres <- matrix(as.matrix(rowSums(PR)), dimnames= dimnames(x))
	
	# We aggregate by country to calculate responsibilities
	# (see reshape.R).
	
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
	"/prodresa-",years[i],".csv", sep=""))
	
	# And this is a matrix of the final demand responsibilities.
	FDR <- as.matrix(H %*% FD)
	
	library(reshape)
	fdresdb <- melt(FDR, id="1")
	
	# And its column sum is the actual region responsibility
	fdres <- as.matrix(colSums(FDR))
	
	# SLIGHT ADJUSTMENT
	# Erik recommended "weighing" the average multipliers, using
	# final demand shares as weights. Now, whereas there are 41
	# final demands, meaning 41 vectors of weights and 1435 in-
	# dustries, each vector has to be used 35 times. This is my
	# non-elegant implementation of that idea.
	
	# First we create a matrix of weights per country.
	
	w <- t(FD)/colSums(FD)
	
	# And just for control I think it's better if the weights are
	# by the whole Final Demand of all countries
	
	ds <- as.matrix(rowSums(FD))
	w3 <- ds/colSums(ds)
	w3 <- as.vector(w3)
	
	# And here's the tricky long part. We apply each of those to
	# only the rows of the multipliers table that correspond to
	# the same country. I'm sure there's a more elegant way of
	# doing it, but this will have to suffice in view of the time
	# we have. So,
	
	# duplicate H
	# transpose it so element-wise multiplication works okay, and
	
	H2 <- t(H)
	
	# proceed with indexed multiplications
	
	H2[,c(1:35)] 		<- H2[,c(1:35)]		*w[1,]
	
	# And then just transpose back (remember to export to csv)
	
	H2 <- t(H2)
	
	# Now for the other case
	
	H3 <- t(H)
	H3 <- H3 * w3
	H3 <- t(H3)


	# ==================================================
		
	write.csv(FDR, file = paste(getwd(),"/", years[i],
	"/FDRa-",years[i],".csv", sep=""))
	write.csv(fdres, file = paste(getwd(),"/", years[i],
	"/fdresa-",years[i],".csv", sep=""))
#	write.csv(H, file = paste(getwd(),"/", years[i],
#	"/Ha-",years[i],".csv", sep=""))
	write.csv(H2, file = paste(getwd(),"/", years[i],
	"/H2a-",years[i],".csv", sep=""))
	write.csv(H3, file = paste(getwd(),"/", years[i],
	"/H3a-",years[i],".csv", sep=""))
#	write.csv(fdresdb, file = paste(getwd(),"/", years[i],"/fdresadb-",years[i],".csv", sep=""))
}

for (i in 1:length(years)) {
	assign(paste("fdres",years[i], sep=""), as.matrix(read.table(paste(getwd(),"/", years[i],"/fdresa-", years[i], ".csv", sep=""), header = TRUE, dec = ".", sep=",", row.names = 1 )))
	assign(paste("prodres",years[i], sep=""),
	as.matrix(read.table(paste(getwd(),"/",years[i],"/prodresa-",years[i],".csv", sep=""), header=TRUE, dec=".", sep=",", row.names = 1)))
}

fdresAY <- cbind(fdres1995, fdres1996, fdres1997, fdres1998, fdres1999, fdres2000, fdres2001, fdres2002, fdres2003, fdres2004, fdres2005, fdres2006, fdres2007, fdres2008, fdres2009)
colnames(fdresAY) <- as.character(c(1995:2009))
write.csv(fdresAY, file = paste(getwd(),"/results/EX01_findemAllYrs.csv", sep=""))

prodresAY <- cbind(prodres1995, prodres1996, prodres1997, prodres1998, prodres1999, prodres2000, prodres2001, prodres2002, prodres2003, prodres2004, prodres2005, prodres2006, prodres2007, prodres2008, prodres2009)
colnames(prodresAY) <- as.character(c(1995:2009))
write.csv(prodresAY, file = paste(getwd(),"/results/EX01_prodresAllYrs.csv", sep=""))


rm(list=ls())