# 00_reshapeFDa.R
# Aggregate WIOD Final demands in one for every region for all WIOD years.

# Renato Vargas
# University of Groningen

# Preamble:
rm(list=ls())
setwd("/Users/renato/Documents/02Study/01_RM-RUG/Thesis/Data/wiodmatrices")

years <- as.character(c(1995:2009))

for (i in 1:length(years)) {

	FD <- as.matrix(read.table(paste(getwd(),"/", years[i],"/FDa_", years[i], ".csv", sep=""), header = TRUE, dec = ".", sep=",", comment.char="\"", row.names = 1))
	
	# Then we have to create a flat file for manipulation
	# from the WIOD matrix. The process is called "melt" the 
	# data, and hence the command...
	
	library(reshape)
	mFD <- melt(FD, id="1")
	
	
	mFD$X1 <- as.character(mFD$X1)
	mFD$X2 <- as.character(mFD$X2)
	
	sellRegion <- mFD$X1
	purchasing <- strsplit((mFD$X2), "\\_")
	
	purchRegion <- sapply(purchasing, "[", 1)
	purchSector <-sapply(purchasing, "[", 2)
	
	mFD <- cbind(sellRegion, purchRegion, purchSector, mFD)
	
	# We get rid of the variables from the "melt" command.
	myvars <-names(mFD) %in% c("X1", "X2")
	mFD <- mFD[!myvars]
	
	aggmFD <- mFD
	
	# We order by selling region and then by sector
	# aggmZ <- aggmZ[order(sellRegion,),]
	
	# We then concatenate two columns of strings:
	aggmFD$newSell <- aggmFD$sellRegion
	aggmFD$newPurch <- aggmFD$purchRegion
	# ...drop what's not needed and rearrange the data.frame
	aggmFD <- aggmFD[c("newSell", "newPurch", "value")]
	
	# Finally, we cast the new matrix and save it to csv for viewing in Excel.
	# Since now not every element of newSell or newPurch uniquely identifies
	# the data (and hence the need to aggregate), we have to add the aggregating
	# function "fun.aggregate=sum". It could also be means or any other.
	# We also make the resulting data frame into a matrix.
	
	FD <- as.matrix(cast(aggmFD, newSell~newPurch, sum), row.names="1")
	write.csv(FD, file = paste(getwd(),"/", years[i],"/aggFDa_", years[i], ".csv", sep=""), fileEncoding = "macroman")
	
	rm(aggmFD, FD, i, mFD, myvars, purchasing, purchRegion, purchSector, sellRegion)

}

rm(list=ls())
