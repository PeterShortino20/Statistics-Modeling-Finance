rm(list =ls())

library(readr)
library(Hmisc)
hood <- read.csv("BigDataSheet.csv", header = TRUE, na.strings = -1)
parks <- read.csv("Austin Parks.csv", header = TRUE)
rec <- read.csv("Recreation_Centers.csv", header = TRUE)
income <- read.csv("Income Table.csv", header = TRUE)

#this checks if the a recreation center near the neighborgood
#we are using 3 mile radius; if theres a 
#68.7 miles = 1 degree of latitude; degrees of change ~0.0145
# multipled by 2 so its approximately 2 miles of length North, West, South, East
degreechange <- 0.0145 * 2

#compares if recreation center is within the longitude range of the neighborhood area
UpperLongtitudeRec <- as.data.frame(outer(hood$Longtitude + degreechange, rec$Longtitude, ">"))
LowerLongtitudeRec <- as.data.frame(outer(hood$Longtitude - degreechange, rec$Longtitude, "<"))
LongtitudeRec <- UpperLongtitudeRec * LowerLongtitudeRec

#compares if recreation center is within the latitude range of the Neighborhood area
UpperLatitudeRec <- as.data.frame(outer(hood$Latitude + degreechange, rec$Latitude, ">"))
LowerLatitudeRec <- as.data.frame(outer(hood$Latitude - degreechange, rec$Latitude, "<"))
LatitudeRec <- UpperLatitudeRec * LowerLatitudeRec

# matrices is multiplied to see if the recreation is considered to be true with in the values of the neighborhood latitude and longitude area
RecInNeighorhood <- LongtitudeRec * LatitudeRec
Neighborhoods <- as.data.frame(hood$Neighborhood.Reporting.Area)
#adds total number of recreation centers in a neighborhood
TotalRecNeighborhoods <- as.data.frame(rowSums(RecInNeighorhood))
NumRecNeighborhood <- cbind.data.frame(Neighborhoods, TotalRecNeighborhoods)


#do the same with Parks as with Rec
#compares if park is within the latitude range of the neighborhood area
UpperLongtitudePark <- as.data.frame(outer(hood$Longtitude + degreechange, parks$Longtitude, ">"))
LowerLongtitudePark <- as.data.frame(outer(hood$Longtitude - degreechange, parks$Longtitude, "<"))
LongtitudePark <- UpperLongtitudePark * LowerLongtitudePark

#compares if park is within the latitude range of the neighborhood area
UpperLatitudePark <- as.data.frame(outer(hood$Latitude + degreechange, parks$Latitude, ">"))
LowerLatitudePark <- as.data.frame(outer(hood$Latitude - degreechange, parks$Latitude, "<"))
LatitudePark <- UpperLatitudePark * LowerLatitudePark

# matrix is multiplied to see if the park is considered to be true with in the values of the neighborhood latitude and longitude area
ParksInNeighorhood <- LongtitudePark * LatitudePark

#adds total number of parks in a neighborhood
TotalParksNeighborhoods <- as.data.frame(rowSums(ParksInNeighorhood))
NumParkNeighborhood <- cbind.data.frame(Neighborhoods, TotalParksNeighborhoods)
                                        


