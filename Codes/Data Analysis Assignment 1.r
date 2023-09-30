library(dplyr)
library(geosphere)

initial_data <- read.csv('https://github.com/DivyaSharma0795/IDS702_Data_Analysis_Assignment_1/raw/main/Resources/listings.csv')
airbnb <- initial_data

# Adding Distance to Downtown as a variable from lat long data
airbnb$dist_to_dt <- apply(airbnb[,c("longitude","latitude")],1,function(x) distm(
  c(-82.55481168521978,35.59701329976918),
  x,fun=distHaversine))*0.00062137

