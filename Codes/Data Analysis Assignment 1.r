library(dplyr)
library(geosphere)

initial_data <- read.csv('https://github.com/DivyaSharma0795/IDS702_Data_Analysis_Assignment_1/raw/main/Resources/listings.csv')

glimpse(initial_data)

airbnb <- initial_data


# Adding Distance to Downtown as a variable from lat long data
airbnb$dist_to_dt <- apply(airbnb[,c("longitude","latitude")],1,function(x) distm(
  c(-82.55481168521978,35.59701329976918),
  x,fun=distHaversine))*0.00062137

library(tidyverse)
# Cleaning the Price variable - removing $ signs and commas
airbnb$cleaned_price <- as.numeric(gsub(',','',str_sub(airbnb$price,2)))

# Cleaning the bathrooms column
library(readr)
airbnb$bathrooms_text <- tolower(airbnb$bathrooms_text)
airbnb$bathrooms_text <- ifelse(grepl("half", airbnb$bathrooms_text, fixed = TRUE), "0.5",airbnb$bathrooms_text)
airbnb$bathrooms_numeric <- parse_number(airbnb$bathrooms_text)
airbnb$host_verification_email <- ifelse(grepl("email", airbnb$host_verifications, fixed = TRUE), 1, 0)
airbnb$host_verification_phone <- ifelse(grepl("phone", airbnb$host_verifications, fixed = TRUE), 1, 0)
unique(airbnb$host_verification_email)
unique(airbnb$host_verification_phone)
airbnb$has_availability <- ifelse(grepl("t", airbnb$has_availability, fixed = TRUE), 1, 0)
unique(airbnb$has_availability)
airbnb$host_has_profile_pic <- ifelse(grepl("t", airbnb$host_has_profile_pic, fixed = TRUE), 1, 0)
unique(airbnb$host_has_profile_pic)
airbnb$host_identity_verified <- ifelse(grepl("t", airbnb$host_identity_verified, fixed = TRUE), 1, 0)
unique(airbnb$host_identity_verified)
airbnb$host_is_superhost <- ifelse(grepl("t", airbnb$host_is_superhost, fixed = TRUE), 1, 0)
unique(airbnb$host_is_superhost)

# Building the first model with all variables
model_v1 <- lm(cleaned_price ~ 
                 room_type + 
                 #bedrooms + 
                 dist_to_dt + 
                 bathrooms_numeric + 
                 accommodates + 
                 #beds + 
                 minimum_nights + 
                 maximum_nights + 
                 has_availability + 
                 number_of_reviews + 
                 review_scores_rating + 
                 reviews_per_month + 
                 review_scores_location + 
                 review_scores_value + 
                 review_scores_communication + 
                 review_scores_checkin + 
                 review_scores_cleanliness + 
                 review_scores_accuracy + 
                 host_has_profile_pic +
                 host_identity_verified + 
                 host_verification_email +
                 host_verification_phone +
                 host_has_profile_pic + 
                 host_identity_verified +
                 host_is_superhost
               , data = airbnb)
summary(model_v1)

glimpse(airbnb)

# Removing blanks from host_is_superhost
library(tidyr)
airbnb$host_is_superhost <- replace(airbnb$host_is_superhost,airbnb$host_is_superhost == "",'f')

# Multicollinearity
library(corrplot)
corrplot(cor(airbnb[,c(
  "dist_to_dt"
  ,"bedrooms"
  ,"bathrooms_numeric"
  ,"accommodates"
  ,"beds"
  ,"minimum_nights"
  ,"review_scores_rating"
  ,"reviews_per_month"
  ,"review_scores_location"
  ,"review_scores_value"
  ,"review_scores_cleanliness"
  )
],use="pairwise.complete.obs"))


library(car)
vif(model_v1)


#Adding features from Amenities
airbnb$amenities <- tolower(airbnb$amenities)
airbnb$has_wifi <- ifelse(grepl("*wifi*", airbnb$amenities), 1, 0)
unique(airbnb$has_wifi)
airbnb$has_parking <- ifelse(grepl("*parking*", airbnb$amenities), 1, 0)
unique(airbnb$has_parking)
airbnb$has_ac <- ifelse(grepl("*air conditioning*", airbnb$amenities), 1, 0)
unique(airbnb$has_ac)
airbnb$has_kitchen <- ifelse(grepl("*kitchen*", airbnb$amenities), 1, 0)
unique(airbnb$has_kitchen)
airbnb$has_pets <- ifelse(grepl("*pet*", airbnb$amenities), 1, 0)
unique(airbnb$has_pets)
airbnb$has_microwave <- ifelse(grepl("*microwave*", airbnb$amenities), 1, 0)
unique(airbnb$has_microwave)
airbnb$has_tv <- ifelse(grepl("*tv*", airbnb$amenities), 1, 0)
unique(airbnb$has_tv)
airbnb$has_refrigerator <- ifelse(grepl("*refrigerator*", airbnb$amenities), 1, 0)
unique(airbnb$has_refrigerator)
airbnb$has_heating <- ifelse(grepl("*heating*", airbnb$amenities), 1, 0)
unique(airbnb$has_heating)


model_v2 <- lm(cleaned_price ~ 
                 room_type + 
                 bedrooms + 
                 dist_to_dt + 
                 bathrooms_numeric + 
                 accommodates + 
                 beds + 
                 minimum_nights + 
                 maximum_nights + 
                 has_availability + 
                 number_of_reviews + 
                 review_scores_rating + 
                 reviews_per_month + 
                 review_scores_location + 
                 review_scores_value + 
                 review_scores_communication + 
                 review_scores_checkin + 
                 review_scores_cleanliness + 
                 review_scores_accuracy + 
                 host_has_profile_pic +
                 host_identity_verified + 
                 host_verification_email +
                 host_verification_phone +
                 host_has_profile_pic + 
                 host_identity_verified +
                 host_is_superhost +
                 has_ac +
                 has_parking +
                 has_wifi +
                 has_kitchen +
                 has_pets +
                 has_microwave +
                 has_refrigerator +
                 has_heating
               , data = airbnb)
summary(model_v2)



vif(model_v2)



plot(model_v2, which = 1, main = "Diagnostic Plot 1: Residuals vs. Fitted Values")
plot(model_v2, which = 2, main = "Diagnostic Plot 2: Normal Q-Q Plot")
plot(model_v2, which = 3, main = "Diagnostic Plot 3: Scale-Location Plot")
plot(model_v2, which = 4, main = "Diagnostic Plot 4: Residuals vs. Leverage")



library(car)
vif_plot <- plot(vif(model_v2))
vif_plot

# Generate diagnostic plots
plot_list <- plot(model_v2)
plot_list



library(ggplot2)
ggplot(airbnb, aes(x=cleaned_price)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept=mean(cleaned_price, na.rm=T)), color="red", linetype="dashed", size=1) +
  ggtitle("Distribution of Price in USD") +
  xlab("Price (USD)") +
  ylab("# of Listings")
