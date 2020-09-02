library(tidyverse)
library(lubridate)
library(extrafont)
library(scales)
library(ggridges)
library(corrplot)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(forecast)
library(corrr)
library(imputeTS)
library(dplyr)
library(corrplot)
library(GGally)

# Create Variables to import dataset for train,building and weather
train <- read_csv("train.csv")
building <- read_csv("building_metadata.csv")
weather <- read_csv("weather_train.csv")

#Join individual datasets of train,building and weather variables
ashrae <- train %>%
  left_join(building, by = "building_id") %>%
  left_join(weather, by = c("site_id", "timestamp"))
#rm(train, building, weather,ashra,ashrae_sample,ashrae2,ashrae3,combine_train,weather_train)
#rm(building,train,weather)
#rm(ashrae,col_pal,percent_missing)
# The percentage of missing values in the dataset are as follows:
percent_missing<-(colSums(is.na(ashrae))/nrow(ashrae))*100
print(percent_missing)

rm(train,building,weather)

ashrae<-select (ashrae,-c(year_built,floor_count,cloud_coverage))

ashrae %>% 
  ggplot(aes(x=meter_reading)) +
  geom_histogram(alpha = 0.5, fill = "#5EB296", colour = "#4D4D4D") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("METER READING VARIABLE IS HEAVILY SKEWED BY SOME OUTLIERS", subtitle = "We will remove those and visualise") +
  labs(x= "Meter Reading", y= "Count")


outlier <- round(1.5 * IQR(ashrae$meter_reading),0)

ashrae %>% 
  mutate(outlier = ifelse(meter_reading > outlier, "Outlier", "Not Outlier")) %>% 
  ggplot(aes(x=meter_reading)) +
  geom_histogram(alpha = 0.5, fill = "#5EB296", colour = "#4D4D4D") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("METER READING VARIABLE IS HEAVILY SKEWED", subtitle = "Meter Reading still skewed even for non-outliers") +
  labs(x= "Meter Reading", y= "Count") +
  facet_wrap(~ outlier, scales = "free")




ashrae[, 8] <- na_interpolation(ashrae[, 8])
ashrae[, 9] <- na_interpolation(ashrae[, 9])
ashrae[, 10] <- na_interpolation(ashrae[, 10])
ashrae[, 11] <- na_interpolation(ashrae[, 11])
ashrae[, 12] <- na_interpolation(ashrae[, 12])
ashrae[, 13] <- na_interpolation(ashrae[, 13])

percent_missing<-(colSums(is.na(ashrae))/nrow(ashrae))*100
print(percent_missing)

# Rename the values for meter with 0 being the 'Electricity' and 3 being 'Hot Water'
ashrae2 <- ashrae %>%
  mutate(meter = if_else(meter== 0,'Electricity',
                         if_else(meter == 1,'Chilled Water',
                                 if_else(meter== 2,'Steam','Hot Water'))))

head(ashrae2)

# Find Hours,Day of the month,Month and year individually and create columns for them
ashrae3 <- ashrae2 %>%
  mutate(month = month(timestamp),year = year(timestamp),
         day = wday(timestamp))




#Correlation matrix for all columns
ashrae4<-select (ashrae,-c(primary_use,timestamp))


#Correlation matrix between all the columns
res2 <- rcorr(as.matrix(ashrae4))
res2

res2$r
res2$P

#Heat map between all the different columns
#corrplot(res2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Since the p value < 0.05 for a column to be significant we remove wind_direction and precip_depth_1_hr

ashrae <- select(ashrae,-c(wind_direction,precip_depth_1_hr))

#Model building for ASHRAE

### Addressing the Outliers: split_outliers function
#We wrote a function that splits a meter reading x timestamp array into 3 new arrays: lower outliers, non-outliers, and upper outliers. 
#The point of this is so that we can plot the outliers separately from the non-outliers in each building type. Note that this is generalized 
#so that it can be used for any Nx2 array. (We have included a file called split_outliers_how_to_use.R on canvas for further info on how to 
#use this function and a simple example.)




########################################################################################
#####################################################################################
#For a 4 variable correlation rather than 3

split_outliers <- function(input_array){
  # takes input_array and outputs 3 subrrays, one each for lower outliers, upper outliers, and non-outliers
  # the 3 subarrays are ordered by the elemtns in the second column, i.e. by time
  # NOTE: if upper or lower outliers do not exist in the input_array, the output is 0
  
  
  # define function
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  
  
  # create data arrays
  # input_array <- array(data = c(rnorm(500),rnorm(500)),dim = c(500,2))
  input  <- as.double(asplit((input_array),2)[[1]])
  input2 <- asplit(input_array,2)[[2]]
  
  n <- length(input)
  
  # sort data
  sorted_input_array <- input_array[order(input_array[,1]),]
  sorted_input  <- asplit(sorted_input_array,2)[[1]]
  sorted_input2 <- asplit(sorted_input_array,2)[[2]]
  
  
  # find outlier cutoffs
  id1 <- boxplot.stats(input)	
  id2 <- boxplot.stats(input, coef=2)	# Uses a step of 2, instead of the default 1.5
  id1$stats	                            # Display the 5 values (see below)
  id1$stats[1]                         	# The lower adjacent value
  id1$stats[5]                        	# The upper adjacent value
  
  lh <- quantile(input,probs=0.25)	# Lower hinge (first quartile)
  uh <- quantile(input,probs=0.75)	# Upper hinge (third quartile)
  step <- 1.5 * (uh-lh)	              # Define the step as 1.5?IQR
  
  # Outliers are then all values outside the interval lh-step < and lh+step
  # the logical expression: input < lh-step | input > lh+step can be used to select the outliers for further processing.
  
  
  # determine if elements are outliers
  logical_input <- sorted_input < lh-step | sorted_input > lh+step
  #logical_input
  
  
  # find min index for FALSE
  min_index <- min(which(logical_input == FALSE))
  
  
  # find max index of FALSE
  max_index <- max(which(logical_input == FALSE))+1
  
  
  if(min_index != 1 & max_index < n+1){
    # lower and upper outliers exist
    
    # split input
    lower_sorted_input  <- splitAt(sorted_input, c(min_index,max_index))[[1]]
    middle_sorted_input <- splitAt(sorted_input, c(min_index,max_index))[[2]]
    upper_sorted_input  <- splitAt(sorted_input, c(min_index,max_index))[[3]]
    
    # split input2
    lower_sorted_input2  <- splitAt(sorted_input2, c(min_index,max_index))[[1]]
    middle_sorted_input2 <- splitAt(sorted_input2, c(min_index,max_index))[[2]]
    upper_sorted_input2  <- splitAt(sorted_input2, c(min_index,max_index))[[3]]
    
    # join 2 vectors to form array
    lower_array  <- array(data = c(lower_sorted_input,lower_sorted_input2),dim = c(length(lower_sorted_input),4))
    middle_array <- array(data = c(middle_sorted_input,middle_sorted_input2),dim = c(length(middle_sorted_input),4))
    upper_array  <- array(data = c(upper_sorted_input,upper_sorted_input2),dim = c(length(upper_sorted_input),4))
    
    lower_output_array  <- array(data=c(lower_sorted_input,lower_sorted_input2),dim=c(length(lower_sorted_input),4))
    middle_output_array <- array(data=c(middle_sorted_input,middle_sorted_input2),dim=c(length(middle_sorted_input),4))
    upper_output_array  <- array(data=c(upper_sorted_input,upper_sorted_input2),dim=c(length(upper_sorted_input),4))
    
    lower_output_array <- lower_output_array[order(lower_output_array[,4]),]
    middle_output_array <- middle_output_array[order(middle_output_array[,4]),]
    upper_output_array <- upper_output_array[order(upper_output_array[,4]),]
  }
  else if(min_index!=1 & max_index == n+1){
    # lower outliers exist
    
    # split input
    lower_sorted_input  <- splitAt(sorted_input, c(min_index))[[1]]
    middle_sorted_input <- splitAt(sorted_input, c(min_index))[[2]] 
    
    # split input2
    lower_sorted_input2  <- splitAt(sorted_input2, c(min_index))[[1]]
    middle_sorted_input2 <- splitAt(sorted_input2, c(min_index))[[2]]
    
    # join 2 vectors to form array
    lower_array  <- array(data = c(lower_sorted_input,lower_sorted_input2),dim = c(length(lower_sorted_input),4))
    middle_array <- array(data = c(middle_sorted_input,middle_sorted_input2),dim = c(length(middle_sorted_input),4))
    
    lower_output_array  <- array(data=c(lower_sorted_input,lower_sorted_input2),dim=c(length(lower_sorted_input),4))
    middle_output_array <- array(data=c(middle_sorted_input,middle_sorted_input2),dim=c(length(middle_sorted_input),4))
    
    lower_output_array  <- lower_output_array[order(lower_output_array[,4]),]
    middle_output_array <- middle_output_array[order(middle_output_array[,4]),]
    upper_output_array  <- 0
  }
  else if(min_index == 1 & max_index < n+1){
    # upper outliers exist
    
    # split input
    middle_sorted_input <- splitAt(sorted_input, c(max_index))[[1]]
    upper_sorted_input  <- splitAt(sorted_input, c(max_index))[[2]]
    
    # split input2
    middle_sorted_input2 <- splitAt(sorted_input2, c(max_index))[[1]]
    upper_sorted_input2  <- splitAt(sorted_input2, c(max_index))[[2]]
    
    # join 2 vectors to form array
    middle_array <- array(data = c(middle_sorted_input,middle_sorted_input2),dim = c(length(middle_sorted_input),4))
    upper_array  <- array(data = c(upper_sorted_input,upper_sorted_input2),dim = c(length(upper_sorted_input),4))
    
    middle_output_array <- array(data=c(middle_sorted_input,middle_sorted_input2),dim=c(length(middle_sorted_input),4))
    upper_output_array  <- array(data=c(upper_sorted_input,upper_sorted_input2),dim=c(length(upper_sorted_input),4))
    
    lower_output_array  <- 0
    middle_output_array <- middle_output_array[order(middle_output_array[,4]),]
    upper_output_array  <- upper_output_array[order(upper_output_array[,4]),]
  }
  else if(min_index== 1 & max_index > n){
    # no outliers exist
    
    lower_output_array  <- 0
    middle_output_array <- input_array[order(input_array[,2]),]
    upper_output_array  <- 0
  }
  
  output_list <- list(lower_output_array,middle_output_array,upper_output_array)
  
  return(output_list)
}

timebybuilding = split(x = ashrae$timestamp, f = ashrae$primary_use)
names(timebybuilding)

meterbybuilding = split(x = ashrae$meter_reading, f = ashrae$primary_use)
names(meterbybuilding)

sqft = split(x = ashrae$square_feet, f = ashrae$primary_use)
names(sqft)

airtemp = split(x = ashrae$air_temperature, f = ashrae$primary_use)
names(airtemp)

#Arrays for each of the building types: meter reading x timestamp
education_array = array(data = c(meterbybuilding$Education, timebybuilding$Education,sqft$Education,airtemp$Education), dim = c(length(meterbybuilding$Education), 4))

entertainment_array = array(data = c(meterbybuilding$`Entertainment/public assembly`, timebybuilding$`Entertainment/public assembly`,sqft$`Entertainment/public assembly`,airtemp$`Entertainment/public assembly`), 
                            dim = c(length(meterbybuilding$`Entertainment/public assembly`), 4))

food_array = array(data = c(meterbybuilding$`Food sales and service`, timebybuilding$`Food sales and service`,sqft$`Food sales and service`,airtemp$`Food sales and service`), 
                   dim = c(length(meterbybuilding$`Food sales and service`), 4))

healthcare_array = array(data = c(meterbybuilding$Healthcare, timebybuilding$Healthcare,sqft$Healthcare,airtemp$Healthcare), dim = c(length(meterbybuilding$Healthcare), 4))

lodging_array = array(data = c(meterbybuilding$`Lodging/residential`, timebybuilding$`Lodging/residential`,sqft$`Lodging/residential`,airtemp$`Lodging/residential`), 
                      dim = c(length(meterbybuilding$`Lodging/residential`), 4))

manufacturing_array = array(data = c(meterbybuilding$`Manufacturing/industrial`, timebybuilding$`Manufacturing/industrial`,sqft$`Manufacturing/industrial`,airtemp$`Manufacturing/industrial`), 
                            dim = c(length(meterbybuilding$`Manufacturing/industrial`), 4))

office_array = array(data = c(meterbybuilding$Office, timebybuilding$Office,sqft$Office,airtemp$Office), dim = c(length(meterbybuilding$Office), 4))

other_array = array(data = c(meterbybuilding$Other, timebybuilding$Other,sqft$Other,airtemp$Other), dim = c(length(meterbybuilding$Other), 4))

parking_array = array(data = c(meterbybuilding$Parking, timebybuilding$Parking,sqft$Parking,airtemp$Parking), dim = c(length(meterbybuilding$Parking), 4))

public_array = array(data = c(meterbybuilding$`Public services`, timebybuilding$`Public services`,sqft$`Public services`,airtemp$`Public services`), 
                     dim = c(length(meterbybuilding$`Public services`), 4))

religious_array = array(data = c(meterbybuilding$`Religious worship`, timebybuilding$`Religious worship`,sqft$`Religious worship`,airtemp$`Religious worship`), 
                        dim = c(length(meterbybuilding$`Religious worship`), 4))

retail_array = array(data = c(meterbybuilding$Retail, timebybuilding$Retail,sqft$Retail,airtemp$Retail), dim = c(length(meterbybuilding$Retail), 4))

services_array = array(data = c(meterbybuilding$Services, timebybuilding$Services,sqft$Services,airtemp$Services), dim = c(length(meterbybuilding$Services), 4))

technology_array = array(data = c(meterbybuilding$`Technology/science`, timebybuilding$`Technology/science`,sqft$`Technology/science`,airtemp$`Technology/science`), 
                         dim = c(length(meterbybuilding$`Technology/science`), 4))

utility_array = array(data = c(meterbybuilding$Utility, timebybuilding$Utility,sqft$Utility,airtemp$Utility), dim = c(length(meterbybuilding$Utility), 4))

warehouse_array = array(data = c(meterbybuilding$`Warehouse/storage`, timebybuilding$`Warehouse/storage`,sqft$`Warehouse/storage`,airtemp$`Warehouse/storage`), 
                        dim = c(length(meterbybuilding$`Warehouse/storage`), 4))


#We used split_outliers to split each of the above arrays into new arrays of low outliers, non-outliers, and high outliers.
#buildingtype_low is an array consisting of the meter reading x timestamp of the low outliers of the building type.
#buildingtype_non is an array consisting of the meter reading x timestamp of the non-outliers of the building type.
#buildingtype_high is an array consisting of the meter reading x timestamp of the high outliers of the building type.
#In declaring these new arrays with low outliers, non-outliers, and high outliers, we hope that the following groups after us will be 
#able to build fast and accurate forecasting models.

education_low = split_outliers(education_array)[[1]]
education_non = split_outliers(education_array)[[2]]
education_high = split_outliers(education_array)[[3]]

entertainment_low = split_outliers(entertainment_array)[[1]]
entertainment_non = split_outliers(entertainment_array)[[2]]
entertainment_high = split_outliers(entertainment_array)[[3]]

food_low = split_outliers(food_array)[[1]]
food_non = split_outliers(food_array)[[2]]
food_high = split_outliers(food_array)[[3]]

healthcare_low = split_outliers(healthcare_array)[[1]]
healthcare_non = split_outliers(healthcare_array)[[2]]
healthcare_high = split_outliers(healthcare_array)[[3]]

lodging_low = split_outliers(lodging_array)[[1]]
lodging_non = split_outliers(lodging_array)[[2]]
lodging_high = split_outliers(lodging_array)[[3]]

manufacturing_low = split_outliers(manufacturing_array)[[1]]
manufacturing_non = split_outliers(manufacturing_array)[[2]]
manufacturing_high = split_outliers(manufacturing_array)[[3]]

office_low = split_outliers(office_array)[[1]]
office_non = split_outliers(office_array)[[2]]
office_high = split_outliers(office_array)[[3]]

other_low = split_outliers(other_array)[[1]]
other_non = split_outliers(other_array)[[2]]
other_high = split_outliers(other_array)[[3]]

parking_low = split_outliers(parking_array)[[1]]
parking_non = split_outliers(parking_array)[[2]]
parking_high = split_outliers(parking_array)[[3]]

public_low = split_outliers(public_array)[[1]]
public_non = split_outliers(public_array)[[2]]
public_high = split_outliers(public_array)[[3]]

religious_low = split_outliers(religious_array)[[1]]
religious_non = split_outliers(religious_array)[[2]]
religious_high = split_outliers(religious_array)[[3]]

retail_low = split_outliers(retail_array)[[1]]
retail_non = split_outliers(retail_array)[[2]]
retail_high = split_outliers(retail_array)[[3]]

services_low = split_outliers(services_array)[[1]]
services_non = split_outliers(services_array)[[2]]
services_high = split_outliers(services_array)[[3]]

technology_low = split_outliers(technology_array)[[1]]
technology_non = split_outliers(technology_array)[[2]]
technology_high = split_outliers(technology_array)[[3]]

utility_low = split_outliers(utility_array)[[1]]
utility_non = split_outliers(utility_array)[[2]]
utility_high = split_outliers(utility_array)[[3]]

warehouse_low = split_outliers(warehouse_array)[[1]]
warehouse_non = split_outliers(warehouse_array)[[2]]
warehouse_high = split_outliers(warehouse_array)[[3]]


######################################################################
######################################################################

#for ggpair plots

religious_non_frame<-as.data.frame(religious_non)
names(religious_non_frame)[1]<- "timestamp"
names(religious_non_frame)[2]<- "meter_reading"
names(religious_non_frame)[3]<- "square_feet"
names(religious_non_frame)[4]<- "air_temperature"
ggpairs(data = religious_non_frame)

education_non_frame<-as.data.frame(education_non)
names(education_non_frame)[1]<- "timestamp"
names(education_non_frame)[2]<- "meter_reading"
names(education_non_frame)[3]<- "square_feet"
names(education_non_frame)[4]<- "air_temperature"
ggpairs(data = education_non_frame)

entertainment_non_frame<-as.data.frame(entertainment_non)
names(entertainment_non_frame)[1]<- "timestamp"
names(entertainment_non_frame)[2]<- "meter_reading"
names(entertainment_non_frame)[3]<- "square_feet"
names(entertainment_non_frame)[4]<- "air_temperature"
ggpairs(data=entertainment_non_frame)

food_non_frame<-as.data.frame(food_non)
names(food_non_frame)[1]<- "timestamp"
names(food_non_frame)[2]<- "meter_reading"
names(food_non_frame)[3]<- "square_feet"
names(food_non_frame)[4]<- "air_temperature"
ggpairs(data=food_non_frame)

healthcare_non_frame<-as.data.frame(healthcare_non)
names(healthcare_non_frame)[1]<- "timestamp"
names(healthcare_non_frame)[2]<- "meter_reading"
names(healthcare_non_frame)[3]<- "square_feet"
names(healthcare_non_frame)[4]<- "air_temperature"
ggpairs(data=healthcare_non_frame)

lodging_non_frame<-as.data.frame(lodging_non)
names(lodging_non_frame)[1]<- "timestamp"
names(lodging_non_frame)[2]<- "meter_reading"
names(lodging_non_frame)[3]<- "square_feet"
names(lodging_non_frame)[4]<- "air_temperature"
ggpairs(data=lodging_non_frame)

manufacturing_non_frame<-as.data.frame(manufacturing_non)
names(manufacturing_non_frame)[1]<- "timestamp"
names(manufacturing_non_frame)[2]<- "meter_reading"
names(manufacturing_non_frame)[3]<- "square_feet"
names(manufacturing_non_frame)[4]<- "air_temperature"
ggpairs(data=manufacturing_non_frame)

office_non_frame<-as.data.frame(office_non)
names(office_non_frame)[1]<- "timestamp"
names(office_non_frame)[2]<- "meter_reading"
names(office_non_frame)[3]<- "square_feet"
names(office_non_frame)[4]<- "air_temperature"
ggpairs(data=office_non_frame)

other_non_frame<-as.data.frame(other_non)
names(other_non_frame)[1]<- "timestamp"
names(other_non_frame)[2]<- "meter_reading"
names(other_non_frame)[3]<- "square_feet"
names(other_non_frame)[4]<- "air_temperature"
ggpairs(data=other_non_frame)

public_non_frame<-as.data.frame(public_non)
names(public_non_frame)[1]<- "timestamp"
names(public_non_frame)[2]<- "meter_reading"
names(public_non_frame)[3]<- "square_feet"
names(public_non_frame)[4]<- "air_temperature"
ggpairs(data=public_non_frame)

retail_non_frame<-as.data.frame(retail_non)
names(retail_non_frame)[1]<- "timestamp"
names(retail_non_frame)[2]<- "meter_reading"
names(retail_non_frame)[3]<- "square_feet"
names(retail_non_frame)[4]<- "air_temperature"
ggpairs(data=retail_non_frame)

services_non_frame<-as.data.frame(services_non)
names(services_non_frame)[1]<- "timestamp"
names(services_non_frame)[2]<- "meter_reading"
names(services_non_frame)[3]<- "square_feet"
names(services_non_frame)[4]<- "air_temperature"
ggpairs(data=services_non_frame)


technology_non_frame<-as.data.frame(technology_non)
names(technology_non_frame)[1]<- "timestamp"
names(technology_non_frame)[2]<- "meter_reading"
names(technology_non_frame)[3]<- "square_feet"
names(technology_non_frame)[4]<- "air_temperature"
ggpairs(data=technology_non_frame)

utility_non_frame<-as.data.frame(utility_non)
names(utility_non_frame)[1]<- "timestamp"
names(utility_non_frame)[2]<- "meter_reading"
names(utility_non_frame)[3]<- "square_feet"
names(utility_non_frame)[4]<- "air_temperature"
ggpairs(data=utility_non_frame)


warehouse_non_frame<-as.data.frame(warehouse_non)
names(warehouse_non_frame)[1]<- "timestamp"
names(warehouse_non_frame)[2]<- "meter_reading"
names(warehouse_non_frame)[3]<- "square_feet"
names(warehouse_non_frame)[4]<- "air_temperature"
ggpairs(data=warehouse_non_frame)
##########################################################################

#We fit the linear model for different types of buildings

#We Build a model for some of the buildings that had a good correlation at this 
#stage using the linear regression
#Warehouse
mlr_warehouse <- lm(meter_reading ~ square_feet+air_temperature, data=warehouse_non_frame)

summary(mlr_warehouse)

#Services
mlr_services <- lm(meter_reading ~ square_feet+air_temperature, data= services_non_frame)

summary(mlr_services)

#Religious
mlr_religious <- lm(meter_reading ~ square_feet+air_temperature, data= religious_non_frame)

summary(mlr_religious)

#Education
mlr_education <- lm(meter_reading ~ square_feet+air_temperature, data= education_non_frame)

summary(mlr_education)

#Entertainment
mlr_entertainment <- lm(meter_reading ~ square_feet+air_temperature, data= entertainment_non_frame)

summary(mlr_entertainment)

#Food

mlr_food <- lm(meter_reading ~ square_feet+air_temperature, data= food_non_frame)

summary(mlr_food)

#healthcare

mlr_healthcare <- lm(meter_reading ~ square_feet+air_temperature, data= healthcare_non_frame)

summary(mlr_food)

#lodging

mlr_lodging <- lm(meter_reading ~ square_feet+air_temperature, data= lodging_non_frame)

summary(mlr_lodging)

#Manufacturing

mlr_manufacturing <- lm(meter_reading ~ square_feet+air_temperature, data= manufacturing_non_frame)

summary(mlr_manufacturing)

#office

mlr_office <- lm(meter_reading ~ square_feet+air_temperature, data= office_non_frame)

summary(mlr_office)

#other

mlr_other <- lm(meter_reading ~ square_feet+air_temperature, data= other_non_frame)

summary(mlr_other)

#public
mlr_public <- lm(meter_reading ~ square_feet+air_temperature, data= public_non_frame)

summary(mlr_public)

#retail
mlr_retail <- lm(meter_reading ~ square_feet+air_temperature, data= retail_non_frame)

summary(mlr_retail)

#technology
mlr_technology <- lm(meter_reading ~ square_feet+air_temperature, data= technology_non_frame)

summary(mlr_technology)

#utility
mlr_utility <- lm(meter_reading ~ square_feet+air_temperature, data= utility_non_frame)

summary(mlr_utility)

#warehouse
mlr_warehouse <- lm(meter_reading ~ square_feet+air_temperature, data= warehouse_non_frame)

summary(mlr_warehouse)





##########################################################################
##########################################################################

test <- read_csv("test.csv")
building <- read_csv("building_metadata.csv")
weather_test <- read_csv("weather_test.csv")

rm(ashrae2,ashrae3,ashrae4,ashrae)

#Join individual datasets of train,building and weather variables
ashrae_test <- test %>%
  left_join(building, by = "building_id") %>%
  left_join(weather_test, by = c("site_id", "timestamp"))

percent_missing<-(colSums(is.na(ashrae_test))/nrow(ashrae_test))*100
print(percent_missing)

ashrae_test<-select (ashrae_test,-c(year_built,floor_count,cloud_coverage))

ashrae_test[, 8] <- na_interpolation(ashrae_test[, 8])
ashrae_test[, 9] <- na_interpolation(ashrae_test[, 9])
ashrae_test[, 10] <- na_interpolation(ashrae_test[, 10])
ashrae_test[, 11] <- na_interpolation(ashrae_test[, 11])
ashrae_test[, 12] <- na_interpolation(ashrae_test[, 12])
ashrae_test[, 13] <- na_interpolation(ashrae_test[, 13])

ashrae_test.df <- as.data.frame(ashrae_test)
Education_test_df <- subset(ashrae_test.df, primary_use=="Education")



lodging_residential_test_df <- subset(ashrae_test.df, primary_use=="Lodging/residential")


office_test_df<- subset(ashrae_test.df, primary_use=="Office")


entertainment_publicassembly_test_df <- subset(ashrae_test.df, primary_use=="Entertainment/public assembly")


retail_test_df<- subset(ashrae_test.df, primary_use=="Retail") 
 

other_test_df <- subset(ashrae_test.df, primary_use=="Other")
 

parking_test_df <- subset(ashrae_test.df, primary_use=="Parking")


food_test_df<- subset(ashrae_test.df, primary_use=="Food sales and service") 


healthcare_test_df <- subset(ashrae_test.df, primary_use=="Healthcare")
 

manufacturing_test_df<- subset(ashrae_test.df, primary_use=="Manufacturing/industrial") 


public_services_test_df <- subset(ashrae_test.df, primary_use=="Public services")
 

religious_test_df <- subset(ashrae_test.df, primary_use=="Religious worship")
 

services_test_df<- subset(ashrae_test.df, primary_use=="Services") 
 

technology_test_df<- subset(ashrae_test.df, primary_use=="Technology/science") 


utility_test_df<- subset(ashrae_test.df, primary_use=="Utility") 


warehouse_test_df <- subset(ashrae_test.df, primary_use=="Warehouse/storage") 

rm(airtem,ashrae_test,ashrae_test.df,building,education_array,education_high)
rm(education_non)
rm(education_non_frame,entertainment_array,entertainment_high,entertainment_low)
rm(entertainment_non,food_array,food_non)
predict_warehouse_non <- predict(mlr_warehouse, warehouse_test_df)
predict_services_non <- predict(mlr_services, services_test_df) 
predict_religious_non <- predict(mlr_religious, religious_test_df) 
predict_education_non <- predict(mlr_education,Education_test_df)
predict_entertainment_non <- predict(mlr_entertainment,entertainment_publicassembly_test_df)
predict_food_non <- predict(mlr_food,food_test_df)
predict_healthcare_non <- predict(mlr_healthcare,healthcare_test_df)
predict_lodging_non <- predict(mlr_lodging,lodging_residential_test_df )
predict_manufacturing_non <- predict(mlr_manufacturing,manufacturing_test_df)
predict_office_non <- predict(mlr_office,office_test_df)
predict_other_non <- predict(mlr_other,other_test_df)
predict_public_non <- predict(mlr_public,public_services_test_df)
predict_technology_now <- predict(mlr_technology,technology_test_df)
predict_utility_non <- predict(mlr_utility,utility_test_df)
predict_warehouse_non <- predict(mlr_warehouse,warehouse_test_df)

































