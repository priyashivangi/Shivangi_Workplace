

#Loading the Data files

setwd("C:/Users/Shivangi/Documents/Application/CapitalOne/Code/DataFiles")
gz_file <- gzfile("listings.csv.gz") # load the .gz file
Listings <- read.csv(gz_file,header = T) # read csv file
Zillow <- read.csv("Zip_Zhvi_2bedroom.csv")

#Dimension check for the data

dim(Listings)
dim(Zillow)

colnames(Listings)
colnames(Zillow)

#Data Merge

Zillow_May <- function(temp,cityName){ # Zillow data and New york city given as function arguement
  
  temp <- temp[,c(2,3,7,226:262)] # Select only relevant columns
  
  temp <- filter(temp,city==cityName) # Filter for the required city
  
  colnames(temp)[colnames(temp)=="RegionName"] <- "zipcode" # Set proper column name to be used for merging later
  
  temp$currentPrice <- NULL # Create a new column to store the latest price in May 2018
  # we define a for loop to iterate over each zipcode to obtain latest cost of property
  for(i in 1:nrow(temp)){
    
    tmp = ts(as.vector(t(temp[,c(4:40)])[,i]),start = c(2014,6),frequency = 12) # Convert the monthly cost data into time series data 
    
    ARIMAfit = arima(tmp, order=c(1,1,1), seasonal=list(order=c(1,0,1),period=NA), 
                     method="ML")# Define ARIMA model to be used for prediction
    
    pred = predict(ARIMAfit, n.ahead = 11)# use the ARIMA model to predict the price from July 2017 to May 2018
    
    predval <- pred$pred # Store the predicted values in a variable 
    
    temp$currentPrice[i] <- predval[length(predval)] # set the value of current price for the specific zipcode as price in May 2018
  }
  return(temp[,c(1,2,3,41)]) # return the filtered data containing only relevant columns
  
}

city="New York"
Zillow1 <- Zillow_May(Zillow,city) # call cleanZillow function
str(Zillow1) # view the structure of clean and filtered Zillow data

head(Zillow1,5)

#Filtering the Listing Data

filterList <- function(tempdf,numberOfBedroom){
  # Select only relevant columns
  relevantcol <- c("id","zipcode","bedrooms","square_feet","price","weekly_price","monthly_price","cleaning_fee","number_of_reviews","review_scores_rating")
  tempdf <- tempdf[,relevantcol]
  # filter data containing 2 bedrooms
  tempdf <- filter(tempdf,bedrooms==numberOfBedroom)
  return(tempdf)
}

bedrooms=2 # set required number of bedrooms
filteredListData <- filterList(Listings,numberOfBedroom) # call the function
str(filteredListData) # observe the structure of filtered listings data

head(filteredListData,5)

finalData <- merge(filteredListData,Zillow1,by = c("zipcode")) # merge data sets on zipcode
str(finalData)

#Setting the number of levels in the data to include only New York City

# Set the new column names 
colnames(finalData) <-  c("zipcode","id","bedrooms","square_feet","price","weekly_price",
                          "monthly_price","cleaning_fee","number_of_reviews","review_scores_rating","city","size_rank","current_price")

# Correct the factor levels in city column
finalData$city <- factor(finalData$city, levels=c("New York"))

#Removing the $ sign from the columns like price,weekly_price,monthly_price,cleaning_fee

# Specify a function to replace characters with whitespace

replace_dollar <- function(x){
  price <- as.numeric(gsub("[$,]","",x)) # this function removes $ from data
  return(price)
}

# Apply function to replace characters with whitespace

finalData[cols] <- lapply(finalData[cols], replace_dollar) # running the above defined function on cols

# Again check the structure of the data

str(finalData)

#creating the normalization function

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x))) # function to scale variables between 0 and 1
}

# Scale the number of reviews column

finalData["number_of_reviews"] <- lapply(finalData["number_of_reviews"], normalize) 

# Check the summary of column to ensure scaling has been applied

summary(finalData$number_of_reviews)

# Check number of NA in all the columns

missingValues <- as.data.frame(colSums(sapply(finalData,is.na)))  

# Convert ronames to columns

missingValues <- as.data.frame(setDT(missingValues, keep.rownames = TRUE))

# Rename the column names

colnames(missingValues) <- c("columnName","totalNA_values")

# Transform totalNA to percent, add it as column and arrange in descending order on the basis of it

missingValues <- missingValues %>% 
  mutate_at(vars(totalNA_values),funs(percentNA_values=.*100/nrow(finalData))) %>% 
  arrange(desc(percentNA_values)) 

# Check the top columns having maximum NA values 

head(missingValues,n=nrow(missingValues))

## Missing Data Imputation

# Subset the data to remove the constants

dataSet <- subset(finalData,select = -c(id,city)) # id and city are constant and will not help in imputation

# Impute the Missing Data using Mice Package

cat("Started...Imputing Missing Data using Calssification and Regression Tree 'CART'")

summary(finaldf_complete)

# Find average of the required columns

avg_df <- finaldf_complete %>% 
  group_by(zipcode) %>% 
  summarise_at(vars(square_feet:current_price),mean) # mean of current price and other price attributes

# Find unique id (no. of properties) for each zipcode

unique_id_df <- finaldf_complete %>% select(zipcode,id) %>% 
  group_by(zipcode) %>%
  mutate(unique_id = n_distinct(id)) %>% 
  select(zipcode,unique_id) %>% distinct() # count number of properties in each zipcode as unique_id

# Combine both the dataframes to get final summary of the data

summary_df <- inner_join(avg_df, unique_id_df, by = "zipcode") # combine two agregations using zipcode
summary(summary_df)


p_daily=.40 # probability that booking is done for a day

p_weekly=.40 # probability that booking is done for a week

p_month=.20 # probability that booking is done for a month

occupancy_rate <- .75

Quarter_1days <- 90 # number of days in quarter

Quarter_2days <- 180 # number of days in 2 quarters

Year_days <- 365 # number of days in a year

summary_df$Total_cost <- summary_df$current_price*summary_df$unique_id # cost of all properties

summary_df$Review_effect <- normalize(summary_df$review_scores_rating) # scale the review_scores_rating

summary_df$Review_effect <-ifelse(summary_df$Review_effect>0,summary_df$Review_effect,mean(summary_df$Review_effect)) 
# impute those properties which had no ratings 

# Generate the revenue for first quarter
summary_df$Revenue_by_q1 <-occupancy_rate*Quarter_1days*((p_daily*summary_df$price)+(p_weekly*summary_df$weekly_price/7)+
                                  (p_month*summary_df$monthly_price/30))*summary_df$unique_id*summary_df$Review_effect

# Generate the revenue for second quarter
summary_df$Revenue_by_q2 <-occupancy_rate*Quarter_2days*((p_daily*summary_df$price)+(p_weekly*summary_df$weekly_price/7)+
                                                           (p_month*summary_df$monthly_price/30))*summary_df$unique_id*summary_df$Review_effect

# Generate the revenue for first year
summary_df$Revenue_by_year<-occupancy_rate*Year_days*((p_daily*summary_df$price)+(p_weekly*summary_df$weekly_price/7)+
                                                        (p_month*summary_df$monthly_price/30))*summary_df$unique_id*summary_df$Review_effect

# Obtain the Revenue by amount spend ratio for first quarter
summary_df$Revenue_by_Cost_RatioQ1 <-normalize(summary_df$Revenue_by_q1/summary_df$Total_cost)    

# derive the percent of properties listed for the given zipcode
summary_df$Percent_units <- summary_df$unique_id*100/sum(summary_df$unique_id)

plot_my_graph <- function(col_name){
  # sort the dataframe for by col_name in descending order and subset for top N zipcodes
  
  v <- enquo(col_name)
  
  n=10 # select top n values
  
  df_sorted_unique_id <- arrange(summary_df[summary_df$unique_id>10,],desc(!!v)) [1:n,] # order data
  
  # reassign factor levels
  
  df_sorted_unique_id$zipcode <- factor(df_sorted_unique_id$zipcode)
  
  
  # Return zipcodes
  
  return(df_sorted_unique_id)
  
}



dff <- plot_my_graph(Percent_units) # call the ordering and filtering function using Percent units as key metric

dff$zipcode <- factor(dff$zipcode, levels = unique(dff$zipcode)[order(dff$Percent_units, decreasing = TRUE)])

# plot bar graph
dff %>%  plot_ly( x = ~zipcode, y = ~Percent_units, type = 'bar',marker = list(color = c('green','grey','grey','grey','grey','grey','grey','grey',
                                                                                         'grey','grey'))) %>%
  layout(title = "Top zipcodes with maximum percent of properties",
         xaxis = list(title = "Zipcodes"),
         yaxis = list(title = "Percent"))

# plot bar graph for quarter 1
plot_my_graph(Revenue_by_q1) %>%  plot_ly( x = ~zipcode, y = ~Revenue_by_q1, type = 'bar',marker = list(color = c('green','grey','grey',
                                                                                                'grey','grey','grey','grey','grey','grey','grey'))) %>%
  layout(title = "Top zipcodes by Revenue in Quarter 1",
         xaxis = list(title = "Zipcode"),
         yaxis = list(title = "Revenue"))

# plot bar graph for quarter 2
plot_my_graph(Revenue_by_q1) %>%  plot_ly( x = ~zipcode, y = ~Revenue_by_q1, type = 'bar',marker = list(color = c('green','grey','grey','grey','grey','grey','grey','grey','grey','grey'))) %>%
  layout(title = "Top zipcodes by Revenue in Quarter 2",
         xaxis = list(title = "Zipcode"),
         yaxis = list(title = "Revenue"))

# plot bar graph
plot_my_graph(Revenue_by_year) %>%  plot_ly( x = ~zipcode, y = ~Revenue_by_year, type = 'bar',marker = list(color = c('green','grey','grey','grey',
                                                                                                        'grey','grey','grey','grey','grey','grey'))) %>%
  layout(title = "Top Zipcodes by Revenue in a Year",
         xaxis = list(title = "Zipcode"),
         yaxis = list(title = "Revenue"))






