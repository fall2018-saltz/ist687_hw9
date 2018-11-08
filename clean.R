
##################################################################
# Use this block comment at the top of each R code file you submit
# Homework 8 – Submitted by Yao Wang on November 7, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.
# IST 687. Due is November 7, 2018


# Part A: Explore Data Set
# 1)	Load the dataset: hotelSurveyBarriot.json (similar to HW8, but a different dataset)
# "Load JSON Libraries"
clean_data <- raw_data
# 2)	Name the dataframe hotelSurvey
hotelSurvey <- as.data.frame(clean_data)
View(hotelSurvey)
# Part B: Explore Data Set
# 1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
str(hotelSurvey)

# 2)	Map each numeric attribute to a category  – Since we want to create rules, 
# we should convert the attributes that have a numeric range into buckets (ex. low or high)
# Hint: For Survey attributes that range from 0 to 10 one can use the following:
# vBuckets <- replicate(length(vec), "Average")
# vBuckets[vec > 7] <- "High"
# vBuckets[vec < 7] <- "Low"
# For other attributes, you can use the following code:
# q <- quantile(vec, c(0.4, 0.6))
# vBuckets <- replicate(length(vec), "Average")
# vBuckets[vec <= q[1]] <- "Low"
# vBuckets[vec > q[2]] <- "High"

# the columns which have the number with 0 to 10 are using below function.
category_10 <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 7] <- "High"
  vBuckets[vec < 7] <- "Low"
  return(vBuckets)
}
# the columns which have the number greater than 10 are using below function.
category_0 <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
# the colums using category_10 function
hotelSurvey$overallCustSat <- category_10(hotelSurvey$overallCustSat)
hotelSurvey$checkInSat <- category_10(hotelSurvey$checkInSat)
hotelSurvey$hotelClean <- category_10(hotelSurvey$hotelClean)
hotelSurvey$hotelFriendly <- category_10(hotelSurvey$hotelFriendly)

# the colums using category_0 function
hotelSurvey$hotelSize <- category_0(hotelSurvey$hotelSize)
hotelSurvey$guestAge <- category_0(hotelSurvey$guestAge)
hotelSurvey$lengthOfStay <- category_0(hotelSurvey$lengthOfStay)
hotelSurvey$whenBookedTrip <- category_0(hotelSurvey$whenBookedTrip)

# make sure to document the code you use!!!
str(hotelSurvey)

#3)	Count the people in each category of for the age and friendliness attributes
# Hint: Use the table( ) command.
table(hotelSurvey$guestAge)
table(hotelSurvey$hotelFriendly)

# 4)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command
prop.table(table(hotelSurvey$guestAge))
prop.table(table(hotelSurvey$hotelFriendly))

# 5)	Show a “contingency table” of percentages for the age and the overall satisfaction variables together. 
# Write a block comment about what you see.
guestAge <- hotelSurvey$guestAge
overallCustSat <- hotelSurvey$overallCustSat
table_GO <- table(guestAge,overallCustSat)
prop.table(table_GO)

# The average guest age customers prefer to choose high overall customer satisfaction because high satisfaction is 0.0917. The second option is average satisfaction, 0.0703.
# Most high guest age customers prefer to choose high overall satisfaction,0.2602.
# Most low guest age customers prefer to choose low satisfaction, 0.2548.
# It make sense.

# Part C: Coerce the data frame into transactions
# 6)	Install and library two packages: arules and arulesViz.

library(arules)
library(arulesViz)

# 7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
# hotelSurveyX <- as(hotelSurvey,"transactions")
hotelSurveyX <- as(hotelSurvey,"transactions")

# 8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.
inspect(hotelSurveyX)
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX,support=0.05,cex.names=0.5)

# Part D: Use arules to discover patterns
# Support is the proportion of times that a particular set of items occurs relative to the whole dataset. 
# Confidence is proportion of times that the consequent occurs when the antecedent is present. See the review on the next page.  
# 9)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high – above 7).
rules <- apriori(hotelSurveyX,parameter=list(support=0.1, confidence=0.5),appearance = list(rhs="overallCustSat=High",default="lhs"))
plot(rules,jitter=0) 
goodRules <- rule[quality(rule)$lift > 2]

# 10)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
inspect(goodRules)

# 11)	 If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, 
# what would those two rules be?  Use a block comment to explain your answer.
top.lift <- sort(goodRules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 2))                                                        
    
# Sort the rules based on the value of lift, then choose the first two association rules.
# First
# {checkInSat=High,                                                                 
# hotelClean=High,                                                                 
# hotelFriendly=Average,                                                           
# whenBookedTrip=High}   => {overallCustSat=High}      lift: 2.089386
# Second
# {hotelClean=High,                                                                 
#  hotelFriendly=Average,                                                           
#  whenBookedTrip=High}   => {overallCustSat=High}     lift: 2.084722

# There are rules with high lift and high confidence.
# The lift value of the first rule is 2.089386 and the lift value of the second rule is 2.084722, it means that they are very close and higher than others.
# From my point of view, the two rules happen occurring together is higher. I will provide these two rules to the client.
