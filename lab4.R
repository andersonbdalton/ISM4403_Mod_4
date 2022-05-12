# importing data also taking care of nas
dataset = read.csv('lab4_flat_file.csv')
head(dataset)
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("modeest")
#load dplyr
library(dplyr)
library(caret)
library(modeest)

# encoding the dataset
names(dataset) <- c("id", "height", "gender", "hair_color", "eye_color", "age")
gender_num <- factor(dataset$gender, level = c("female", "male"), labels =c(1,2))
hair_color_num <- factor(dataset$hair_color, level = c("brown", "blond", "red"), labels =c(1,2,3))
eye_color_num <- factor(dataset$eye_color, level = c("brown", "green", "blue"), labels =c(1,2,3))
head(dataset)

# summary of data
mean(dataset$height)
sum(dataset$height)
# trying to get mode
getMode <- function(x) {
  keys <- unique(x)
  keys[which.max(tabulate(match(x, keys)))]
}

# I got the mode formula from https://www.tutorialspoint.com/r/r_mean_median_mode.htm

gendermode <- getMode(gender_num)
print(gendermode)

hairmode <- getMode(hair_color_num)
print(hairmode)

eyemode <- getMode(eye_num)
print(eyemode)


# summary of data

summary(dataset)
count_gender <- table(dataset$gender)
count_hair <- table(dataset$hair_color)
count_eye  <- table(dataset$eye_color)


##     pie and line chart
count(dataset$gender)

pie(count_gender)
pie(count_hair)
pie(count_eye)

# line chart

plot(y = dataset$height , x = dataset$age, type = "l", 
     title(main = "Realtionship Between Height & Age",
     xlab = "Age", ylab = "Height"))
# no there is no relationship between height and age. All the people in this dataset are <20 years old.
# meaning there is not connetion between the two.

##    part 2 quantmod

#install.packages("quantmod")
library("quantmod")

# apple stock AAPL
getSymbols("AAPL", src = "yahoo")
addMACD()
addBBands()
# boeing stock BA
getSymbols("BA", src = "yahoo")
# nvida stock
getSymbols("NVDA", src = "yahoo")
# AMD stock
getSymbols("AMD", src = "yahoo")
# intel stock
getSymbols("INTC", src = "yahoo")

# apple stock AAPL
getSymbols("AAPL", src = "yahoo")
#   use chart series
addMACD()
addBBands()

mean_apple <- mean(AAPL$AAPL.Close)
mean_amd <- mean(AMD$AMD.Close)
mean_ba <- mean(BA$BA.Close)
mean_intc <- mean(INTC$INTC.Close)
df2 <- data.frame(mean_apple, mean_amd, mean_ba, mean_intc)
# you need this make the df a matrix before you can plot the data
df2 = as.matrix(df2)
merge(mean_apple, mean_amd, mean_ba, mean_intc, all = TRUE)

barplot(df2, main = "Mean Closing Price", 
        xlab = "Stock Ticker", ylab = "Price in USD", las = 1)

