# install.packages("nameofpackage")
# data <- read_csv(’mystats.csv’)
head(iris)
tail(iris)
iris[1,2] # 1st row, 2nd column
iris[1:8,2] # 2nd column for rows 1 to 8
iris[1, ] # all the columns for the 1st row
iris[,2] # 2nd column for all the rows
iris$Sepal.Length # accessing the column called ‘Sepal.Length’
iris$Sepal.Length[1:8] # rows 1 to 8 of the column ‘Sepal.Length
summary(iris)
summary(iris$Sepal.Length)
subset(iris,iris$Sepal.Length < 5 & iris$Species != 'setosa')

plot(iris$Sepal.Length,iris$Sepal.Width)
plot(iris$Sepal.Length,iris$Sepal.Width,main="Iris Dataset Scatter Plot", xlab="Sepal Length", ylab='Sepal Width')
boxplot(iris[,1:4])
boxplot(iris[,1:4],col = c('red','blue','green','orange'),
        ylab='Measurements (cm)',
        main='Measurement Distributions')
hist(iris$Sepal.Length,main='Distribution of Lenght',
     xlab='Sepal Lenght',
     ylab='Relative Frequency', freq=FALSE,col=c('blue', 'red', 'green'), breaks=5)

# Tidyverse: dplyr for manipulation and ggplot for visualization
# filter(). arrange(), select(), mutate(), summarise(), group_by()
library(tidyverse)
library(nycflights13)
df <- flights
filter(df, month==1, day==1)
filter(flights, month %in% c(11, 12))
arrange(df, year, month , day)
arrange(df, desc(dep_delay))
select(flights, year, month, day)
select(flights, year:day) # all consecutive variables
select(flights, -(year:day)) # all except the ones in -()
# Helper functions
starts_with("abc") # matches names that begin with ”abc”
ends_with("xyz") # matches names that end with ”xyz”
contains("ijk") # matches names that contain ”ijk”
matches("(.)\\1") # selects variables that match a regular expression
num_range("x", 1:3) # matches x1, x2 and x3

mutate(df,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
# or transmute() to keep only the new data

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# Putting it all together:
y_dest <- group_by(flights, dest) # new dataframe grouped by destination
delay <- summarise(y_dest, # get number of flights
                   count = n(),
                   dist = mean(distance, na.rm = TRUE), # mean distance
                   delay = mean(arr_delay, na.rm = TRUE)) # and delay
delay <- filter(delay, count > 20, dest != "HNL") # keep obs that have at least 20 flights and not in Hawaii (HNL)

# Better attempt:
delays <- df %>% # We used the pipe operator “%>%” to chain the verbs into one “phrase"
  group_by(dest) %>%
  summarise(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")

#  %>%
#   select(the columns we need) %>%
#   filter(the rows we need) %>%
#   group_by(the variables we are interested) %>%
#   summarise(the stats we want to acquire) %>%
#   arrange(columns in the order we want)



# ggplot(data = df) + geom_function(aes(x = ., y = ., styles = .)
ggplot(data = mpg) + geom_point(aes(x = displ, y = hwy, color = class))

# Naive
ggplot(data = mpg) + geom_point(aes(x = displ, y = hwy, color=class)) + geom_smooth(aes(x = displ, y = hwy))
# Better
ggplot(mpg, aes( x = displ, y = hwy )) + geom_point(aes(color = class)) + geom_smooth

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(aes(color = class)) +
  geom_smooth() + labs(x = "Engine Displacement", y = "Mileage",
                       title = "My awesome scatterplot")
# geom_point (scatterplot)
# geom_boxplot (boxplot)
# geom_hist (histogram)
# geom_bar (bar plot)
# geom_smooth (fits a line using several methods
