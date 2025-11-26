library(tidyverse)

# 1.1
df <- iris # keep iris as the name, its more clear
head(df)

# 1.2
names(df)
select(df, Sepal.Length, Sepal.Width) # head(iris%>%select(Sepal.Length,Sepal.Width))
select(df, -Petal.Width) # head(iris%>%select(-c(Petal.Width)))
select(df, starts_with("p"))
filter(df, Sepal.Length >= 4.6, Petal.Width >= 0.5)
arrange(df, Sepal.Width)
arrange(df, Sepal.Width, desc(Sepal.Length))
mutate(df, Sepal.Proportion = Sepal.Length / Sepal.Width) # Sepal.Proportion <- iris$Sepal.Length/iris$Sepal.Width
avg_length <- summarise(df, mean(Sepal.Length))
max_length <- summarise(df, max(Sepal.Length))
min_length <- summarise(df, min(Sepal.Length))
# iris %>% summarise(avg_length=mean(Sepal.Length),max_length=max(Sepal.Length),min_length=min(Sepal.Length))
by_species <- group_by(df, Species)
avg_length <- summarise(by_species, mean(Sepal.Length))
max_length <- summarise(by_species, max(Sepal.Length))
min_length <- summarise(by_species, min(Sepal.Length))
# iris %>% group_by(Species) %>% summarise(avg_length=mean(Sepal.Length),max_length=max(Sepal.Length),min_length=min(Sepal.Length)
ggplot(data = df) + geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))
# Or: ggplot(iris,mapping = aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point()
# As a whole they seem correlated. You can see that the setosa species is in its own little group, completely separate of Versicolor and virginca
# Oops, I had laready colored them.
ggplot(data = df) + geom_boxplot(aes(x = Species, y = Sepal.Length, color = Species, fill = Species), alpha = 0.2)


# 2.1
source("http://www.openintro.org/stat/data/cdc.R")
# 2.2
df <- cdc # keep cdc as the name, its more clear
names(df)
head(df)
# 9 variables: 
# genhlth - Categorical, Ordinal
# exerany - Categorical, Nomial
# hlthplan - Categorical, Nomial
# smoke100 - Categorical, Nomial
# height - Numeric, Discrete
# weight - Numeric, Discrete
# wtdesire - Numeric, Discrete
# age - Numeric, Discrete
# gender - Categorical, Nomial

# BETTER
# summary(df)
# Since the categories are now seen as numerical, we should transform them into factors.
# df$exerany <- factor(df$exerany)
# df$hlthplan <- factor(df$hlthplan)
# df$smoke100 <- factor(df$smoke100)
# Now the summary makes sense:
# summary(df)

summary(df$height)
summary(df$age)
quantile(df$height)
quantile(df$age)
IQR(df$height)
IQR(df$age)
prop.table(table(df$gender))
prop.table(table(df$exerany))
table(df$gender)
# 9569 males
prop.table(table(df$genhlth))
# 0.23285 (WRONG, this is of all people)
# Correct: addmargins(prop.table(table(cdc$gender,cdc$genhlth),1))
mosaicplot(addmargins(table(df$smoke100, df$gender)))
# Men smoke more than woman
mutate(df, under23_and_smoke = age < 23 & smoke100 == 1)
df %>% mutate(df, bmi = weight/(height**2)*703) %>%
ggplot() + geom_boxplot(aes(x = genhlth, y = bmi, color = genhlth, fill = genhlth), alpha = 0.2)
# In general, the worse your health, the higher your BMI.
ggplot(data = df) + geom_point(aes(x = weight, y = wtdesire))
# They have a strong correlation, however we can see this correlation becomes weaker when someones weight increases.
weight_happiness <- df%>%select(weight, wtdesire)
round(cor(weight_happiness),digits=2)
# We can see a correlation of 0.8 which is pretty high
mutate(df, wdiff = wtdesire-weight)
# 0 means they have their desired weight. Positive means they need to gain weight, negative means they need to lose weight.
wdiff_data <- transmute(df, wdiff = wtdesire-weight)
ggplot(data = df) + geom_point(aes(x = weight, y = wtdesire, color=gender))
summary(wdiff_data)
as.numeric(names(table(wdiff_data)[table(wdiff_data) == max(table(wdiff_data))]))
# We could use a histogram here
# mean: On average people want to lose about 14.5 lbs.
# mode: Most people are happy with their current weight.
# shape: Peoples weight and desired weight are strongly correlated.
# spread: The less you weight, the more likely you are to be your desired weight.
# All in all, most people are pretty pleased with their weight.
df %>%
  mutate(df, wdiff = wtdesire-weight) %>%
  group_by(gender) %>%
  summarise(mean = mean(wdiff), mode = as.numeric(names(table(wdiff_data)[table(wdiff_data) == max(table(wdiff_data))])))
df %>% mutate(df, wdiff = wtdesire-weight) %>% ggplot() + geom_boxplot(aes(x = gender, y = wdiff, color = gender, fill = gender), alpha = 0.2)
# On average men are a little happier with their weight, but the difference is small.
# In both genders most people are happy with their weight.