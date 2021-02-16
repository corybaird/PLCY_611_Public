# Part 1: Set up the directory and folders

# R
#1 Find working directory
path = getwd()
#2 Make new folder
dir.create('hw1')
#3 Change working directory
setwd('hw1')
#4 Create dta folder
dir.create('dta')
#5 Return to hw folder
setwd(path+'/hw1')


## 1.1 Import libraries
## Step 1
install.packages('dplyr') #data manipulation library
install.packages('foreign') #Import dta library for stata 5-12
install.packages("readstata13")  #Import dta library for stata 13
install.packages('ggplot2')

## Step 2
library(dplyr)
library(foreign)
library(readstata13)
library(ggplot2)

## 1.2 Import data from github
url = "https://github.com/corybaird/PLCY_611_Public/blob/main/Sessions/Session_1/session1.dta?raw=true"
df = read.dta13(url)

# Part 2

## 2.1 Describe

#DPLYR
df %>% str()

#Non-DPLYR
str(df)

## 2.2 Check unique id


## 2.3 Check missing values

### 2.3.1 Summary lists NAs for each variable
df %>% 
  summary()


### 2.3.1 sum total number of nas
sum(is.na(df))


### 2.3.2 Filter any rows containing na
df %>% filter_all(any_vars(is.na(.))) 


## 2.4 Summarize data
df %>% 
  summary()


## 2.5 
df %>% 
  select(sex, AGE, urban, yrseduc) %>% 
  summary()


## 2.6 generate variable

df %>% 
  mutate(age_group = 
           ifelse(AGE>=15 & AGE<=24, 1 , 
                  ifelse(AGE>=15 & AGE<=24,2,
                         ifelse(AGE>=35 & AGE<=54,3,4
                         )))
                  )

## 2.7 Relabel

df %>% 
  rename('age'='AGE') %>% 
  names()

## 2.8 Recode

df %>% 
  mutate(
    marital = recode(marital, '2'=0, '1'=1)
           ) %>% 
  select(marital)


# Part 3

## 3.1 Generate dummy for ages between 15-34
#Note: similar to 2.6

df %>% 
  mutate(
    youth = 
      case_when(AGE>=15 & AGE<=34~1,
                TRUE~0) # Sets all other obs to zero
    )

## 3.2 Working age population
#Note: remember in order to save our work we need to either overwrite our data by setting the data name = the function or writing a new name

# This example overwrites our data
df = df %>% 
  mutate(working_age = as.numeric(AGE>=15 & AGE<=64))

#Show the column names and notice working_age is now a column
df %>% names()


# 3.3 Children in household
df = df %>% mutate(baby = as.numeric(AGE<=5))

df %>% 
  group_by(hhid) %>% 
  summarise(
    sum(baby)
  )


# 3.4 Sort by sub district average

df %>% 
  group_by(subdistrict) %>% 
  filter(!is.na(yrseduc)) %>%  #filter out na: alternative is yrseduc!="NA"
  summarise(
    mean(yrseduc)
  )


# 3.5 Count the observations meeting criterion
### 3.5.1 Avg education for all
avg_edu = df %>% filter(!is.na(yrseduc)) %>% pull(yrseduc) %>% mean()
avg_edu 

### 3.5.2 Show answer
df %>% 
  group_by(subdistrict) %>% 
  filter(!is.na(yrseduc)) %>%  #filter out na: alternative is yrseduc!="NA"
  summarise(
    mean_edu = mean(yrseduc)
  ) %>% 
  filter(mean_edu>avg_edu)

### 3.5.3 Count
df %>% 
  group_by(subdistrict) %>% 
  filter(!is.na(yrseduc)) %>%  #filter out na: alternative is yrseduc!="NA"
  summarise(
    mean_edu = mean(yrseduc)
  ) %>% 
  filter(mean_edu>avg_edu) %>% 
  count()


# Part 4

## 4.1 Histogram

hist(df$yrseduc)


## 4.2 Scatter

plot(df$age1stmarr, df$yrseduc)


# Part 5

## 5.1 Compare educational attainment between unemployed and employed
df %>% 
  group_by(unemp1) %>% 
  summarise(mean_ed = mean(yrseduc)) %>% 
  na.omit() %>% 
  ggplot(aes(x=unemp1, y=mean_ed))+ geom_bar(stat="identity") + 
  xlab('Attendance (%)') + 
  ylab('Age')






