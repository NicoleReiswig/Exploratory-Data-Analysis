#Check working directory
getwd()

#data profiling 
str("~/MSDA/churn_clean.csv") 
str("C:/Users/ntrei/OneDrive/Documents/MSDA/churn_clean.csv") 

#dimension of churn_clean [in-text citation: (R programming 101, n.d.)]
dim(churn_clean)
library(tidyverse)
glimpse(churn_clean)

#rename column names item 1- 8 [in-text citation: (Zach, 2022)]
colnames(churn_clean)[colnames(churn_clean) == 'Item1'] <- 'Timely_Response'
colnames(churn_clean)[colnames(churn_clean) == 'Item2'] <- 'Timely_Fixes'
colnames(churn_clean)[colnames(churn_clean) == 'Item3'] <- 'Timely_Replacements'
colnames(churn_clean)[colnames(churn_clean) == 'Item4'] <- 'Reliability'
colnames(churn_clean)[colnames(churn_clean) == 'Item5'] <- 'Options'
colnames(churn_clean)[colnames(churn_clean) == 'Item6'] <- 'Respectful_Response'
colnames(churn_clean)[colnames(churn_clean) == 'Item7'] <- 'Courteous_Exchange'
colnames(churn_clean)[colnames(churn_clean) == 'Item8'] <- 'Active_Listening'

#Verify columns were re-named successfully
glimpse(churn_clean)

#explore data variables [in-text citation: (R programming 101, n.d.)]
View(sort(table(churn_clean$Tenure)))
View(sort(table(churn_clean$Churn)))
View(sort(table(churn_clean$Timely_Response)))
View(sort(table(churn_clean$Timely_Fixes)))

barplot(sort(table(churn_clean$Churn)))
barplot(sort(table(churn_clean$Tenure)))
barplot(sort(table(churn_clean$Timely_Response)))
barplot(sort(table(churn_clean$Timely_Fixes)))

mean(churn_clean$Tenure)
mean(churn_clean$Timely_Response)
mean(churn_clean$Timely_Fixes)

hist(churn_clean$Tenure) 
hist(churn_clean$Timely_Response) 
hist(churn_clean$Timely_Fixes) 

b <- boxplot(churn_clean$Tenure) 
b <- boxplot(churn_clean$Timely_Response) 
b <- boxplot(churn_clean$Timely_Fixes) 

churn_clean %>%
  summarise(mean(Timely_Response), mean(Timely_Fixes), mean(Tenure))

churn_clean %>%
  summarise(median(Timely_Response), median(Timely_Fixes), median(Tenure))

#Create a table with data columns
My_data <- churn_clean %>%
  select(Tenure, Timely_Response, Timely_Fixes) %>%
  drop_na()

#Graph data
library(ggplot2)
  ggplot(data = My_data, 
  mapping =  aes(x = Tenure, y = Timely_Response)) +
    geom_point(color = "cornflowerblue",
               alpha = .7,
               size = 2) +
    geom_smooth(method = "lm", 
                se = FALSE, 
                linewidth = 1.5)
    

#ANOVA (R programming 101, n.d.)]
#Research question: What is the effect of Timely_Response on the length of tenure?
#Hypothesis: Ho- there is a correlation between the mean tenure and the survey 
#question  H1- there is not a correlation between the mean tenure and the survey question
#Calculate test statistic

My_data %>%
  select(Tenure, Timely_Response, Timely_Fixes) %>%
  drop_na()

My_anova <- 
  aov(My_data$Timely_Response ~ My_data$Tenure, data = My_data)

summary(my_anova)

My_anova2 <- aov(My_data$Timely_Fixes ~ My_data$Tenure, data = My_data)

summary(My_anova2)

#univariate graphs
hist(My_data$Tenure) 
hist(My_data$Timely_Response) 
hist(My_data$Timely_Fixes) 
hist(churn_clean$Income)


My_data %>%
  drop_na(Timely_Response) %>%
  ggplot(aes(Timely_Response)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

My_data %>%
  drop_na(Timely_Fixes) %>%
  ggplot(aes(Timely_Fixes)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

My_data %>%
  drop_na(Tenure) %>%
  ggplot(aes(Tenure)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

churn_clean %>%
  drop_na(Income) %>%
  ggplot(aes(Income)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

#Bivariate graphs

My_data %>%
  drop_na(Timely_Response) %>%
  ggplot(aes(Tenure, Timely_Response)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

My_data %>%
  drop_na(Timely_Fixes) %>%
  ggplot(aes(Tenure, Timely_Fixes)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()

churn_clean %>%
  drop_na(Income) %>%
  ggplot(aes(Tenure, Income)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()


#Create a heatmap
My_data %>%
  drop_na(Timely_Response) %>%
  ggplot(aes(Tenure, Timely_Response)) +
  geom_bin2d()

My_data %>%
  drop_na(Timely_Fixes) %>%
  ggplot(aes(Tenure, Timely_Fixes)) +
  geom_bin2d()

#Export code
system("C:/Users/ntrei/OneDrive/Documents/MSDA/executable_code207.r")  


