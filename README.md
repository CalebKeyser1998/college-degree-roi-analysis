# College Degree ROI by Gender

📊 **Overview**  
This project uses 2022 IPUMS microdata to analyze the difference in earnings between men and women with bachelor’s degrees. The analysis focuses on how degree field and gender impact average earnings.

👨‍💻 **Tools Used**  
- R  
- dplyr, tidyr, ggplot2, stargazer  
- Data from IPUMS 2022

🧠 **What I Did**  
- Filtered bachelor degree holders working full-time (30+ hours)
- Calculated average earnings by degree and gender
- Measured gender pay gap across fields
- Ran linear regressions for:
  - All degrees
  - Male-dominated fields
  - Female-dominated fields
- Visualized earnings vs gender ratios

📈 **Key Findings**
- Male-dominated degrees have higher average earnings for both genders
- There is a consistent gender pay gap across most degree fields
- Female-dominated degrees tend to pay less on average

📎 **Sample Visual**
![Graph1](https://github.com/user-attachments/assets/0c262532-cfc1-4704-bd75-d4dca3c572f6)
![Graph2](https://github.com/user-attachments/assets/54f57b44-5d17-4763-a427-6ad26f00e643)


  
## R Code

```r
# Loading the data and renaming it to "Df"
library(tidyr)
library(dplyr)
library(ggplot2)
library(stargazer)

load("~/SharedProjects/ECO495/ipums2022.rda")
Df <- ipums2022

# Converts the sex variable to a categorical variable "Male" when sex ==1, and "Female" when 0.
Df$sex <- ifelse(Df$sex == 1, "Male", "Female")

# Filters the Df data set to only include observations with a bachelor's degree, who is in the
# labor force, and "Usual hours worked per week" is greater than or equal to 30.
Bachelor_Degrees <- Df %>% filter(educd == "101" & labforce == "2" & uhrswork >= 30)

# Calculate the mean earnings for each gender and degree field.
MVW_Mean_Earnings_All <- Bachelor_Degrees %>% group_by(sex, degfieldd) %>% summarize(Mean_Earnings = mean(incearn, na.rm = TRUE))

#Pivots the MVW_Mean_Earnings_All data set to have male and female earnings in separate columns.
MVW_Mean_Earnings_Pivoted <- MVW_Mean_Earnings_All %>% pivot_wider(names_from = sex, values_from = Mean_Earnings)

# Calculates the percent difference in earnings between males and females for each degree field
MVW_Mean_Earnings_Pivoted$Difference <- ((MVW_Mean_Earnings_Pivoted$Male - MVW_Mean_Earnings_Pivoted$Female) / MVW_Mean_Earnings_Pivoted$Male) *100

#Calculates the mean percent difference for males and females for all degree fields.
Mean_Difference_Degrees <- mean(MVW_Mean_Earnings_Pivoted$Difference, na.rm = TRUE)
print(Mean_Difference_Degrees)

# Counts the number of males/females in each degree field.
male_female_counts_Bachelors <- Bachelor_Degrees %>% group_by(degfieldd, sex) %>% summarize(count = n()) %>% 
  pivot_wider(names_from = sex, values_from = count)

# Adds 2 columns to the male_female_counts_Bachelors data frame prop_female and male which shows
# the proportion of men/women in each degree field.
male_female_counts_Bachelors <- male_female_counts_Bachelors %>%
  mutate(prop_female = Female / (Female + Male), prop_male = Male / (Male+Female))

# Filters the male_female_counts_Bachelors variable to only include degree fields 
# where proportion of males is > .80
Male_Dom_Degrees <- male_female_counts_Bachelors %>% filter(prop_male > .80)

# Filters the male_female_counts_Bachelors variable to only include degree fields 
# where proportion of females is > .80
Female_Dom_Degrees <- male_female_counts_Bachelors %>% filter(prop_female > .80)

# Filters Bachelor_Degrees corresponding to those in Female_Dom_Degrees into a new variable 
Female_Dom_Degrees_Bachelors <- Bachelor_Degrees %>% filter(degfieldd %in% Female_Dom_Degrees$degfieldd)

# Filters Bachelor_Degrees corresponding to those in Male_Dom_Degrees into a new variable 
Male_Dom_Degrees_Bachelors <- Bachelor_Degrees %>% filter(degfieldd %in% Male_Dom_Degrees$degfieldd)

#Creates a regression of incearn on sex within male dominated bachelro degrees
Regression_Male <- lm(incearn ~ sex, data = Male_Dom_Degrees_Bachelors)
summary(Regression_Male)

#Creates a regression of incearn on sex within Female dominated bachelor degrees
Regression_Female <- lm(incearn ~ sex, data = Female_Dom_Degrees_Bachelors)
summary(Regression_Female)

#Creates a regression of incearn on sex within all bachelor degrees
Regression <- lm(incearn ~ sex,  data = Bachelor_Degrees)
summary(Regression)

# Filters the mean earnings of Males and Females into two new data frames 
Women_Summary <- MVW_Mean_Earnings_All %>% filter(sex == "Female")
Men_Summary <- MVW_Mean_Earnings_All %>% filter(sex == "Male")

# Assigns the mean earnings of bachelor degrees into a new variable and divides by 1000
Mean_Bachelors <- Bachelor_Degrees$incearn /10000

# Assigns the observations with "female" within bachelor degrees to a new variable
Women_Bachelors <- Bachelor_Degrees %>% filter(sex == "Female")

# Assigns the observations with "male" within bachelor degrees to a new variable
Men_Bachelors <- Bachelor_Degrees %>% filter(sex == "Male")

# Assigns the mean earnings of females with bachelor degrees into a new variable and divides by 1000
Mean_Women <- Women_Bachelors$incearn /10000

# Assigns the mean earnings of males with bachelor degrees into a new variable and divides by 1000
Mean_Men <- Men_Bachelors$incearn /10000

# Converts and combines Mean_Men, Mean_Women, and Mean_bachelors into a data frame
library(plyr)
DFMean_Men <- data.frame(Mean_Men = Mean_Men)
DFMean_Women <- data.frame(Mean_Women = Mean_Women)
DFMean_Bachelors <- data.frame(Mean_Bachelors = Mean_Bachelors)
combined_DF <- rbind.fill(DFMean_Men, DFMean_Women, DFMean_Bachelors)

# Makes a summary statistics table of combined_DF
stargazer(combined_DF,type = "text")

# Extracts the mean earnings of men from the Bachelor_Degrees variable
Men_Mean <- Bachelor_Degrees[Bachelor_Degrees$sex == "Male", "incearn"]

# Creates a regression table of the three regressions Regression, Regression_Male, Regression_Female
stargazer(Regression, Regression_Male, Regression_Female,
          title = "Table 1: Regression Results",
          dep.var.caption = "<center>Dependant Variable: Mean Earnings<center>",
          covariate.labels = c("Change in Earnings: Male (USD)", "Average Earnings: Female (USD)"),
          column.labels = c("All Degrees", "Male Dominated > (90%)", "Female Dominated > (90%)"),
          notes.label = "Significane levels",
          type = "html", out = "~/project1.htm")

# Creates a new column in the male_female_counts_Bachelors data frame that shows percentage of men/women
# in each degree
male_female_counts_Bachelors <- male_female_counts_Bachelors %>%
  mutate(percentage_male = prop_male * 100)

# Merges male_female_counts_Bachelors, MVW_Mean_Earnings_All, by "degfieldd"
merged_data <- merge(male_female_counts_Bachelors, MVW_Mean_Earnings_All, by = "degfieldd")

# Creates a line plot (male/female) with percentage of males in degfieldd on the x-axis, and
# mean earnings on the Y.
ggplot(merged_data, aes(x = percentage_male, y = Mean_Earnings, color = sex, group = sex)) +
  geom_smooth() +
  labs(x = "Percentage of Males in Degree Field", y = "Mean Earnings") +
  scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
  theme_minimal()

# Separates only observations including "male" into separate subsets, than runs regressions
# on mean earnings and percentage of males in the degfieldd.
male_data <- merged_data[merged_data$sex == "Male", ]
female_data <- merged_data[merged_data$sex == "Female", ]
lm_male <- lm(Mean_Earnings ~ percentage_male, data = male_data)
lm_female <- lm(Mean_Earnings ~ percentage_male, data = female_data)

# Creates a regression table of the lm_male and lm_female regression results.
stargazer(lm_male, lm_female,
          title = "Table 1: Regression Results",
          dep.var.caption = "<center>Dependant Variable: Mean Earnings<center>",
          covariate.labels = c("Change in Earnings (USD)", "Intercept (USD)"),
          column.labels = c("Male", "Female"),
          notes.label = "Significance levels",
          type = "html", out = "~/project2.htm")
