#This Question of RDA:
#1A
#call rio so we can read in the datasets and have tidyverse as we want "import" from that library 
library(rio)
library(tidyverse)
#copy the path of each dataset and add it ingo the import function 
survey_2021 <- import("C:/Users/thula/Downloads/survey_2021.csv") #always in "", always turn the backward slashes into forward slashes \ to /
survey_2022 <- import("C:/Users/thula/Downloads/survey_2022.csv")
survey_2023 <- import("C:/Users/thula/Downloads/survey_2023.csv")
#LET import the necessary library to ensure we read in the data
#now within this library want to access dplyr so we can use select 
library(dplyr) #we need it for binding_rows as well
#let us import the datasets into the environment tab. Then join them, select the necessary columns
#we will import the tibble so it easier to read
dataframe_of_2021 <- as_tibble(survey_2021)
dataframe_of_2022 <- as_tibble(survey_2022)
dataframe_of_2023 <- as_tibble(survey_2023)
#now we can select the specific variables from the scenario from each dataset 
after_survey_21 <- dataframe_of_2021 %>%
  select(YearsCode, MainBranch, Country, EdLevel,LanguageHaveWorkedWith, LanguageWantToWorkWith, DatabaseHaveWorkedWith, DatabaseWantToWorkWith,Age )%>%
  mutate(Survey_Year = 2021) #this is an indicator for 21
View(after_survey_21)
#we want to view the new survey22 after what has transpired 
after_survey_22 <- dataframe_of_2022 %>%
  select(YearsCode, MainBranch, Country, EdLevel,LanguageHaveWorkedWith, LanguageWantToWorkWith, DatabaseHaveWorkedWith, DatabaseWantToWorkWith,Age )%>%
  mutate(Survey_Year = 2022) #this is an indicator-permanent(hence hence mutate)
View(after_survey_22)
after_survey_23 <- dataframe_of_2023 %>%
  select(YearsCode, MainBranch, Country, EdLevel,LanguageHaveWorkedWith, LanguageWantToWorkWith, DatabaseHaveWorkedWith, DatabaseWantToWorkWith,Age ) %>%
  mutate(Survey_Year = 2023) #this is an indicator for survey 23
View(after_survey_23)

#Now we want to bind the "after_surveys" into one Yearly_Survey 
#so we will use the bind_rows-rows as the columns are literally the same except the rows. Rows are unique to each other Not cols as that would just stack everything next to each other resulting in duplicate columns/same headings 
Yearly_Surveys <- bind_rows(after_survey_21, after_survey_22, after_survey_23)
#now we want to see if it is binded/joined into 1 
head(Yearly_Surveys)

#For question B:
#let us look into country column
Yearly_Surveys %>%
  count(Country, sort=TRUE)

#now let us do the B: #we use mutate to bring permanent changes 
Yearly_Surveys <- Yearly_Surveys %>%
  mutate(Country = case_when(
    Country %in% c("Republic of Korea", "Korea, Republic of") ~ "South Korea",
    Country %in% c("United States of America", "USA") ~ "United States",
    Country %in% c("United Kingdom of Great Britain and Northern Ireland", "UK") ~ "United Kingdom",
    Country == "Democratic People's Republic of Korea" ~ "North Korea",
    Country == "Russian Federation" ~ "Russia",
    Country == "Iran, Islamic Republic of" ~ "Iran",
    Country == "Viet Nam" ~ "Vietnam",
    TRUE ~ Country
  ))

Yearly_Surveys %>%
  count(Country, sort = TRUE)

#1c:
#ket us begin with checking for the missing values 
is.na(Yearly_Surveys)
#let us be more interesting by getting the total number of missing values in each column
colSums(is.na(Yearly_Surveys))
#the variables that are important:
#objective according to the scenario is to identify the programming lanaguages and database users that are working with or find the popular in the industy
#what attributes/ variables align with these?
# we want remove missing values  from the critical 
Yearly_Surveys_after_cleaned <- Yearly_Surveys %>%
  filter(!is.na(Country) & !is.na(Age) & !is.na(YearsCode) & !is.na(MainBranch) & !is.na(EdLevel))

Yearly_Surveys_after_cleaned <- Yearly_Surveys_after_cleaned %>%
  mutate(across(c(LanguageHaveWorkedWith, LanguageWantToWorkWith,
                  DatabaseHaveWorkedWith, DatabaseWantToWorkWith),
                ~ ifelse(is.na(.), "", .)))

# Verify no missing values remain in critical columns
colSums(is.na(Yearly_Surveys_after_cleaned))

#pick the top 10 most frequent countries.
Countries_in_TOP10 <- Yearly_Surveys_after_cleaned %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Country)

Yearly_Surveys_finally <- Yearly_Surveys_after_cleaned %>%
  filter(Country %in% Countries_in_TOP10)

# Check the final country frequencies
Yearly_Surveys_finally %>%
  

  count(Country, sort = TRUE)

#Now we will look into question 2:
#a)Create categories for the various values in the Mainbranch column.
#let us verify the different catergories we have within this mainbranch column
unique(Yearly_Surveys_finally$MainBranch)
#based on what is seen in the  branch of what exist is the each one needs to be put properly in catergories
Yearly_Surveys_finally <- Yearly_Surveys_finally %>%
  mutate(MainBranch_Category = case_when(
    str_detect(MainBranch, "not primarily a developer") ~ "Coder On Occations",
    str_detect(MainBranch, "code primarily as a hobby") ~ "Hobby",
    str_detect(MainBranch, "developer by profession") ~ " Developer Professionally ",
    str_detect(MainBranch, "used to be a developer") ~ "Retired Developer",
    str_detect(MainBranch, "learning to code") ~ "Student",
    TRUE ~ "NONE"
  ))
#now we want to verify 
count(Yearly_Surveys_finally, MainBranch, MainBranch_Category)

#2b: Modify the categories of EdLevel to be more concise and encompass a wide range of educational backgrounds.
#let us check firstly in the column of the EDlevel the various values that exist in it 
unique(Yearly_Surveys_finally$EdLevel)
Yearly_Surveys_finally <- Yearly_Surveys_finally %>%
  mutate(EdLevel_Group = case_when(
    
    EdLevel %in% c("Primary/elementary school",
                   "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)")
    ~ "School Education",
    
    EdLevel %in% c("Associate degree (A.A., A.S., etc.)",
                   "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
                   "Some college/university study without earning a degree")
    ~ "Undergraduate Level",
    
    EdLevel %in% c("Master’s degree (M.A., M.S., M.Eng., MBA, etc.)",
                   "Professional degree (JD, MD, etc.)",
                   "Professional degree (JD, MD, Ph.D, Ed.D, etc.)")
    ~ "Postgraduate Level",
    
    EdLevel == "Other doctoral degree (Ph.D., Ed.D., etc.)"
    ~ "Doctoral Level",
    
    TRUE ~ "Other"
  ))
#let us verify what was performed above:
count(Yearly_Surveys_finally, EdLevel, EdLevel_Group)

#let us look at 2c:
library(ggplot2)
# Aggregate counts
trending_branch <- Yearly_Surveys_finally %>%
  count(Survey_Year, MainBranch_Category)

ggplot(trending_branch, aes(x = Survey_Year, y = n, color = MainBranch_Category, group = MainBranch_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Trend of Participants by Main Branch of the years",
       x = "Survey Year", y = "Number of Participants",
       color = "Main Branch Category") +
  theme_minimal()

#now we will look at question 2d:
# 1. Age vs MainBranch (boxplot)
ggplot(Yearly_Surveys_finally, aes(x = MainBranch_Category, y = Age, fill = MainBranch_Category)) +
  geom_boxplot() +
  labs(title = "Using Main Branch for distributing AGE",
       x = "Main Branch Category", y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # hide legend as x-axis already shows categories

# 2. Country vs MainBranch (proportion within each country)
# Since we already filtered to top 10 countries, we can use that.
branching_country <- Yearly_Surveys_finally %>%
  count(Country, MainBranch_Category) %>%
  group_by(Country) %>%
  mutate(proportion = n / sum(n))

ggplot(branching_country, aes(x = Country, y = proportion, fill = MainBranch_Category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Main Branch by Top 10 Countries",
       x = "Country", y = "Proportion", fill = "Main Branch") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Education Level vs MainBranch (proportion within each education group)
education_branch <- Yearly_Surveys_finally %>%
  count(EdLevel_Group, MainBranch_Category) %>%
  group_by(EdLevel_Group) %>%
  mutate(proportion = n / sum(n))

ggplot(education_branch, aes(x = EdLevel_Group, y = proportion, fill = MainBranch_Category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Using Education Level in Proportion of Main Branch ",
       x = "Education Level", y = "Proportion", fill = "Main Branch") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Years of Coding Experience vs MainBranch (boxplot)
ggplot(trending_branch, aes(x = Survey_Year, y = n, color = MainBranch_Category, group = MainBranch_Category)) +
    geom_line(linewidth = 1) +
     geom_point(size = 2) +
   labs(title = "Main Branch of the years of Trend of Participants",
                 x = "Survey Year", y = "Number of Participants",
                  color = "Main Branch Category") +
    theme_minimal()


#THIS IS QUESTION 3!!!
#In my console I typed show(Yearly_Surveys_finally)-this displays the "LanguageHaveworkedwith" - we can see here that the values within this variable are squashed into a row, separated by a ;
#3a: 
# ---Currently Databases used: ---
Database_currently <- Yearly_Surveys_finally %>%
  # Keep only rows that have at least one database (empty strings after cleaning are fine)
  filter(DatabaseHaveWorkedWith != "") %>%
  # Split the column so each database gets its own row
  separate_rows(DatabaseHaveWorkedWith, sep = ";") %>%
  # Trim any accidental whitespace (sometimes the data has spaces after semicolons)
  mutate(DatabaseHaveWorkedWith = str_trim(DatabaseHaveWorkedWith)) %>%
  # Count them!!
  count(DatabaseHaveWorkedWith, name = "count_used", sort = TRUE) %>%
  # Take top 5
  slice_head(n = 5)

# --- Databases wanted for future ---
Database_desired <- Yearly_Surveys_finally %>%
  filter(DatabaseWantToWorkWith != "") %>%
  separate_rows(DatabaseWantToWorkWith, sep = ";") %>%
  mutate(DatabaseWantToWorkWith = str_trim(DatabaseWantToWorkWith)) %>%
  count(DatabaseWantToWorkWith, name = "count_want", sort = TRUE) %>%
  slice_head(n = 5)

#now we want to display the top up currently and desired 
print(paste0("Top 5 databases currently used:",Database_currently ))

print(paste0("Top 5 databases wanted in the future:", Database_desired))

#3B: 
#now I need to repeat the above process for the "LanguageHaveWorkedWith and LanguageWantToWorkWith" 
Languages_worked_with <- Yearly_Surveys_finally %>%
  # Keep only rows that have at least one database (empty strings after cleaning are fine)
  filter( LanguageHaveWorkedWith!= "") %>%
  # Split the column so each database gets its own row
  separate_rows(LanguageHaveWorkedWith, sep = ";") %>%
  # Trim any accidental whitespace (sometimes the data has spaces after semicolons)
  mutate( LanguageHaveWorkedWith= str_trim(LanguageHaveWorkedWith)) %>%
  # Count them!!
  count(LanguageHaveWorkedWith, name = "count_used", sort = TRUE) %>%
  # Take top 5
  slice_head(n = 5)

# --- Languages desired for future ---
Languages_desired <- Yearly_Surveys_finally %>%
  filter(LanguageWantToWorkWith != "") %>%
  separate_rows(LanguageWantToWorkWith, sep = ";") %>%
  mutate( LanguageWantToWorkWith= str_trim(LanguageWantToWorkWith)) %>%
  count(LanguageWantToWorkWith, name = "count_want", sort = TRUE) %>%
  slice_head(n = 5)
#now we want to display the top up currently and desired langauges
knitr::kable(Languages_worked_with, caption = "Top 5 Language worked with:")
knitr::kable(Languages_desired,  caption = "Top 5 Languages desired to work with in Future")


#now we want to display the top up currently and desired langauges
knitr::kable(Database_currently, caption = "Top 5 Databases Currently Used")
knitr::kable(Database_desired,  caption = "Top 5 Databases Wanted in Future")

#now for 3c:I need to use those specific top databases to visualise trends over the years
#so we:
#we will assign the two to new variables
top_used <- Database_currently$DatabaseHaveWorkedWith
top_wanted <- Database_desired$DatabaseWantToWorkWith

#now will combine these two into 1 
top_databases_used_and_wanted <- union(top_used, top_wanted)
#now let us see this
show(top_databases_used_and_wanted)
# Count used databases per year
databases_used_already <- Yearly_Surveys_finally %>%
  filter(DatabaseHaveWorkedWith != "") %>%
  separate_rows(DatabaseHaveWorkedWith, sep = ";") %>%
  mutate(DatabaseHaveWorkedWith = str_trim(DatabaseHaveWorkedWith)) %>%
  filter(DatabaseHaveWorkedWith %in% top_databases_used_and_wanted) %>%
  group_by(Survey_Year, DatabaseHaveWorkedWith) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(category = "Used Right Now")

# Count wanted databases per year
desired_in_years <- Yearly_Surveys_finally %>%
  filter(DatabaseWantToWorkWith != "") %>%
  separate_rows(DatabaseWantToWorkWith, sep = ";") %>%
  mutate(DatabaseWantToWorkWith = str_trim(DatabaseWantToWorkWith)) %>%
  filter(DatabaseWantToWorkWith %in% top_databases_used_and_wanted) %>%
  group_by(Survey_Year, DatabaseWantToWorkWith) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(category = "Desired to work with")

# Rename the database column to a common name for binding
databases_used_already<- databases_used_already %>%
  rename(Database = DatabaseHaveWorkedWith)

desired_in_years <-  desired_in_years %>%
  rename(Database = DatabaseWantToWorkWith)

# Combine the two data sets
databases_trends_seen <- bind_rows(databases_used_already ,desired_in_years)

#now we will visualize:
ggplot(databases_trends_seen, aes(x = factor(Survey_Year), y = count, fill = category)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Database, scales = "free_y") +
  labs(title = " Using Years to display Popularity or Trends OF DATABASES",
       x = "Year of Survey", y = "Number of mentions",
       fill = "Category") +
  theme_minimal()

#NOW WE WILL LOOK INTO QUESTION 4
#Question 4.a:
library(tidyr)
library(stringr)

#we need to now plot the "current" languages 
Languages_worked_with_2 <- Languages_worked_with$LanguageHaveWorkedWith

Languages_worked_with_2 <- Yearly_Surveys_finally %>%
  # Keep only rows that have at least one database (empty strings after cleaning are fine)
  filter( LanguageHaveWorkedWith!= "") %>%
  # Split the column so each database gets its own row
  separate_rows(LanguageHaveWorkedWith, sep = ";") %>%
  # Trim any accidental whitespace (sometimes the data has spaces after semicolons)
  mutate( LanguageHaveWorkedWith= str_trim(LanguageHaveWorkedWith)) %>%
  filter(LanguageHaveWorkedWith %in% Languages_worked_with_2) %>%
  group_by(Survey_Year,LanguageHaveWorkedWith)%>%
  # Count them!!
  summarise(count=n(), .group="drop")

#now we will plot the above 
ggplot(Languages_worked_with_2, aes(x = factor(Survey_Year), y = count, fill = LanguageHaveWorkedWith)) +
  geom_col(position = "dodge") +
  labs(title = "Trend of Top 5 Programming Languages currently used(2021–2023)",
       x = "Year Of the Survey", y = "Number of Users", fill = "Language") +
  theme_minimal()

#4b:
#we will create a function that will handle the values in the "YearsCode" that are not numeric hence the stringr function library called 
yearsInConversion <- function(t) {
  case_when(
    #the below handles the values that are not in proper numeric form like the other values in the variable "YearsCode"
    str_detect(t, "More than 50 years") ~ 51,
    str_detect(t, "Less than 1 year") ~ 0.50, #this value is seen in the "unique" function
    TRUE ~ as.numeric(t)
  )
}
yearsInConversion <- function(x){
  case_when(
    x == "Less than 1 year" ~ 0.5,
    x == "More than 50 years" ~ 51,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}
Yearly_Surveys_finally <- Yearly_Surveys_finally %>%
  mutate(
    YearsCode_2 = case_when(
      YearsCode == "Less than 1 year" ~ 0.5,
      YearsCode == "More than 50 years" ~ 51,
      TRUE ~ as.numeric(YearsCode)
    )
  )
sum(is.na(Yearly_Surveys_finally$YearsCode_2))
#now we will use the unique to verify 
Yearly_Surveys_finally %>%
  select(YearsCode, YearsCode_2)
#now we want to display the top up currently and desired langauges
knitr::kable(Database_currently, caption = "Top 5 Databases Currently Used")
knitr::kable(Languages_worked_with, caption = "Top 5 Language worked with:")
#now we need a binary indicator for database and language currently:

#we need our top 5s as a character vector
databases_top5 <- Database_currently$DatabaseHaveWorkedWith
for (database in databases_top5){
  col_name <- paste0("DATAB_", make.names(database))
  Yearly_Surveys_finally <- Yearly_Surveys_finally %>%
    mutate(!!col_name := str_detect(DatabaseHaveWorkedWith, fixed(database)))
}

languages_top_5<- Languages_worked_with$LanguageHaveWorkedWith
for (language in languages_top_5) {
  col_name <- paste0("LAN_", make.names(language))
  Yearly_Surveys_finally <- Yearly_Surveys_finally %>%
    mutate(!!col_name := str_detect(LanguageHaveWorkedWith, fixed(language)))
}
grep("DATAB_", names(Yearly_Surveys_finally), value = TRUE)
grep("LAN_", names(Yearly_Surveys_finally), value = TRUE)

#now we want to reshape and plot:
database__ <- Yearly_Surveys_finally %>%
  select(YearsCode_2, starts_with("DATAB_")) %>%
  pivot_longer(
    cols = starts_with("DATAB_"),
    names_to = "database",
    values_to = "uses",
    names_prefix = "DATAB_"
  )
laguage__ <- Yearly_Surveys_finally %>%
  select(YearsCode_2, starts_with("LAN_")) %>%
  pivot_longer(
    cols = starts_with("LAN_"),
    names_to = "language",
    values_to = "uses",
    names_prefix = "LAN_"
  )
library(ggplot2)

str(database__)
str(laguage__)
ggplot(database__, aes(x = uses, y = YearsCode_2, fill = uses)) +
  geom_boxplot() +
  facet_wrap(~ database) +
  scale_x_discrete(labels = c("FALSE" = "Non-user", "TRUE" = "User")) +
  labs(title = "Years vs Usage of Top Databases",
       x = "Database Usage", y = "Years Coding") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(laguage__, aes(x = uses, y = YearsCode_2, fill = uses)) +
  geom_boxplot() +
  facet_wrap(~ language) +
  scale_x_discrete(labels = c("TRUE" = "User")) +
  labs(title = "Years Coding Experience vs Programming Languages",
       x = "Language User", y = "Years Coding") +
  theme_minimal() +
  theme(legend.position = "none")