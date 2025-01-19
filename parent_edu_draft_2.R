#Esha Modi
#23 November 2024
#differences in preferences between parents with high and low educational attainment
#Regress rank of each category, and each dimension on educational attainment


#libraries 
library(ggplot2)
library(data.table)
library(stringr)
library(readxl)
library(gt)
library(fixest)
library(MASS)
library(magrittr)
library(bit64)
library(dplyr)
library(gt)
library(tidyr)
library(webshot2)
library(stargazer)
library(estimatr)
library(broom)
library(xtable)
library(fixest)
library(dplyr)






dpsnpath <- "~/Desktop/RA/parent_edu"
outpath <- "~/Desktop/RA/parent_edu/output"

#dataframe 
rankings <- fread(file.path(dpsnpath, "rankings_unique.csv"))
demos <- fread(file.path(dpsnpath, "demos_combined.csv"))

asp9 <- c("Your child's empathy for others", "Your child's interpersonal skills", "Your child's leadership and initiative",
          "Your child's literacy skills", "Your child's mathematical skills", "Your child's perseverance and growth mindset",
          "Your child's scientific literacy", "Your child's emotional self-awareness and regulation", "Your child's collaboration and teamwork skills")

#Convert rows to columns using pivot_wider
rankings_wide <- rankings %>%
  pivot_wider(
    names_from = aspect,    # Column to use for creating new column names
    values_from = ranking   # Column to use for populating values in the new columns
  )

head(rankings_wide)


#Merge datasets by 'student_id'
merged_df <- merge(rankings_wide, demos, by = "student_id", all.x = TRUE)

# Rename columns
merged_df <- merged_df %>%
  rename(
    ranking_empathy = "Your child's empathy for others",
    ranking_interpersonal ="Your child's interpersonal skills",
    ranking_leadership = "Your child's leadership and initiative",
    ranking_literacy = "Your child's literacy skills",
    ranking_math = "Your child's mathematical skills",
    ranking_perseverance = "Your child's perseverance and growth mindset",
    ranking_science = "Your child's scientific literacy",
    ranking_selfaware = "Your child's emotional self-awareness and regulation",
    ranking_teamwork = "Your child's collaboration and teamwork skills"
  )



# Define the function to categorize educational levels
categorize_education <- function(edu) {
  if (is.na(edu)) {
    return("unknown")  # Handle missing values
  } else if (edu %in% c("Bachelors", "Graduate degree (Post bachelors)", "Doctorate (PhD)")) {
    return("high")  # High educational attainment
  } else if (edu %in% c("Upper Secondary (Class XII)", "Secondary (Class X)", "Middle (Class VIII)", "Primary (Class V)")) {
    return("low")  # Low educational attainment
  } else if (edu == "None") {
    return("none")  # No formal education
  } else {
    return("unknown")  # Catch unexpected values
  }
}


merged_df <- merged_df %>%
  mutate(
    father_edu_category = sapply(father_education, categorize_education),
    mother_edu_category = sapply(mother_education, categorize_education)
  )

#rankings are numeric 
merged_df$ranking_empathy <- as.numeric(merged_df$ranking_empathy)
merged_df$ranking_interpersonal <- as.numeric(merged_df$ranking_interpersonal)
merged_df$ranking_leadership <- as.numeric(merged_df$ranking_leadership)
merged_df$ranking_literacy <- as.numeric(merged_df$ranking_literacy)
merged_df$ranking_math <- as.numeric(merged_df$ranking_math)
merged_df$ranking_perseverance <- as.numeric(merged_df$ranking_perseverance)
merged_df$ranking_science <- as.numeric(merged_df$ranking_science)
merged_df$ranking_selfaware <- as.numeric(merged_df$ranking_selfaware)
merged_df$ranking_teamwork <- as.numeric(merged_df$ranking_teamwork)

#------------------------------------------------------------------------------------------------------------
#REGRESSIONS Each Educational Attainment 
# List of rankings to iterate over
rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")



# Initialize a list 
regression_results_edu <- list()

#Regressions for each ranking using education categories
for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ father_education + mother_education"))
  model <- feols(formula, data = merged_df, vcov = "hetero")  
  regression_results_edu[[rank]] <- tidy(model)
}


combined_regression_results_edu <- bind_rows(regression_results_edu, .id = "Ranking")


print(combined_regression_results_edu)


xtable_edu_results <- xtable(combined_regression_results_edu)

print(xtable_edu_results, 
      file = "regression_results_edu.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")

#-------------------------------------------------------------------------------------------
#REGRESSIONS On High and Low Educational bins
#List of rankings to iterate over
rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")




#Initialize a list to store regression results
regression_results_category <- list()


for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ father_edu_category + mother_edu_category"))
  model <- feols(formula, data = merged_df, vcov = "hetero")  
  regression_results_category[[rank]] <- tidy(model)
}


combined_regression_results_category <- bind_rows(regression_results_category, .id = "Ranking")


print(combined_regression_results_category)


xtable_category_results <- xtable(combined_regression_results_category)

print(xtable_category_results, 
      file = "regression_results_category.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")

#-------------------------------------------------------------------------------------------
#regression on Occupation 
# List of rankings to iterate over
rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")





regression_results_occupation <- list()


for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ occupation_mother + occupation_father"))
  model <- feols(formula, data = merged_df, vcov = "hetero")  
  regression_results_occupation[[rank]] <- tidy(model)
}


combined_regression_results_occupation <- bind_rows(regression_results_occupation, .id = "Ranking")


print(combined_regression_results_occupation)


xtable_occupation_results <- xtable(combined_regression_results_occupation)

print(xtable_occupation_results, 
      file = "regression_results_occupation.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")


# Add significance asterisks to regression results
combined_regression_results_occupation <- combined_regression_results_occupation %>%
  mutate(
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Estimate = paste0(round(estimate, 3), Significance)  
  )
# Save results
xtable_occupation_results <- xtable(combined_regression_results_occupation %>%
                                  select(Ranking, term, Estimate, std.error, statistic, p.value))

print(xtable_occupation_results, 
      file = "regression_results_occupation_with.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")

#-------------------------------------------------------------------------------------------
#regression on Income 
# List of rankings to iterate over
rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")





regression_results_income <- list()

# Regressions for each ranking using education categories
for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ hh_income"))
  model <- feols(formula, data = merged_df, vcov = "hetero") 
  regression_results_income[[rank]] <- tidy(model)
}


combined_regression_results_income <- bind_rows(regression_results_income, .id = "Ranking")


print(combined_regression_results_income)


xtable_income_results <- xtable(combined_regression_results_income)



combined_regression_results_income <- combined_regression_results_income %>%
  mutate(
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Estimate = paste0(round(estimate, 3), Significance)  
  )
# Save results 
xtable_income_results <- xtable(combined_regression_results_income %>%
                           select(Ranking, term, Estimate, std.error, statistic, p.value))

print(xtable_income_results, 
      file = "regression_results_income_with.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")



#---------------------------------------------------------------------------------------
#regression on child gender  
# List of rankings to iterate over
rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")




# Initialize a list 
regression_results_gender <- list()

# Regressions 
for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ gender_child"))
  model <- feols(formula, data = merged_df, vcov = "hetero")  
  regression_results_gender[[rank]] <- tidy(model)
}


combined_regression_results_gender <- bind_rows(regression_results_gender, .id = "Ranking")

# Print 
print(combined_regression_results_gender)

# Save combined results 
xtable_gender_results <- xtable(combined_regression_results_gender)

print(xtable_gender_results, 
      file = "regression_results_gender.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")


combined_regression_results_gender <- combined_regression_results_gender %>%
  mutate(
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Estimate = paste0(round(estimate, 3), Significance)  
  )
# Save results 
xtable_gender_results <- xtable(combined_regression_results_gender %>%
                                  select(Ranking, term, Estimate, std.error, statistic, p.value))

print(xtable_gender_results, 
      file = "regression_results_gender_with.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")

#---------------------------------------------------------------------------------------------------

rankings <- c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", 
              "ranking_literacy", "ranking_math", "ranking_perseverance", 
              "ranking_science", "ranking_selfaware", "ranking_teamwork")


regression_results <- list()


for (rank in rankings) {
  formula <- as.formula(paste(rank, "~ father_edu_category + mother_edu_category + hh_income + occupation_mother + occupation_father + gender_child"))
  model <- lm(formula, data = merged_df)  # Linear regression
  regression_results[[rank]] <- tidy(model)  # Save tidy results
}


combined_regression_results <- bind_rows(regression_results, .id = "Ranking")


print(combined_regression_results)



library(xtable)
xtable_results <- xtable(combined_regression_results)


print(xtable_results, 
      file = "regression_results_edu_controlled.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")


combined_regression_results <- combined_regression_results %>%
  mutate(
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Estimate = paste0(round(estimate, 3), Significance)  # Combine coefficient and significance
  )

xtable_results <- xtable(combined_regression_results %>%
                           select(Ranking, term, Estimate, std.error, statistic, p.value))

print(xtable_results, 
      file = "regression_results_with_significance.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")

