#Esha Modi
#December 2024
#Teachers Preferences 

#Loading libraries 
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



#Load Datasets 
teacherpath <- "~/Desktop/RA/teacher"
outpath <- "~/Desktop/RA/teacher/output"

#Add dataframe 
rankings <- fread(file.path(teacherpath, "teacher_rankings.csv"))
demos <- fread(file.path(teacherpath, "teacher_demos.csv"))
parent_rankings <- fread(file.path(teacherpath, "parent_rankings.csv"))
parent_demos <- fread(file.path(teacherpath, "parent_demos.csv"))

#Covert teacher_ranking.csv into a wide format 


# Convert the data to wide format
rankings_wide <- rankings %>%
  pivot_wider(
    id_cols = c(teacher_email, student_id), 
    names_from = dimension,  
    values_from = c(ranking_s, ranking_p, conf_s, conf_p) 
  )


print(rankings_wide)

#MERGE DATAFRAMES 

merged_df <- rankings_wide %>%
  left_join(demos, by = "teacher_email")


print(merged_df)

#DROP TEACHERS WITH LESS THAN 6 RANKINGS 

updated_rankings <- merged_df %>%
  group_by(teacher_email) %>%
  filter(n() >= 6) %>%
  ungroup()


print(updated_rankings)

#CONVERT PARENT RANKING INTO A WIDE DATEFRAME
parent_rankings_wide <- parent_rankings %>%
  pivot_wider(
    names_from = aspect,    
    values_from = ranking   
  )

#FILTER THE parent_demos dataframe to retain only the category rankings 


selected_parent_demos <- parent_demos %>%
  select(student_id, ranking_academic, ranking_social, ranking_emotional)

#MERGE selected_parent_demos and parent_rankings
merged_parent_data <- parent_rankings_wide %>%
  left_join(selected_parent_demos, by = "student_id")


print(merged_parent_data)

#Rename columns
merged_parent_data <- merged_parent_data %>%
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

#MERGE PARENT RANKING WITH TEACHER RANKING DATAFRAME
# Ensure both datasets have `student_id` as the same type
merged_df <- merged_df %>%
  mutate(student_id = as.character(student_id))  

merged_parent_data <- merged_parent_data %>%
  mutate(student_id = as.character(student_id))  


final_df <- merged_df %>%
  left_join(merged_parent_data, by = "student_id")


print(final_df)



column_names <- names(final_df)


print(column_names)

#-----------------------------------------------------


# Define the ranking columns
ranking_s_cols <- c("ranking_s_academic", "ranking_s_emotional", "ranking_s_social")

ranking_p_cols <- c("ranking_p_academic", "ranking_p_emotional", "ranking_p_social")

ranking_cols <- c("ranking_academic.y", "ranking_emotional.y", "ranking_social.y")

# Ensure the columns align
stopifnot(length(ranking_s_cols) == length(ranking_p_cols))
stopifnot(length(ranking_s_cols) == length(ranking_cols))

# Calculate gaps

for (i in seq_along(ranking_s_cols)) {
  p_col <- ranking_p_cols[i]
  s_col <- ranking_s_cols[i]
  col <- ranking_cols[i]
  
  final_df <- final_df %>%
    mutate(
      !!paste0("gap_beliefs_parents_", col) := .data[[p_col]] - .data[[col]],
      !!paste0("gap_preferences_parents_", col) := .data[[s_col]] - .data[[col]],
      !!paste0("gap_beliefs_preferences_", col) := .data[[p_col]] - .data[[s_col]]
    )
}
  
  # Calculate category-specific gaps
  final_df <- final_df %>%
    mutate(
      gap_beliefs_parents_academic = ranking_p_academic - ranking_academic.y,
      gap_beliefs_parents_emotional = ranking_p_emotional - ranking_emotional.y,
      gap_beliefs_parents_social = ranking_p_social - ranking_social.y,
      
      gap_preferences_parents_academic = ranking_s_academic - ranking_academic.y,
      gap_preferences_parents_emotional = ranking_s_emotional - ranking_emotional.y,
      gap_preferences_parents_social = ranking_s_social - ranking_social.y,
      
      gap_beliefs_preferences_academic = ranking_p_academic - ranking_s_academic,
      gap_beliefs_preferences_emotional = ranking_p_emotional - ranking_s_emotional,
      gap_beliefs_preferences_social = ranking_p_social - ranking_s_social
    )
  

  # Aggregate gaps by category and teacher
  average_gaps_by_category <- final_df %>%
    group_by(teacher_email) %>%
    summarise(
      avg_gap_beliefs_parents_academic = mean(gap_beliefs_parents_academic, na.rm = TRUE),
      avg_gap_beliefs_parents_emotional = mean(gap_beliefs_parents_emotional, na.rm = TRUE),
      avg_gap_beliefs_parents_social = mean(gap_beliefs_parents_social, na.rm = TRUE),
      
      avg_gap_preferences_parents_academic = mean(gap_preferences_parents_academic, na.rm = TRUE),
      avg_gap_preferences_parents_emotional = mean(gap_preferences_parents_emotional, na.rm = TRUE),
      avg_gap_preferences_parents_social = mean(gap_preferences_parents_social, na.rm = TRUE),
      
      avg_gap_beliefs_preferences_academic = mean(gap_beliefs_preferences_academic, na.rm = TRUE),
      avg_gap_beliefs_preferences_emotional = mean(gap_beliefs_preferences_emotional, na.rm = TRUE),
      avg_gap_beliefs_preferences_social = mean(gap_beliefs_preferences_social, na.rm = TRUE),
    
      gender = first(gender),
      age = first(2024 - yearborn),
      education = first(education),
      years_worked_school = first(years_worked_school),
      years_worked = first(years_worked)
      )
  

  print(average_gaps_by_category)
  


final_df <- final_df %>%
  left_join(average_gaps_by_category, by = "teacher_email")


print(head(final_df))








#-------------------------------------------------------------------------------------------------------------------
# Add a column for age based on the yearborn
final_df <- final_df %>%
  mutate(age = 2024 - yearborn)


head(final_df)
#---------------------------------------------------------------------------------------------


# Define gap variables to iterate over
gap_variables <- c(
  "avg_gap_beliefs_parents_academic", "avg_gap_beliefs_parents_emotional", "avg_gap_beliefs_parents_social",
  "avg_gap_preferences_parents_academic", "avg_gap_preferences_parents_emotional", "avg_gap_preferences_parents_social",
  "avg_gap_beliefs_preferences_academic", "avg_gap_beliefs_preferences_emotional", "avg_gap_beliefs_preferences_social"
)

# Initialize a list to store regression results
regression_results_category <- list()


for (gap in gap_variables) {
  
  formula <- as.formula(paste(gap, "~ gender + age + education + years_worked_school + years_worked"))
  
 
  model <- lm(formula, data = average_gaps_by_category)
  
 
  regression_results_category[[gap]] <- tidy(model)
}


combined_regression_results_category <- bind_rows(regression_results_category, .id = "Gap_Variable")


print(combined_regression_results_category)


combined_regression_results_category <- combined_regression_results_category %>%
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

xtable_results_gaps <- xtable(combined_regression_results_category %>%
                                select(Gap_Variable, term, Estimate, std.error, statistic, p.value))

print(xtable_results_gaps, 
      file = "regression_results_gaps_by_category.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")
