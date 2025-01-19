#Esha Modi
#28 November 2024
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




teacherpath <- "~/Desktop/RA/teacher"
outpath <- "~/Desktop/RA/teacher/output"

#Add dataframe 
rankings <- fread(file.path(teacherpath, "teacher_rankings.csv"))
demos <- fread(file.path(teacherpath, "teacher_demos.csv"))
parent_rankings <- fread(file.path(teacherpath, "parent_rankings.csv"))
parent_demos <- fread(file.path(teacherpath, "parent_demos.csv"))


# Convert the data to wide format
rankings_wide <- rankings %>%
  pivot_wider(
    id_cols = c(teacher_email, student_id), 
    names_from = dimension,  
    values_from = c(ranking_s, ranking_p, conf_s, conf_p) 
  )

# View the result
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
ranking_s_cols <- c("ranking_s_empathy", "ranking_s_interpersonal", "ranking_s_leadership",
                    "ranking_s_literacy", "ranking_s_math", "ranking_s_perseverance",
                    "ranking_s_science", "ranking_s_selfaware", "ranking_s_teamwork",
                    "ranking_s_academic", "ranking_s_emotional", "ranking_s_social")

ranking_p_cols <- c("ranking_p_empathy", "ranking_p_interpersonal", "ranking_p_leadership",
                    "ranking_p_literacy", "ranking_p_math", "ranking_p_perseverance",
                    "ranking_p_science", "ranking_p_selfaware", "ranking_p_teamwork",
                    "ranking_p_academic", "ranking_p_emotional", "ranking_p_social")

ranking_cols <- c("ranking_empathy.y", "ranking_interpersonal.y", "ranking_leadership.y",
                  "ranking_literacy.y", "ranking_math.y", "ranking_perseverance.y",
                  "ranking_science.y", "ranking_selfaware.y", "ranking_teamwork.y",
                  "ranking_academic.y", "ranking_emotional.y", "ranking_social.y")

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

# Calculate absolute gaps
final_df <- final_df %>%
  mutate(
    across(starts_with("gap_beliefs_parents"), abs, .names = "abs_{col}"),
    across(starts_with("gap_preferences_parents"), abs, .names = "abs_{col}"),
    across(starts_with("gap_beliefs_preferences"), abs, .names = "abs_{col}")
  )

#AVERAGE 

# Calculate averages per teacher per dimension
average_gaps <- final_df %>%
  group_by(teacher_email) %>%
  summarise(
    avg_gap_beliefs_parents = mean(c_across(starts_with("gap_beliefs_parents")), na.rm = TRUE),
    avg_gap_preferences_parents = mean(c_across(starts_with("gap_preferences_parents")), na.rm = TRUE),
    avg_gap_beliefs_preferences = mean(c_across(starts_with("gap_beliefs_preferences")), na.rm = TRUE),
    avg_abs_gap_beliefs_parents = mean(c_across(starts_with("abs_gap_beliefs_parents")), na.rm = TRUE),
    avg_abs_gap_preferences_parents = mean(c_across(starts_with("abs_gap_preferences_parents")), na.rm = TRUE),
    avg_abs_gap_beliefs_preferences = mean(c_across(starts_with("abs_gap_beliefs_preferences")), na.rm = TRUE)
  )

# Merge the calculated averages back into the original dataframe
final_df <- final_df %>%
  left_join(average_gaps, by = "teacher_email")


head(final_df)


# Calculate overall average gaps for each teacher
overall_averages <- final_df %>%
  group_by(teacher_email) %>%
  summarise(
    avg_gap_beliefs_parents = mean(c_across(starts_with("gap_beliefs_parents")), na.rm = TRUE),
    avg_gap_preferences_parents = mean(c_across(starts_with("gap_preferences_parents")), na.rm = TRUE),
    avg_gap_beliefs_preferences = mean(c_across(starts_with("gap_beliefs_preferences")), na.rm = TRUE),
    avg_abs_gap_beliefs_parents = mean(c_across(starts_with("abs_gap_beliefs_parents")), na.rm = TRUE),
    avg_abs_gap_preferences_parents = mean(c_across(starts_with("abs_gap_preferences_parents")), na.rm = TRUE),
    avg_abs_gap_beliefs_preferences = mean(c_across(starts_with("abs_gap_beliefs_preferences")), na.rm = TRUE)
  )

# Merge the overall averages back into final_df
final_df <- final_df %>%
  left_join(overall_averages, by = "teacher_email")


print(head(final_df))


#-------------------------------------------------------------------------------------------------------------------
# Add a column for age based on the yearborn
final_df <- final_df %>%
  mutate(age = 2024 - yearborn)

# View the first few rows to verify
head(final_df)
#---------------------------------------------------------------------------------------------


#REGRESSION 
# Define the gap variables to iterate over
gap_variables <- c(
  "avg_abs_gap_beliefs_parents.y",
  "avg_abs_gap_preferences_parents.y",
  "avg_abs_gap_beliefs_preferences.y"
)

# Initialize a list 
regression_results <- list()

# Perform regressions 
for (gap in gap_variables) {
  
  formula <- as.formula(paste(gap, "~ gender + age + education + years_worked_school + years_worked"))
  
  
  model <- lm(formula, data = final_df)  
  
  
  regression_results[[gap]] <- tidy(model)
}


combined_regression_results <- bind_rows(regression_results, .id = "Gap_Variable")


print(combined_regression_results)


xtable_results_teacher <- xtable(combined_regression_results)


print(xtable_results_teacher, 
      file = "regression_results_teacher_abs_avg_gaps.tex", 
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
    Estimate = paste0(round(estimate, 3), Significance)  
  )


xtable_results_teacher <- xtable(combined_regression_results %>%
                                   select(Gap_Variable, term, Estimate, std.error, statistic, p.value))

print(xtable_results_teacher, 
      file = "regression_results_with_significance_teacher_abs_avg_gaps.tex", 
      include.rownames = FALSE, 
      floating = FALSE, 
      sanitize.text.function = identity, 
      tabular.environment = "longtable",
      size = "small")



