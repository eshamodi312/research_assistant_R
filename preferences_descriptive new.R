#Descriptive stats for Parent Preferences
#Code by: Tushar Kundu
#Updated on 06/10/23


#&&&&&&&&&&&&&&&&&&
#SETUP ----
#&&&&&&&&&&&&&&&&&&

rm(list = ls())

library(ggplot2)
library(data.table)
library(stringr)
library(readxl)
library(gt)
library(fixest)
library(MASS)
library(dplyr)
library(data.table)

#Identifies the data we are using
tag <- "dpsn"

asp9 <- c("Mathematical skills", "Literacy skills", "Scientific literacy",
          "Emotional self-awareness and regulation", "Perseverance and growth mindset", "Empathy for others",
          "Interpersonal skills", "Collaboration and teamwork skills", "Leadership and initiative")

# Paths to directories
dictpath <- "~/Desktop/data/grades/dictionaries"
gradepath <- "~/Desktop/data/grades/clean"
inpath <- "~/Desktop/data/clean"
outpath <- "~/Desktop/data/output"


#Parameters
num_asp <- 18
num_triple <- 6
num_ratings <- 18

preferences_dict <- c(rich = "HH income \\textgreater 15 Lakh INR (20K USD)",
                      gender_parentMother = "Mother responding",
                      gender_parentFather = "Father responding",
                      gender_parentBothparentsrespondingtogether = "Both parents responding",
                      gender_childFemale = "Daughter",
                      age_child = "Age of child",
                      aspiration_jobDoctor = "Job Aspiration: Doctor",
                      aspiration_jobLawyer = "Job Aspiration: Lawyer",
                      `aspiration_jobMechanical(engineer,machineoperator,mechanic)` = "Job Aspiration: Engineer",
                      aspiration_jobOther = "Job Aspiration: Other",
                      aspiration_jobScientist = "Job Aspiration: Scientist",
                      aspiration_jobTeacher = "Job Aspiration: Teacher",
                      aspiration_jobOfficeclerk = "Job Aspiration: Office clerk",
                      aspiration_edu_coarseLessthanbachelors = "Educational Aspiration: Less than Bachelor's",
                      aspiration_edu_coarseBachelors = "Educational Aspiration: Bachelor's",
                      `aspiration_edu_coarseDoctorate(PhD)` = "Educational Aspiration: PhD",
                      ranking_academic = "Academic",
                      ranking_emotional = "Emotional",
                      ranking_social = "Social",
                      ranking_math = "Math",
                      ranking_selfaware = "Self-Aware",
                      ranking_interpersonal = "Interpersonal",
                      `-ranking_academic` = "Academic",
                      `-ranking_emotional` = "Emotional",
                      `-ranking_social` = "Social",
                      `-ranking_math` = "Math",
                      `-ranking_selfaware` = "Self-Aware",
                      `-ranking_interpersonal` = "Interpersonal")

remove_se_lines = function(x) {
  start_pattern = "\\\\midrule"
  # Assuming we stop filtering at the end of a tabular section if there's a specific end indicator
  end_pattern = "\\\\end\\{tabular\\}"
  # Adjusted pattern to detect lines that start with whitespace, followed by an `&`, and then have multiple instances of numbers in brackets or parentheses
  se_pattern = "^\\s*&.*\\([0-9,.]+\\)"
  
  in_tabular_section = FALSE
  lines_to_keep = character()
  
  for (line in x) {
    if (grepl(start_pattern, line)) {
      in_tabular_section = TRUE
      lines_to_keep = c(lines_to_keep, line)
    } else if (grepl(end_pattern, line)) {
      in_tabular_section = FALSE
      lines_to_keep = c(lines_to_keep, line)
    } else if (in_tabular_section && !grepl(se_pattern, line)) {
      # Within the tabular section, keep lines that do not match the SE pattern
      lines_to_keep = c(lines_to_keep, line)
    } else if (!in_tabular_section) {
      # Outside of tabular sections, all lines are kept
      lines_to_keep = c(lines_to_keep, line)
    }
    # Note: Lines matching se_pattern within tabular sections are implicitly skipped
  }
  return(lines_to_keep)
}


#&&&&&&&&&&&&&&&&&&
#DATA----
#&&&&&&&&&&&&&&&&&&

#Survey data
ratings <- fread(file.path(inpath, tag, "ratings.csv"))
rankings <- fread(file.path(inpath, tag, "rankings.csv"))
choices <- fread(file.path(inpath, tag, "choices.csv"))
cqs <- fread(file.path(inpath, tag, "cqs.csv"))
demos <- fread(file.path(inpath, tag, "demos.csv"))

grades <- fread(file.path(gradepath, "dpsn.csv"))



ratings_wide <- dcast(ratings[aspect %in% c("Your child's mathematical skills", "Your child's perseverance and growth mindset")],
                      student_id ~ aspect, value.var = "rating")
names(ratings_wide) <- c("student_id", "math_rating", "perseverance_rating")


rankings_wide <- dcast(rankings[aspect %in% c("Your child's empathy for others", "Your child's interpersonal skills", "Your child's leadership and initiative", "Your child's literacy skills", "Your child's mathematical skills", "Your child's perseverance and growth mindset", "Your child's scientific literacy", "Your child's emotional self-awareness and regulation", "Your child's collaboration and teamwork skills")],
                       student_id ~ aspect, value.var = "ranking")
names(rankings_wide) <- c("student_id", "ranking_empathy", "ranking_interpersonal", "ranking_leadership", "ranking_literacy skills", "ranking_math", "ranking_perseverance", "ranking_science", "ranking_selfaware", "ranking_teamwork")


#&&&&&&&&&&&&&&&&&&
#Constructing new demos 
#&&&&&&&&&&&&&&&&&&



# Convert to data.table
demos <- as.data.table(demos)

# Remove columns B and D
demos[, c("ranking_empathy", "ranking_interpersonal", "ranking_leadership", "ranking_literacy", "ranking_math", "ranking_perseverance", "ranking_science", "ranking_selfaware", "ranking_teamwork"):= NULL]

print(demos)



#&&&&&&&&&&&&&&&&&&
#ANALYSIS ----
#&&&&&&&&&&&&&&&&&&

setnames(grades, "id", "student_id")
grades[, student_id := as.character(student_id)]


#REGRESSING RANKS ON LEVELS ----
#Add on the grades for math
grades[, math_av := mean(grade_numeric[aspect == "Mathematical skills"], na.rm = T), student_id]
grades[, perseverance_av := mean(grade_numeric[aspect == "Perseverance and growth mindset"], na.rm = T), student_id]
av_grades <- unique(grades[,.(student_id, math_av, perseverance_av)])



ratings[, rating_mean := mean(rating, na.rm = T), student_id]
ratings_wide <- merge(ratings_wide, ratings[,.(student_id, rating_mean)] %>% unique, "student_id")
ratings_wide[, math_demean := math_rating - rating_mean]
ratings_wide[, perseverance_demean := perseverance_rating - rating_mean]


#Convert the rankings to factors
max_rank <- 9
# Convert all columns that start with 'ranking_' into ordered factors
#rankings_wide[, (names(rankings_wide)[grep("^ranking_", names(rankings_wide))]) := lapply(.SD, function(x) factor(x, levels = max_rank:1, ordered = TRUE)), .SDcols = patterns("^ranking_")]

demos[, student_id := as.numeric(student_id)]
av_grades[, student_id := as.numeric(student_id)]
ratings_wide[, student_id := as.numeric(student_id)]
rankings_wide[, student_id := as.numeric(student_id)]
rankings[, student_id := as.numeric(student_id)]


#Merge on various grades and see the relationship with ratings and with rankings
demos <- merge(demos, rankings_wide, by = "student_id", all=TRUE)
demos_av_grades <- merge(demos, av_grades, by = "student_id")
demos_av_grades <- merge(demos_av_grades, ratings_wide, "student_id")

demos_av_grades[, student_id := as.numeric(student_id)]


run_regressions <- function(aspect_name, ratings_name, ranking_name,
                            grades, ratings, demos) {
  # Create variable names
  aspect_av <- paste0(tolower(gsub("\\s+", "_", aspect_name)), "_av") %>% make.names
  aspect_rating <- paste0(tolower(gsub("\\s+", "_", aspect_name)), "_rating") %>% make.names
  aspect_demean <- paste0(tolower(gsub("\\s+", "_", aspect_name)), "_demean") %>% make.names
  
  # Add on the grades
  aspect_filter <- aspect_name
  grades[, (aspect_av) := mean(grade_numeric[aspect == aspect_filter], na.rm = TRUE), by = "student_id"]
  
  #Average grades
  av_grades <- unique(grades[, .(student_id, get(aspect_av))])
  setnames(av_grades, c("student_id", aspect_av))
  
  print("av_grades")
  print(head(av_grades)) 
  
  # Adjust the aspect filter for ratings_wide calculations
  aspect_filter <- paste("Your child's", aspect_name)
  
  # Add on the grades
  aspect_filter <- aspect_name
  grades[, (aspect_av) := mean(grade_numeric[aspect == aspect_filter], na.rm = TRUE), by = "student_id"]
  
  #Average grades
  av_grades <- unique(grades[, .(student_id, get(aspect_av))])
  setnames(av_grades, c("student_id", aspect_av))
  
  # Adjust the aspect filter for ratings_wide calculations
  aspect_filter <- paste("Your child's", aspect_name)
  ratings_wide <- dcast(ratings[aspect == ratings_name],
                        student_id ~ aspect, value.var = "rating")
  ratings_wide <- setnames(ratings_wide, c("student_id", aspect_rating))
  
  # Calculate the mean rating per student_id
  ratings[, rating_mean := mean(rating, na.rm = TRUE), by = "student_id"]
  ratings_wide <- merge(ratings_wide, ratings[, .(student_id, rating_mean)] %>% unique(), by = "student_id")
  
  # Calculate demeaned ratings
  ratings_wide[, (aspect_demean) := get(aspect_rating) - rating_mean]
  
  # Merge rankings and ratings to grades
  demos_av_grades <- merge(demos_av_grades, av_grades, by = "student_id")
  demos_av_grades <- merge(demos_av_grades, ratings_wide, by = "student_id")
  demos_ratings <- merge(demos, ratings_wide, by="student_id")
  
  # Run the regressions
  formula1 <- as.formula(paste("as.numeric(", ranking_name, ") ~ ", aspect_av))
  formula2 <- as.formula(paste("as.numeric(", ranking_name, ") ~ ", aspect_rating))
  formula3 <- as.formula(paste("as.numeric(", ranking_name, ") ~ ", aspect_demean))
  
  # Collect and return regression results
  list(
    feols(formula1, demos_av_grades, vcov = "hetero"),
    feols(formula2, demos_ratings, vcov = "hetero"),
    feols(formula3, demos_ratings, vcov = "hetero")
  )
}



str(rankings)
str(demos)
str(grades)
str(demos_av_grades)
str(rankings_wide)
str(ratings_wide)


regs_math <- run_regressions("Mathematical skills", "Your child's mathematical skills", "ranking_math",
                               grades, ratings, demos)
regs_science <- run_regressions("Scientific literacy", "Your child's scientific literacy", "ranking_science",
                             grades, ratings, demos)
regs_literacy <- run_regressions("Literacy skills", "Your child's literacy skills", "ranking_literacy",
                             grades, ratings, demos)

regs_perseverance <- run_regressions("Perseverance and growth mindset", "Your child's perseverance and growth mindset", "ranking_perseverance",
                               grades, ratings, demos)
regs_selfaware <- run_regressions(aspect_name = "Emotional self-awareness and regulation", ratings_name =  "Your child's emotional self-awareness and regulation", ranking_name = "ranking_selfaware",
                               grades, ratings, demos)
regs_empathy <- run_regressions(aspect_name = "Empathy for others", ratings_name =  "Your child's empathy for others", ranking_name = "ranking_empathy",
                                  grades, ratings, demos)

regs_collaboration <- run_regressions(aspect_name = "Collaboration and teamwork skills", ratings_name =  "Your child's collaboration and teamwork skills", ranking_name = "ranking_teamwork",
                               grades, ratings, demos)
regs_leadership <- run_regressions(aspect_name = "Leadership and initiative", ratings_name =  "Your child's leadership and initiative", ranking_name = "ranking_leadership",
                               grades, ratings, demos)
regs_interpersonal <- run_regressions(aspect_name = "Interpersonal skills", ratings_name =  "Your child's interpersonal skills", ranking_name = "ranking_interpersonal",
                                   grades, ratings, demos)


#Extract the second regression from each of these
asp_rank_regressions <- lapply(list(regs_math, regs_science, regs_literacy,
                                    regs_perseverance, regs_selfaware, regs_empathy,
                                    regs_collaboration, regs_leadership, regs_interpersonal),
                               `[[`, 2)
etable(asp_rank_regressions)
aspdemean_rank_regressions <- lapply(list(regs_math, regs_science, regs_literacy,
                                    regs_perseverance, regs_selfaware, regs_empathy,
                                    regs_collaboration, regs_leadership, regs_interpersonal),
                               `[[`, 3)
etable(aspdemean_rank_regressions)




#REGRESSING RANKS ON DEMOGRAPHICS ----

#Demographic variables
demos[aspiration_job == "Office clerk", aspiration_job := "Other"] #For now, since there's only one person who said this
demos[, aspiration_job := factor(aspiration_job, levels = c("Business owner/Manager", "Lawyer", "Scientist", "Doctor", "Teacher", "Office clerk", "Salesperson",
                                                            "Service (cook, sweeper, salon worker, etc.)", "Agricultural (farmer, field worker)", "Craftsmen (tailor, weaver, carpenter)", "Mechanical (engineer, machine operator, mechanic)", "Other")) %>%
        relevel(ref = "Business owner/Manager")]

demos[, aspiration_edu := factor(aspiration_edu, levels = c("Primary (Class V)", "Middle (Class VIII)", "Secondary (Class X)", "Upper Secondary (Class XII)", "Bachelors", "Graduate degree (Post bachelors)", "Doctorate (PhD)")) %>%
        relevel(ref = "Graduate degree (Post bachelors)")]
demos[, aspiration_edu_coarse := factor(aspiration_edu, levels = levels(aspiration_edu),
                                        labels = c(rep("Less than bachelors", 4), "Bachelors", "Graduate degree (Post bachelors)", "Doctorate (PhD)")) %>%
        relevel(ref = "Graduate degree (Post bachelors)")]

demos[, hh_income := factor(hh_income, levels = c("Rs 0 - Rs 2.5 lakh",
                                                  "Rs 2.5 - Rs 3.0 lakh",
                                                  "Rs 3.0 - Rs 5.0 lakh",
                                                  "Rs 5.0 - Rs 7.5 lakh",
                                                  "Rs 7.5 - Rs 10.0 lakh",
                                                  "Rs 10.0 - Rs 12.5 lakh",
                                                  "Rs 12.5 - Rs 15.0 lakh",
                                                  "Over 15 lakh")) %>%
        relevel(ref = "Over 15 lakh")]

demos[, gender_child := factor(gender_child, levels = c("Male", "Female")) %>%
        relevel(ref = "Male")]

demos[, gender_parent := factor(gender, levels = c("Father", "Mother", "Both parents responding together", "Other"),
                                labels = c("Father", "Mother", rep("Both parents responding together", 2))) %>%
        relevel(ref = "Father")]

demos[, rich := ifelse(hh_income == "Over 15 lakh", 1, 0)] #Rich ($20,000 a year or more) is around 40% of this baseline sample
demos[, age_child := as.numeric(age_child)]


#Regressions

rank_demo_reg <- function(rank_var){
  formula_temp <- as.formula(paste0("-", rank_var, " ~ gender_child + gender_parent + age_child + rich + aspiration_job + aspiration_edu_coarse"))
  reg_temp <- feols(formula_temp, demos)
  return(reg_temp)
}

rank_list <- c(Academic = "ranking_academic", Emotional = "ranking_emotional", Social = "ranking_social",
               Math = "ranking_math", `Self-Awareness` = "ranking_selfaware", Interpersonal = "ranking_interpersonal")

rank_demo_regs <- lapply(rank_list, rank_demo_reg)


#INCOME HIGHLIGHT
etable(rank_demo_regs, dict = preferences_dict, 
       tex = T,
       headers = list(`^Rank Type` = list("Category (1-3)"=3, "Aspect (1-9)"=3)),
       drop = c("Constant", "Less than Bachelor's", "Other", "Age of child"),
       notes = c("Ranks multiplied by -1 so that a positive (negative) coefficient means more (less) important.",
                 "Reference groups: Business owner; Graduate degree; Son; Father responding."),
       vcov = "hetero",
       highlight = .("thick5, sep2, cornflowerblue!90" = "HH income"),
       digits = 2, digits.stats = 2,
       fitstat = c("n", "r2"),
       replace = T,
       style.tex = style.tex("aer",  fontsize = "small"), arraystretch = 1.5) %>%
  remove_se_lines() %>%
  writeLines(con = file.path(outpath, "tables/rank_demo_reg_INCOME.tex"))


#CHILD GENDER HIGHLIGHT
etable(rank_demo_regs, dict = preferences_dict, 
       tex = T,
       headers = list(`^Rank Type` = list("Category (1-3)"=3, "Aspect (1-9)"=3)),
       drop = c("Constant", "Less than Bachelor's", "Other", "Age of child"),
       notes = c("Ranks multiplied by -1 so that a positive (negative) coefficient means more (less) important.",
                 "Reference groups: Business owner; Graduate degree; Son; Father responding."),
       vcov = "hetero", 
       highlight = .("thick5, sep2, cornflowerblue!90" = "Daughter"),
       digits = 2, digits.stats = 2,
       fitstat = c("n", "r2"),
       replace = T,
       style.tex = style.tex("aer",  fontsize = "small"), arraystretch = 1.5) %>%
  remove_se_lines() %>%
  writeLines(con = file.path(outpath, "tables/rank_demo_reg_CHILDGENDER.tex"))

#PARENTS GENDER HIGHLIGHT
etable(rank_demo_regs, dict = preferences_dict, 
       tex = T,
       headers = list(`^Rank Type` = list("Category (1-3)"=3, "Aspect (1-9)"=3)),
       drop = c("Constant", "Less than Bachelor's", "Other", "Age of child"),
       notes = c("Ranks multiplied by -1 so that a positive (negative) coefficient means more (less) important.",
                 "Reference groups: Business owner; Graduate degree; Son; Father responding."),
       vcov = "hetero",
       highlight = .("thick5, sep2, cornflowerblue!90" = c("Mother@1; Both@6")),
       digits = 2, digits.stats = 2,
       fitstat = c("n", "r2"),
       replace = T,
       style.tex = style.tex("aer",  fontsize = "small"), arraystretch = 1.5) %>%
  remove_se_lines() %>%
  writeLines(con = file.path(outpath, "tables/rank_demo_reg_PARENTGENDER.tex"))

#MATH HIGHLIGHT
etable(rank_demo_regs, dict = preferences_dict, 
       tex = T,
       headers = list(`^Rank Type` = list("Category (1-3)"=3, "Aspect (1-9)"=3)),
       drop = c("Constant", "Less than Bachelor's", "Other", "Age of child"),
       notes = c("Ranks multiplied by -1 so that a positive (negative) coefficient means more (less) important.",
                 "Reference groups: Business owner; Graduate degree; Son; Father responding."),
       vcov = "hetero",
       highlight = .("thick5, sep2, cornflowerblue!90" = c("Daughter@4; PhD@4")),
       digits = 2, digits.stats = 2,
       fitstat = c("n", "r2"),
       replace = T,
       style.tex = style.tex("aer",  fontsize = "small"), arraystretch = 1.5) %>%
  remove_se_lines() %>%
  writeLines(con = file.path(outpath, "tables/rank_demo_reg_MATH.tex"))



#Phd-Interpersonal HIGHLIGHT
etable(rank_demo_regs, dict = preferences_dict, 
       tex = T,
       headers = list(`^Rank Type` = list("Category (1-3)"=3, "Aspect (1-9)"=3)),
       drop = c("Constant", "Less than Bachelor's", "Other", "Age of child"),
       notes = c("Ranks multiplied by -1 so that a positive (negative) coefficient means more (less) important.",
                 "Reference groups: Business owner; Graduate degree; Son; Father responding."),
       vcov = "hetero",
       highlight = .("thick5, sep2, cornflowerblue!90" = c("PhD@6; PhD@6")),
       digits = 2, digits.stats = 2,
       fitstat = c("n", "r2"),
       replace = T,
       style.tex = style.tex("aer",  fontsize = "small"), arraystretch = 1.5) %>%
  remove_se_lines() %>%
  writeLines(con = file.path(outpath, "tables/rank_demo_reg_PHD.tex"))





rank_demo_reg("ranking_teamwork")





#Plots ----


selected_colors <- c("#8ec73f", "#ff8c69", "#3fa9f5")

category_ranks_long <- demos[,.(student_id, school, ranking_academic, ranking_emotional, ranking_social)] %>%
  melt(id.vars = c("student_id", "school"), variable.name = "dimension", value.name = "rank")
category_ranks_long[, dimension := str_remove_all(dimension, "ranking_") %>% str_to_title]

n_parents <- na.omit(category_ranks_long)$student_id %>% uniqueN

#Histogram of ranks for the categories
category_rank_plot <- ggplot(category_ranks_long) +
  geom_histogram(aes(x = rank, y = ..density.., fill = dimension), binwidth = 1, alpha = 0.5) +
  facet_wrap(~ dimension, scales = "fixed") +
  scale_x_continuous(breaks = 1:3) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  labs(x = "Rank", y = "Density") +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = "black", size = 0.5),
        strip.text = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 1))) + 
  ggtitle(label = "Parents rank of skill categories",
          subtitle = paste0("N = ", n_parents)) + 
  coord_cartesian(ylim = c(0,.5))

ggsave(file.path(outpath, "graphs/ranks/category_rank.pdf"), category_rank_plot, width = 7, height = 5)

#Same plot for aspects
aspect_ranks_long <- demos[,.(student_id, school, ranking_math, ranking_literacy, ranking_science,
                              ranking_selfaware, ranking_perseverance, ranking_empathy,
                              ranking_interpersonal, ranking_teamwork, ranking_leadership)] %>%
  melt(id.vars = c("student_id", "school"), variable.name = "dimension", value.name = "rank")
aspect_ranks_long[, dimension := str_remove_all(dimension, "ranking_") %>% str_to_title %>%
                    factor(levels = c("Math", "Literacy", "Science",
                                      "Selfaware", "Perseverance", "Empathy",
                                      "Interpersonal", "Teamwork", "Leadership"))]

aspect_ranks_long[, category := factor(dimension, levels = levels(dimension),
                                       labels = c(rep("Academic", 3),
                                                  rep("Emotional", 3),
                                                  rep("Social", 3)))]
aspect_rank_plot <- ggplot(aspect_ranks_long) +
  geom_histogram(aes(x = rank, y = ..density.., fill = category), binwidth = 1, alpha = 0.5) +
  facet_wrap(~ dimension, scales = "fixed") +
  scale_x_continuous(breaks = 1:9) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  labs(x = "Rank", y = "Density") +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = "black", size = 0.5),
        strip.text = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 1))) + 
  ggtitle(label = "Parents rank of skill categories",
          subtitle = paste0("N = ", n_parents)) + 
  coord_cartesian(ylim = c(0,.2))

ggsave(file.path(outpath, "graphs/ranks/aspect_rank.pdf"), aspect_rank_plot, width = 8, height = 5)



#PCA----


#*PCA for grades ----
grades[, grade_aspect := mean(grade_numeric, na.rm = T), .(id, aspect)]

grades_aspect <- unique(grades[aspect != "",.(id, grade_aspect, aspect)])
grades_aspect[, n := .N, .(id, aspect)]

grades_aspect[, aspect := factor(aspect, levels = asp9)]

grades_wanted <- c("Mathematical skills", "Literacy skills", "Scientific literacy",
                   "Emotional self-awareness and regulation", "Perseverance and growth mindset",
                   "Collaboration and teamwork skills")

grades_wide <- dcast(grades_aspect[aspect %in% grades_wanted], id ~ aspect, value.var = "grade_aspect")

grades_cov <- cov(grades_wide[,-"id"], use = "pairwise.complete.obs")
e <- eigen(grades_cov)

e$values/sum(e$values)


#*PCA for ratings ----
asp9_long <- paste0("Your child's ", tolower(asp9))
ratings[, aspect := factor(aspect, levels = asp9_long)]
ratings_wide <- dcast(ratings[aspect %in% asp9_long & id <= 9],
                      student_id ~ aspect, value.var = "rating") %>% na.omit
ratings_cov <- cov(ratings_wide[,-"student_id"])
ratings_cor <- cor(ratings_wide[,-"student_id"])

e_ratings <- eigen(ratings_cov)
e_ratings$vectors
#Are there any dimensions that just have a low loading across all PCs?
apply(e_ratings$vectors, 1, function(x) mean(abs(x)))

e_ratings$values/sum(e_ratings$values)

e_ratings_cor <- eigen(ratings_cor)

#Are there any dimensions that just have a low loading across all PCs?
apply(e_ratings_cor$vectors[,1:4], 1, function(x) mean(abs(x)))

x <- data.table(aspect = asp9, e_ratings$vectors)



#*PCA for preferences ----

aspect_ranks_wide <- unique(aspect_ranks_long[,.(student_id, dimension, rank)]) %>% na.omit
aspect_ranks_wide[, n := .N, .(student_id, dimension)]
aspect_ranks_wide <- aspect_ranks_wide[n == 1]
aspect_ranks_wide <- dcast(aspect_ranks_wide, student_id ~ dimension, value.var = "rank") %>% na.omit

ranks_cov <- cov(aspect_ranks_wide[,-"student_id"])
ranks_cor <- cor(aspect_ranks_wide[,-"student_id"])

e_ranks <- eigen(ranks_cov)
e_ranks$vectors
#Are there any dimensions that just have a low loading across all PCs?
apply(e_ranks$vectors, 1, function(x) mean(abs(x)))

round(e_ranks$values/sum(e_ranks$values), 2)

e_ratings_cor <- eigen(ratings_cor)

#Are there any dimensions that just have a low loading across all PCs?
apply(e_ratings_cor$vectors[,1:4], 1, function(x) mean(abs(x)))

x <- data.table(aspect = asp9, e_ranks$vectors)




