library(tidyverse)
library(afex)
library(emmeans)

# Part 1 - Between Participants ANOVA
# Read Data
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")

head(my_data)

## change Condition to factor <chr> to <fct>
my_data_tidied <- my_data %>%
 mutate(Condition = factor(Condition))
head(my_data_tidied)

# Summarising out Data
my_data_tidied %>%
 group_by(Condition) %>%
 summarise(mean = mean(Ability), sd = sd(Ability))

# Visualize Data
set.seed(1234)
my_data_tidied %>%
 ggplot(aes(x = Condition, y = Ability, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 theme_minimal() +
 theme(text = element_text(size = 13))

# Building ANOVA Model
model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied)

## getting output of the ANOVA
summary(model)

## emmeans() function for telling us what mean(s) differ(s) from what other mean(s)
emmeans(model, pairwise ~ Condition) # This is default comparison - Tukey's method

## Changing to Bonderroni
emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# Part2 - Repeated Measures ANOVA
# Read Data
rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
head(rm_data)

# change Condition to factor <chr> to <fct>
rm_data_tidied <- rm_data %>%
 mutate(Condition = factor(Condition))
head(rm_data_tidied)

# Summarising Data (add mean &sd)
rm_data_tidied %>%
 group_by(Condition) %>%
 summarise(mean = mean(RT), sd = sd(RT))

# Visualising Data
rm_data_tidied %>%
 ggplot(aes(x = fct_reorder(Condition, RT), y = RT, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 xlab("Condition") +
 ylab("RT (s)")

# Building ANOVA Model
rm_model <- aov_4(RT ~ Condition +(1 + Condition | Participant), data = rm_data_tidied)

# Interpreting the Model Output
summary(rm_model)

# anova format
anova(rm_model)

# "Bonferroni" for multiple comparisons
emmeans(rm_model, pairwise ~ Condition, adjust = "Bonferroni")


# Part3 - Factorial ANOVA
# Read Data
factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/factorial_data.csv")
head(factorial_data)

# change 2 expt factors from <chr> to <fct>
factorial_data_tidied <- factorial_data %>%
 mutate(Sentence = factor(Sentence), Context = factor(Context))
head(factorial_data_tidied)

# Summarize Data - 2 groups
factorial_data_tidied %>%
 group_by(Context, Sentence) %>%
 summarise(mean_rt = mean(RT), sd_rt = sd(RT))

# Install and launch library(visdat)
library(visdat)

# Visualize missing data
vis_miss(factorial_data_tidied)

# go back to summarize the data by ignoring missing data
factorial_data_tidied %>%
 group_by(Context, Sentence) %>%
 summarise(mean_rt = mean(RT, na.rm = TRUE), sd_rt = sd(RT, na.rm = TRUE))

# Visualize Data
factorial_data_tidied %>%
 filter(!is.na(RT)) %>%
 ggplot(aes(x = Context:Sentence, y = RT, color = Context:Sentence)) +
 geom_violin() +
 geom_jitter(width = .1, alpha = .25) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 theme_minimal() +
 theme(text = element_text(size = 13)) +
 xlab("Context X Sentence") +
 ylab("RT (ms)")
 
# Building ANOVA Model
# F1 ANOVA - Subject as random factor
### Context * Sentence = Context + Sentence + Context:Sentence
model_subjects <- aov_4(RT ~ Context * Sentence +(1 + Context * Sentence | Subject), data = factorial_data_tidied, na.rm = TRUE)
anova(model_subjects)

# F2 ANOVA - item comparison
model_items <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Item), data = factorial_data_tidied, na.rm = TRUE)
anova(model_items)

# see subject comparison
emmeans(model_subjects, pairwise ~ Context * Sentence, adjust = "none")





# Challenge 1
ch_1 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")
head(ch_1)

# Condition <chr> to <fct>
ch_1_ty <- ch_1 %>%
 mutate(Condition = factor(Condition))
head(ch_1_ty)

# Summarize data
ch_1_ty %>%
 group_by(Condition) %>%
 summarise(mean = mean(RT), sd = sd(RT))

# Visualize 1
ch_1_ty %>%
 ggplot(aes(x = Condition, y = RT, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black")

# build ANOVA M1
M1 <- aov_4(RT ~ Condition + (1 | Subject), data = ch_1_ty)

# Interpret Model Output
summary(M1)

# Comparison
emmeans(M1, pairwise ~ Condition, adjust = "Bonferroni")
### Sig. Diff. in high vs low lexical freq. words, 
### (F(1,22) = 91.217, p <0.001, ges = .81),
### Reaction time in processing low lexical = higher


# Challenge 2
ch_2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")
head(ch_2)

# Condition <chr> to <fct>
ch_2_ty <- ch_2 %>%
 mutate(Condition = factor(Condition))
head(ch_2_ty)

# Group sum
ch_2_ty %>%
 group_by(Condition) %>%
 summarise(mean2 = mean(RT), sd2 = sd(RT))

# Visualize 2
ch_2_ty %>%
 ggplot(aes(x = fct_reorder(Condition, RT), y = RT, color = Condition)) +
 geom_violin() +
 geom_jitter(width = .1) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 xlab("Condition") +
 ylab("RT")
 
# build ANOVA M2
M2 <- aov_4(RT ~ Condition + (1 | Subject), data = ch_2_ty)

# Interpreting the Model Output
summary(M2)

# Comparison
emmeans(M2, pairwise ~ Condition, adjust = "Bonferroni")
### Sig. Diff. from all v.low * v.high lexical freq. words, 
### (F(3,44) = 203.21, p <0.001, ges = .93),
### Reaction time in processing v.low > low > high >v.high lexical 




# Challenge 3 - Repeated Measure
ch_3 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data3.csv")
head(ch_3)

# Size & Colour, <chr> to <fct>
ch_3_ty <- ch_3 %>%
 mutate(Size = factor(Size), Colour = factor(Colour))
head(ch_3_ty)

# Summarize
ch_3_ty %>%
 group_by(Size, Colour) %>%
 summarise(mean3 = mean(RT), sd3 = sd(RT))

# Visualize 3
ch_3_ty %>%
 ggplot(aes(x = Size:Colour, y = RT, color = Size:Colour)) +
 geom_violin() +
 geom_jitter(width = .1, alpha = .25) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 xlab("Size X Color") +
 ylab("RT")

# Build ANOVA M3
M3 <- aov_4(RT ~ Size * Colour + (1 + Size * Colour | Subject), data = ch_3_ty)
anova(M3)

# See relations
emmeans(M3, pairwise ~ Size * Colour, adjust = "none")
## Size eff. F(1,23) = 198.97, p < .001, ges = .623
## Color eff. F(1,23) = 524.27, p < .001, ges = .866
## butinteraction Size * Color F(1,23) = 11.08, p = .002, ges = .137
### React sig. faster when processing color, followed by size 
### Large Color (593), Small Color (957), Large B.W (1228), Small B.W (1420)



# Challenge 4
ch_4 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data4.csv")
head(ch_4)

# <chr> to <fct>
ch_4_ty <- ch_4 %>%
 mutate(Difficulty = factor(Difficulty), Time_Pressure = factor(Time_Pressure), Group = factor(Group))
head(ch_4_ty)

# grouping
ch_4_ty %>%
 group_by(Difficulty, Time_Pressure, Group) %>%
 summarise(mean4 = mean(RT), sd4 = sd(RT))

# Visualize 4
ch_4_ty %>%
 ggplot(aes(x = Difficulty:Time_Pressure:Group, y = RT, color = Group)) +
 geom_violin(alpha = .5) +
 geom_jitter(width = .1, alpha = .25) +
 guides(color = 'none') +
 stat_summary(fun.data = "mean_cl_boot", color = "black") +
 theme(axis.text.x = element_text(angle = 20, size = 9, face = "bold")) +
 xlab("Difficulty X Time_Pressure X Group") +
 ylab("RT")

## Filter by student 3 separate groups

# Psychololgy_Students
ch_4_ty1 <- ch_4_ty %>%
 filter(Group == "Psychology_Students")
# Build ANOVA MPsy
MPsy <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = ch_4_ty1)
anova(MPsy)
# multiple relations
emmeans(MPsy, pairwise ~ Difficulty * Time_Pressure, adjust = "none")
#Report
 sig. diff. in Difficulty - F(1,23) = 524.27, p < .001, ges = .866
 sig. diff. in Time_Pressure - F(1,23) = 198.97, p < .001, ges = .623
 interaction Difficulty:time pressure - F(1,23) = .223, p = .641, ges = .003


# Arts_Students
ch_4_ty2 <- ch_4_ty %>%
 filter(Group == "Arts_Students")
# Build ANOVA MArts
MArts <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = ch_4_ty2)
anova(MArts)
# multiple relations
emmeans(MArts, pairwise ~ Difficulty * Time_Pressure, adjust = "none")
# Report
 Difficulty - F(1,23) = 3.82, p = .063, ges = .045
 Time_Pressure - F(1,23) = .925, p = .346, ges = .008
 interaction Difficulty:time pressure - F(1,23) = 11.08, p = .003, ges = .137



# Maths_Students
ch_4_ty3 <- ch_4_ty %>%
 filter(Group == "Maths_Students")
# Build ANOVA MMaths
MMaths <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = ch_4_ty3)
anova(MMaths)
# multiple relations
emmeans(MMaths, pairwise ~ Difficulty * Time_Pressure, adjust = "none")


