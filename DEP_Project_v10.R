############################################################
# DEP project - Adult census income dataset
# area: Population and Society
# data: UCI Adult dataset (1994 US census), adult.data + adult.test
#
# what i'm trying to find out: what kind of people earn more than 50K
# a year (age, education, job, sex etc) and then get the data cleaned,
# encoded and run PCA on it
#
# adult.data and adult.test need to be in the same folder as this file
# plots go into figures/ and csv files into outputs/
############################################################


#### setup ####

#use setwd() in the folder you have the datasets 

# installs only the packages i dont have yet
pkgs <- c("tidyverse", "gridExtra", "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)   # dplyr, ggplot2, tidyr etc
library(gridExtra)   # for putting plots next to each other
library(scales)      # percent / comma labels on axes

# folders for the plots and tables
dir.create("figures", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# small function so every plot shows up AND gets saved as png
save_plot <- function(plot, name, width = 8, height = 5) {
  print(plot)
  ggsave(filename = file.path("figures", paste0(name, ".png")),
         plot = plot, width = width, height = height, dpi = 300)
}

# same theme for all plots so they match on the poster
theme_set(theme_minimal(base_size = 13) +
            theme(plot.title = element_text(face = "bold")))
income_cols <- c("<=50K" = "#4C78A8", ">50K" = "#F58518")  # blue/orange for income


#### 1. loading the data ####

# files dont have a header so i have to give the column names myself
# (got them from adult.names on the UCI site)
col_names <- c("age", "workclass", "fnlwgt", "education", "education_num",
               "marital_status", "occupation", "relationship", "race", "sex",
               "capital_gain", "capital_loss", "hours_per_week",
               "native_country", "income")

# problems i ran into when reading the files:
# - theres a space after every comma so strip.white = TRUE
# - missing values are "?" not blank so na.strings = "?"
# - first line of adult.test is junk ("|1x3 Cross validator") so skip = 1
# - adult.test labels have a dot at the end ("<=50K.") which gives 4 classes
#   instead of 2, fixing that below
adult_train <- read.csv("adult.data", header = FALSE, col.names = col_names,
                        na.strings = "?", strip.white = TRUE,
                        stringsAsFactors = FALSE)
adult_test  <- read.csv("adult.test", header = FALSE, col.names = col_names,
                        na.strings = "?", strip.white = TRUE,
                        stringsAsFactors = FALSE, skip = 1)

# putting both files together since im not training a model here,
# more rows = better for exploring (32561 + 16281 = 48842)
adult_raw <- bind_rows(adult_train, adult_test) %>%
  mutate(income = str_remove(income, "\\.$"))   # gets rid of the dot
# quick look at the data
dim(adult_raw)        # 48842 rows, 15 cols
glimpse(adult_raw)    # 6 numeric, 9 text columns
summary(adult_raw)
table(adult_raw$income)   # should only be 2 classes now

# little data dictionary - type, unique values and NAs for each column
data_dictionary <- tibble(
  attribute = names(adult_raw),
  type      = sapply(adult_raw, class),
  n_unique  = sapply(adult_raw, n_distinct),
  n_missing = sapply(adult_raw, function(x) sum(is.na(x)))
)
print(data_dictionary)
write_csv(data_dictionary, "outputs/data_dictionary.csv")

#### 2. cleaning ####

## missing values
# how many NAs per column + percent
missing_summary <- adult_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "attribute", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(adult_raw), 2)) %>%
  arrange(desc(n_missing))
print(missing_summary)
# only 3 columns have NAs: occupation 2809, workclass 2799, native_country 857

p_missing <- missing_summary %>%
  filter(n_missing > 0) %>%
  ggplot(aes(x = reorder(attribute, n_missing), y = n_missing)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(pct_missing, "%")), hjust = -0.1) +
  coord_flip() +
  expand_limits(y = 3300) +
  labs(title = "Missing values per attribute", x = NULL, y = "Number of rows")
save_plot(p_missing, "01_missing_values", 7, 3.5)

# are workclass and occupation missing for the same people?
table(workclass_NA = is.na(adult_raw$workclass),
      occupation_NA = is.na(adult_raw$occupation))
# yes - whenever workclass is missing occupation is too (2799 rows)
# the other 10 missing occupations are "Never-worked" people, which makes
# sense, they dont have a job. so its not really random

# rows with at least one NA
sum(!complete.cases(adult_raw))   # 3620, around 7.4%

# checking if the rows with NAs are different on income
adult_raw %>%
  mutate(has_na = !complete.cases(.)) %>%
  group_by(has_na) %>%
  summarise(pct_high_income = mean(income == ">50K"))

# tried mode imputation as well just to compare (not using it in the end)
get_mode <- function(x) names(sort(table(x), decreasing = TRUE))[1]
adult_mode_imputed <- adult_raw %>%
  mutate(across(c(workclass, occupation, native_country),
                ~ replace_na(., get_mode(.))))
colSums(is.na(adult_mode_imputed))   # no NAs left
# problem is this dumps ~2800 people into one job category which messes up
# the occupation counts. only 7% of rows are affected and theres still 45k
# left so i'm just deleting the rows with NAs instead
## duplicates
sum(duplicated(adult_raw))   # 52 duplicate rows

## doing the actual cleaning
adult <- adult_raw %>%
  distinct() %>%                 # drop duplicates
  drop_na() %>%                  # drop rows with NAs
  select(-fnlwgt)                # fnlwgt is just a census weight, it doesnt
                                 # say anything about the person so dropping it

# education and education_num look like the same thing, checking:
table(adult$education, adult$education_num)
# yep each level matches one number. keeping both for now, text one for the
# plots and the number one is basically the label encoded version

dim(adult)   # 45175 rows, 14 cols - still way over 3000 rows / 7 cols
## outliers
num_vars <- c("age", "education_num", "capital_gain", "capital_loss",
              "hours_per_week")

# IQR method - anything below Q1 - 1.5*IQR or above Q3 + 1.5*IQR
iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
}
# z-score method - more than 3 sd away from the mean
z_outliers <- function(x) sum(abs(scale(x)) > 3)

outlier_table <- tibble(
  variable      = num_vars,
  iqr_outliers  = sapply(adult[num_vars], iqr_outliers),
  z_outliers    = sapply(adult[num_vars], z_outliers),
  pct_zero      = sapply(adult[num_vars], function(x) round(100 * mean(x == 0), 1))
)
print(outlier_table)
write_csv(outlier_table, "outputs/outlier_table.csv")
# what this shows:
# - hours_per_week: IQR says ~11900 outliers?? thats because most people work
#   40-45 hrs so the IQR is tiny. z-score makes more sense for this one
# - capital_gain/loss: over 90% are 0 so every non zero value counts as an
#   outlier with IQR. they're real people with investments tho, not errors
# - age: max is 90, which is fine, 90 is the top code in the census

# boxplots of everything on the z scale so they fit in one plot
p_box_all <- adult %>%
  select(all_of(num_vars)) %>%
  mutate(across(everything(), ~ as.numeric(scale(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "z") %>%
  ggplot(aes(x = variable, y = z)) +
  geom_boxplot(fill = "#72B7B2", outlier.alpha = 0.2) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(title = "Outliers in numeric attributes (z-scores, dashed = |z| = 3)",
       x = NULL, y = "z-score")
save_plot(p_box_all, "02_outlier_boxplots", 8, 4)

# weird value: capital_gain = 99999, looks like a cap not a real amount
sum(adult$capital_gain == 99999)                 # 229 people
sort(unique(adult$capital_gain), decreasing = TRUE)[1:5]  # huge jump down after 99999
# hours = 99 is probably the same kind of thing
sum(adult$hours_per_week == 99)
## dealing with the outliers + transformations
# not deleting any of them since they're real values. instead:
# - flag columns for the special cases
# - log(1 + x) on capital gain/loss bc theyre super skewed
# - cap hours at the 1st and 99th percentile
cap_values <- function(x, lower = 0.01, upper = 0.99) {
  q <- quantile(x, c(lower, upper))
  pmin(pmax(x, q[1]), q[2])
}

adult <- adult %>%
  mutate(
    cg_topcode_flag   = as.integer(capital_gain == 99999),  # flag
    has_capital_gain  = as.integer(capital_gain > 0),       # flag
    has_capital_loss  = as.integer(capital_loss > 0),       # flag
    log_capital_gain  = log1p(capital_gain),                # log(1 + x)
    log_capital_loss  = log1p(capital_loss),
    hours_capped      = cap_values(hours_per_week),         # capped
    # 91% are from the US and the other 40 countries have barely any rows
    # each so just grouping it into US / Non-US
    native_region     = if_else(native_country == "United-States", "US", "Non-US"),
    income            = factor(income, levels = c("<=50K", ">50K"))
  )

# skewness before vs after the log (closer to 0 is better)
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
c(capital_gain = skewness(adult$capital_gain),
  log_capital_gain = skewness(adult$log_capital_gain))

# before/after histograms, only people with gain > 0
p_cg_raw <- adult %>% filter(capital_gain > 0) %>%
  ggplot(aes(capital_gain)) + geom_histogram(bins = 50, fill = "grey40") +
  labs(title = "Capital gain (raw)", x = "US$", y = "Count")
p_cg_log <- adult %>% filter(capital_gain > 0) %>%
  ggplot(aes(log_capital_gain)) + geom_histogram(bins = 50, fill = "#54A24B") +
  labs(title = "Capital gain (log1p)", x = "log(1 + US$)", y = "Count")
grid.arrange(p_cg_raw, p_cg_log, ncol = 2)
ggsave("figures/03_capital_gain_transform.png",
       arrangeGrob(p_cg_raw, p_cg_log, ncol = 2), width = 9, height = 4, dpi = 300)

## scaling - comparing min-max and z-score
normalize_mm <- function(x) (x - min(x)) / (max(x) - min(x))   # min-max
scaling_demo <- adult %>%
  transmute(age_raw = age, hours_raw = hours_capped,
            age_minmax = normalize_mm(age), hours_minmax = normalize_mm(hours_capped),
            age_z = as.numeric(scale(age)), hours_z = as.numeric(scale(hours_capped)))
summary(scaling_demo)

set.seed(123)
demo_sample <- slice_sample(scaling_demo, n = 3000)  # sample so it plots faster
s1 <- ggplot(demo_sample, aes(age_raw, hours_raw)) + geom_point(alpha = .2) +
  labs(title = "Original")
s2 <- ggplot(demo_sample, aes(age_minmax, hours_minmax)) +
  geom_point(alpha = .2, colour = "blue") + labs(title = "Min-max [0, 1]")
s3 <- ggplot(demo_sample, aes(age_z, hours_z)) +
  geom_point(alpha = .2, colour = "red") + labs(title = "Z-score")
grid.arrange(s1, s2, s3, ncol = 3)
ggsave("figures/04_scaling_comparison.png", arrangeGrob(s1, s2, s3, ncol = 3),
       width = 11, height = 3.8, dpi = 300)
# the shape of the points doesnt change, only the scale on the axes.
# going with z-score for PCA since PCA works off variance and needs
# everything on the same scale

#### 3. EDA ####
# questions i want to answer:
#  Q1 is income balanced?
#  Q2 how does age look, is it different for high earners?
#  Q3 anything weird with hours per week?
#  Q4 how much does education matter?
#  Q5 is there a gender gap, and is it there in every job?
#  Q6 marital status / relationship vs income
#  Q7 which numeric variables are correlated?
#  Q8 age and hours together

## Q1 - income balance
prop.table(table(adult$income))   # 75.2% <=50K vs 24.8% >50K so not balanced
p_q1 <- ggplot(adult, aes(income, fill = income)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = percent(after_stat(count) / sum(after_stat(count)), 0.1)),
            vjust = -0.4) +
  scale_fill_manual(values = income_cols) +
  scale_y_continuous(labels = comma) +
  labs(title = "Q1. Income classes are imbalanced (3 : 1)",
       x = "Annual income", y = "Number of people") +
  theme(legend.position = "none")
save_plot(p_q1, "05_Q1_income_balance", 6, 4)

## Q2 - age
adult %>% group_by(income) %>% summarise(median_age = median(age), mean_age = mean(age))
p_q2 <- ggplot(adult, aes(age, fill = income)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = income_cols) +
  labs(title = "Q2. High earners are older (median 43 vs 34)",
       x = "Age (years)", y = "Count", fill = "Income")
save_plot(p_q2, "06_Q2_age_by_income")
# skewed right. lower earners are mostly in their 20s, higher earners 35-55
# also a little spike at 90 bc everyone over 90 is recorded as 90

## Q3 - hours per week
mean(adult$hours_per_week == 40)   # about 47% put exactly 40 hours
p_q3 <- ggplot(adult, aes(hours_per_week, fill = income)) +
  geom_histogram(binwidth = 2, position = "stack") +
  scale_fill_manual(values = income_cols) +
  annotate("text", x = 55, y = 19000,
           label = "47% report exactly 40 h\n(standard full-time week)", hjust = 0) +
  labs(title = "Q3. Hours worked cluster at exactly 40",
       x = "Hours per week", y = "Count", fill = "Income")
save_plot(p_q3, "07_Q3_hours_spike")

p_q3b <- ggplot(adult, aes(income, hours_per_week, fill = income)) +
  geom_boxplot(outlier.alpha = 0.15) +
  scale_fill_manual(values = income_cols) +
  facet_wrap(~ sex) +
  labs(title = "Q3b. Hours worked by income and sex",
       x = NULL, y = "Hours per week") +
  theme(legend.position = "none")
save_plot(p_q3b, "08_Q3b_hours_box", 7, 4)

## Q4 - education vs income
edu_income <- adult %>%
  group_by(education, education_num) %>%
  summarise(n = n(), pct_high = mean(income == ">50K"), .groups = "drop") %>%
  arrange(education_num)
print(edu_income)
p_q4 <- ggplot(edu_income, aes(reorder(education, education_num), pct_high)) +
  geom_col(fill = "#F58518") +
  geom_text(aes(label = percent(pct_high, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = percent, limits = c(0, 0.85)) +
  labs(title = "Q4. Share earning >$50K rises steeply with education",
       x = "Education (ordered)", y = "% earning >$50K")
save_plot(p_q4, "09_Q4_education_income", 8, 5.5)
# goes from 1.4% for preschool to 16% HS-grad, 42% bachelors, 75% prof-school
## Q5 - gender gap
adult %>% group_by(sex) %>% summarise(pct_high = mean(income == ">50K"))
# men 31.3% vs women 11.4%, nearly 3 times more

occ_sex <- adult %>%
  group_by(occupation, sex) %>%
  summarise(pct_high = mean(income == ">50K"), n = n(), .groups = "drop")
p_q5 <- ggplot(occ_sex, aes(reorder(occupation, pct_high), pct_high, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c(Female = "#B279A2", Male = "#4C78A8")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Q5. Men out-earn women in almost every occupation",
       x = NULL, y = "% earning >$50K", fill = "Sex")
save_plot(p_q5, "10_Q5_occupation_sex", 8, 6)

## Q6 - marital status
p_q6 <- adult %>%
  count(marital_status, income) %>%
  ggplot(aes(reorder(marital_status, n), n, fill = income)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = income_cols) +
  scale_y_continuous(labels = percent) +
  labs(title = "Q6. Married (civ-spouse) people are far more likely to earn >$50K",
       x = NULL, y = "Share of group", fill = "Income")
save_plot(p_q6, "11_Q6_marital_income")

# relationship and sex kind of say the same thing
table(adult$relationship, adult$sex)
# husband = male and wife = female pretty much always, so theres a lot of
# overlap between these two. PCA should pick that up

## Q7 - correlation
cor_mat <- adult %>%
  select(age, education_num, hours_per_week, log_capital_gain, log_capital_loss) %>%
  mutate(income_high = as.integer(adult$income == ">50K")) %>%
  cor()
round(cor_mat, 2)
p_q7 <- as.data.frame(as.table(cor_mat)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3.8) +
  scale_fill_gradient2(low = "#4C78A8", mid = "white", high = "#E45756",
                       limits = c(-1, 1)) +
  labs(title = "Q7. Correlation matrix (numeric features + income)",
       x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p_q7, "12_Q7_correlation_heatmap", 7, 5.5)
# the numeric columns barely correlate with each other (all under 0.15)
# education_num and age are the ones most linked to income

## Q8 - age vs hours
set.seed(42)
p_q8 <- adult %>%
  slice_sample(n = 5000) %>%
  ggplot(aes(age, hours_per_week, colour = income)) +
  geom_jitter(alpha = 0.35, width = 0.4, height = 0.6) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(values = income_cols) +
  labs(title = "Q8. Age vs hours worked (5,000-row sample)",
       x = "Age", y = "Hours per week", colour = "Income")
save_plot(p_q8, "13_Q8_age_hours_scatter")
# high earners are mostly 30-60 and work a bit more hours.
# after 60 hours drop for everyone (retiring / part time)

# who has the 99999 capital gains?
adult %>% group_by(income) %>%
  summarise(pct_with_gain = mean(has_capital_gain),
            n_99999 = sum(cg_topcode_flag))
# all 229 of them are in the >50K group

#### 4. encoding ####

## label encoding
# income: <=50K = 0, >50K = 1
adult$income_label <- as.integer(adult$income == ">50K")
table(adult$income, adult$income_label)

# education has an order (preschool -> doctorate) so label encoding works
# here, numbers 1 to 16 in order
edu_order <- c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th",
               "11th", "12th", "HS-grad", "Some-college", "Assoc-voc",
               "Assoc-acdm", "Bachelors", "Masters", "Prof-school", "Doctorate")
adult$education_label <- as.integer(factor(adult$education, levels = edu_order))
all(adult$education_label == adult$education_num)  # TRUE, same as education_num

# not doing label encoding on stuff like occupation because then
# 1, 2, 3 would mean one job is "bigger" than another which makes no sense.
# so those get one-hot instead

## one-hot encoding
nominal_vars <- c("workclass", "marital_status", "occupation",
                  "relationship", "race", "sex", "native_region")

# example with race - one 0/1 column for each race
race_onehot <- model.matrix(~ race - 1, data = adult)
head(cbind(race = adult$race, race_onehot))

# all the nominal columns one-hot encoded
onehot_full <- map_dfc(nominal_vars, function(v) {
  as_tibble(model.matrix(as.formula(paste0("~ ", v, " - 1")), data = adult))
})
dim(onehot_full)    # 43 new 0/1 columns

## dummy encoding (k-1) for PCA
# with full one-hot some columns are just the opposite of another one
# (sexMale = 1 - sexFemale) which adds pointless variance to PCA.
# so dropping one level from each variable
dummy_df <- as_tibble(model.matrix(
  ~ workclass + marital_status + occupation + relationship + race + sex + native_region,
  data = adult))[, -1]                         # drop intercept
dim(dummy_df)       # 36 columns

# final table for PCA. leaving income out on purpose, only using it after
# to colour the plots
pca_input <- bind_cols(
  adult %>% select(age, education_num, hours_capped,
                   log_capital_gain, log_capital_loss),
  dummy_df
)
names(pca_input) <- make.names(names(pca_input))  # fix column names with - in them
dim(pca_input)      # 41 columns
write_csv(bind_cols(pca_input, income = adult$income), "outputs/encoded_dataset.csv")

#### 5. PCA ####

# just in case a column has no variance (scale would break)
pca_input <- pca_input[, sapply(pca_input, sd) > 0]

# scale = TRUE so age (17-90) doesnt overpower the 0/1 columns
pca <- prcomp(pca_input, center = TRUE, scale. = TRUE)
summary(pca)$importance[, 1:10]

## how much variance each PC explains
eig <- pca$sdev^2
var_tbl <- tibble(
  PC         = seq_along(eig),
  eigenvalue = eig,
  prop_var   = eig / sum(eig),
  cum_var    = cumsum(eig) / sum(eig)
)
print(head(var_tbl, 12))
write_csv(var_tbl, "outputs/pca_variance.csv")

n_kaiser <- sum(eig > 1)                         # kaiser rule, eigenvalue > 1
n_80     <- which(var_tbl$cum_var >= 0.80)[1]    # PCs needed for 80%
n_90     <- which(var_tbl$cum_var >= 0.90)[1]    # PCs needed for 90%
cat("Kaiser rule keeps:", n_kaiser, "PCs\n",
    "80% variance needs:", n_80, "PCs\n",
    "90% variance needs:", n_90, "PCs (out of", ncol(pca_input), ")\n")
# got around 21 for kaiser, 25 for 80% and 30 for 90%, out of 41

# scree plot, first 15
p_scree <- var_tbl %>% slice(1:15) %>%
  ggplot(aes(PC, prop_var)) +
  geom_col(fill = "#4C78A8") +
  geom_line() + geom_point() +
  geom_text(aes(label = percent(prop_var, 0.1)), vjust = -0.6, size = 3) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 1:15) +
  labs(title = "Scree plot - variance explained by each PC",
       x = "Principal component", y = "% of variance")
save_plot(p_scree, "14_pca_scree", 8, 4.5)

# cumulative variance with lines at 80 and 90%
p_cum <- ggplot(var_tbl, aes(PC, cum_var)) +
  geom_line(colour = "#4C78A8", linewidth = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed", colour = "red") +
  geom_vline(xintercept = c(n_80, n_90), linetype = "dotted") +
  annotate("text", x = n_80, y = 0.3, label = paste(n_80, "PCs = 80%"), angle = 90, vjust = -0.5) +
  annotate("text", x = n_90, y = 0.3, label = paste(n_90, "PCs = 90%"), angle = 90, vjust = -0.5) +
  scale_y_continuous(labels = percent) +
  labs(title = "Cumulative variance explained",
       x = "Number of principal components", y = "Cumulative % of variance")
save_plot(p_cum, "15_pca_cumulative", 8, 4.5)

## loadings - what each PC is made of
# the +/- sign doesnt really matter, its more about which variables
# go together
loadings <- as_tibble(pca$rotation[, 1:3], rownames = "feature")

top_loadings <- function(pc, n = 10) {
  loadings %>%
    select(feature, value = all_of(pc)) %>%
    slice_max(abs(value), n = n) %>%
    mutate(component = pc)
}
top_pc <- bind_rows(top_loadings("PC1"), top_loadings("PC2"))
print(top_pc, n = 20)
write_csv(top_pc, "outputs/pca_top_loadings.csv")

p_load <- ggplot(top_pc, aes(reorder(feature, abs(value)), value, fill = value > 0)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ component, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#F58518", `FALSE` = "#4C78A8"), guide = "none") +
  labs(title = "Top 10 loadings on PC1 and PC2", x = NULL, y = "Loading")
save_plot(p_load, "16_pca_loadings", 11, 5)

## biplot coloured by income
scores <- as_tibble(pca$x[, 1:2]) %>% mutate(income = adult$income)

# average PC score for each income group
scores %>% group_by(income) %>% summarise(mean_PC1 = mean(PC1), mean_PC2 = mean(PC2))

set.seed(7)
score_sample <- slice_sample(scores, n = 6000)
arrow_feats <- top_loadings("PC1", 6)$feature %>%
  union(top_loadings("PC2", 4)$feature)
arrows <- loadings %>% filter(feature %in% arrow_feats)
arrow_scale <- 0.8 * max(abs(score_sample$PC1)) / max(abs(arrows$PC1))

p_biplot <- ggplot(score_sample, aes(PC1, PC2)) +
  geom_point(aes(colour = income), alpha = 0.3, size = 1) +
  geom_segment(data = arrows,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  geom_text(data = arrows,
            aes(x = PC1 * arrow_scale * 1.1, y = PC2 * arrow_scale * 1.1, label = feature),
            size = 3) +
  scale_colour_manual(values = income_cols) +
  labs(title = "PCA biplot: PC1 vs PC2 (6,000-row sample)",
       x = paste0("PC1 (", percent(var_tbl$prop_var[1], 0.1), ")"),
       y = paste0("PC2 (", percent(var_tbl$prop_var[2], 0.1), ")"),
       colour = "Income")
save_plot(p_biplot, "17_pca_biplot", 9, 6.5)

p_pc1_density <- ggplot(scores, aes(PC1, fill = income)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = income_cols) +
  labs(title = "PC1 separates the income groups", x = "PC1 score", y = "Density")
save_plot(p_pc1_density, "18_pca_pc1_density", 7, 4)

## PCA with only the 5 numeric columns, to compare
pca_num <- prcomp(adult %>% select(age, education_num, hours_capped,
                                   log_capital_gain, log_capital_loss),
                  center = TRUE, scale. = TRUE)
summary(pca_num)
round(pca_num$rotation, 2)
# variance is split pretty evenly here (~26, 21, 19, 18, 16%) since these
# columns hardly correlate, so not much to reduce

## reduced dataset - keeping the PCs that cover 80%
adult_pca_reduced <- as_tibble(pca$x[, 1:n_80]) %>% mutate(income = adult$income)
dim(adult_pca_reduced)
write_csv(adult_pca_reduced, "outputs/adult_pca_reduced.csv")

## what the PCA shows
# PC1 (~8%): married, older, male, more hours on one side and never-married /
#   own-child on the other. kind of a life stage thing. >50K people average
#   about +1.6 on PC1 and <=50K about -0.5, so PC1 splits income pretty well
#   even though income wasnt used in the PCA at all
# PC2 (~5%): mostly race (white vs black) and private vs local-gov jobs.
#   doesnt split income
# need 25 of 41 PCs to get 80% - the variance is really spread out because of
#   all the one-hot columns. so about 40% fewer columns, not a huge reduction,
#   and the PCs are harder to explain than the original columns


#### 6. saving the clean data ####
adult_clean <- adult %>%
  select(age, workclass, education, education_num, marital_status, occupation,
         relationship, race, sex, capital_gain, capital_loss, hours_per_week,
         native_country, native_region, log_capital_gain, log_capital_loss,
         hours_capped, cg_topcode_flag, has_capital_gain, has_capital_loss,
         income, income_label)
write_csv(adult_clean, "outputs/adult_clean.csv")

cat("\nFinal cleaned dataset:", nrow(adult_clean), "rows x",
    ncol(adult_clean), "columns\n")
cat("Figures saved in ./figures, tables saved in ./outputs\n")

# sessionInfo()

# references
# Becker, B. and Kohavi, R. (1996) Adult [Dataset]. UCI Machine Learning
#   Repository. Available at: https://doi.org/10.24432/C5XW20
# Jolliffe, I.T. and Cadima, J. (2016) 'Principal component analysis: a review
#   and recent developments', Philosophical Transactions of the Royal Society A,
#   374(2065), 20150202.
# Larose, D.T. and Larose, C.D. (2015) Data Mining and Predictive Analytics.
#   2nd edn. Hoboken, NJ: Wiley.
# Wickham, H., Cetinkaya-Rundel, M. and Grolemund, G. (2023) R for Data
#   Science. 2nd edn. Sebastopol, CA: O'Reilly.
# R Core Team (2024) R: A Language and Environment for Statistical Computing.
#   Vienna: R Foundation for Statistical Computing.
