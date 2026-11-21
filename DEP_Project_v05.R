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
