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
