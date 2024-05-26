##PH125.6X Data: Wrangling 
# In the Data Import section, you will learn how import data into R.
# 
# After completing this section, you will be able to:
#   
# Import data from spreadsheets.
# Identify and set your working directory and specify the path to a file.
# Use the readr and readxl packages to import spreadsheets.
# Use R-base functions to import spreadsheets.
# Download files from the internet using R.

#Section 1: Data Import / 1.1: Data Import---------------

# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)

#readr is the tidyverse library that includes functions for reading data stored 
#in text file spreadsheets into R. Functions in the package include 
#read_csv(), read_tsv(), read_delim() and more. 
#These differ by the delimiter they use to split columns.
#The readxl package provides functions to read Microsoft Excel formatted files.
#The excel_sheets() function gives the names of the sheets in the Excel file. 
#These names are passed to the sheet argument for the readxl functions 
#read_excel(), read_xls() and read_xlsx().
#The read_lines() function shows the first few lines of a file in R.

library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Ex：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))


#R-base functions 
# R-base import functions (read.csv(), read.table(), read.delim()) generate 
# data frames rather than tibbles.
# Note that as of R 4.0, it is no longer necessary to use the argument 
# stringsAsFactors=FALSE to prevent characters from being converted into factors.

# filename is defined in the previous video
# read.csv to import the data
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)

# read files from internet 
# The read_csv() function and other import functions can read a URL directly.
# If you want to have a local copy of the file, you can use download.file().
# tempdir() creates a directory with a name that is very unlikely not to be unique.
# tempfile() creates a character string that is likely to be a unique filename.

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)


#Assessment section 1 - 1.1. part 2
library(tidyverse)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
url_data <- read_csv(url, col_names = FALSE)


#Section 2: Tidy Data /2.1: Reshaping Data-------------------------------------
#In the Tidy Data section, you will learn how to convert data from a raw to a tidy format.
# 
# This section is divided into three parts: Reshaping Data, Combining Tables, and Web Scraping. There are comprehension checks at the end of each part.
# 
# After completing the Tidy Data section, you will be able to:
#   
#   Reshape data using functions from the tidyr package, including gather(), spread(), separate() and unite().
# Combine information from different tables using join functions from the dplyr package.
# Combine information from different tables using binding functions from the dplyr package.
# Use set operators to combine data frames.
# Gather data from a website through web scraping and use of CSS selectors.

#In tidy data, each row represents an observation and each column represents a different variable.
# In wide data, each row includes several observations and one of the variables is stored in the header.

#Code 
library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# 
# After importing data, a common next step is to reshape the data into a form useful for the rest of the analysis by tidying it. The tidyr package includes several useful functions for tidying data.
# The pivot_longer() function converts wide data into tidy data.
# The first argument of pivot_longer() is the data frame to be reshaped. The second argument specifies the columns containing the values to be moved into a single column.
# The new column of values is called value by default and the column containing the original names of those columns is called name by default.
# The values_to and names_to arguments can be used to change the default names of these columns.


# example dataset: fertility data in wide format (from previous video)
library(tidyverse) 
library(dslabs)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# snippet of wide data
wide_data %>% select(country, '1960':'1965')

# move the values in the columns 1960 through 2015 into a single column
wide_data %>% pivot_longer(`1960`:`2015`)

# another way to do this - only country isn't being pivoted
wide_data %>% pivot_longer(-country)

# change the default column names
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility")
head(new_tidy_data)

# compare the class from our original tidy data (year is an integer) and in the new version (year is a character)
class(tidy_data$year)
class(new_tidy_data$year)

# use the names_transform argument to change the class of the year values to numeric
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility", 
               names_transform = list(year=as.numeric))

# plot the data as before
new_tidy_data %>% ggplot(aes(year, fertility, color = country)) +
  geom_point()


#The pivot_wider() function converts tidy data into wide data, which can be a useful intermediate step in data tidying.
# The data frame to be reshaped is the first argument in pivot_wider().
# The argument names_from tells pivot_wider() which variable will be used for the column names and the argument values_from tells pivot_wider() which variable to use to fill in the values.

# still working with the same data as in the previous video
# convert the tidy data to wide data
new_wide_data <- new_tidy_data %>% 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)
# 
# The separate() function splits one column into two or more columns at a specified character that separates the variables.
# The separate() function takes three arguments (apart from the data): the name of the column to be separated, the names to be used for the new columns, and the character that separates the variables.
# When there is an extra separation, you can use extra = "merge" to merge the last two variables.

# import data
path <- system.file("extdata", package = "dslabs")
fname <-  "life-expectancy-and-fertility-two-countries-example.csv"
filename <- file.path(path, fname)

raw_dat <- read_csv(filename)
select(raw_dat, 1:4)

# pivot all columns except country
dat <- raw_dat %>% pivot_longer(-country)
head(dat)
dat$name[1:5]

# separate on underscores
dat %>% separate(name, c("year", "name"), sep = "_")

# separate on underscores (the default), convert years to numeric
dat %>% separate(name, c("year", "name"), convert = TRUE)

# split on all underscores, pad empty cells with NA
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill = "right", convert = TRUE)

# split on first underscore but keep life_expectancy merged
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE)

# separate then create a new column for each variable using pivot_wider
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE) %>%
  pivot_wider()

#unite 
# The unite() function joins two columns into one.

# using the data from the previous video
# if we had used this non-optimal approach to separate
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE)

# we could unite the second and third columns using unite()
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(variable_name, name_1, name_2, sep="_")

# spread the columns
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(name, name_1, name_2, sep="_") %>%
  spread(name, value) %>%
  rename(fertlity = fertility_NA)

#Assessment
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- pivot_longer(co2_wide, -year, names_to = "month", values_to = "co2")

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))

#Section 2: Tidy Data/2.2: Combining Tables

# The join functions in the dplyr package combine two tables such that matching rows are together.
# left_join() only keeps rows that have information in the first table.
# right_join() only keeps rows that have information in the second table.
# inner_join() only keeps rows that have information in both tables.
# full_join() keeps all rows from both tables.
# semi_join() keeps the part of first table for which we have information in the second.
# anti_join() keeps the elements of the first table for which there is no information in the second.

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

#combining tables
# Unlike the join functions, the binding functions do not try to match by a variable, but rather just combine datasets.
# bind_cols() binds two objects by making them columns in a tibble. The R-base function cbind() binds columns but makes a data frame or matrix instead.
# The bind_rows() function is similar but binds rows instead of columns. The R-base function rbind() binds rows but makes a data frame or matrix instead.

bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)
# 
# By default, the set operators in R-base work on vectors. If tidyverse/dplyr are loaded, they also work on data frames.
# You can take intersections of vectors using intersect(). This returns the elements common to both sets.
# You can take the union of vectors using union(). This returns the elements that are in either set.
# The set difference between a first and second argument can be obtained with setdiff(). Note that this function is not symmetric.
# The function set_equal() tells us if two sets are the same, regardless of the order of elements.

# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)


#Assessments 
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

People %>% as_tibble()
 
AwardsPlayers_2016 <- AwardsPlayers %>% 
  filter(yearID == "2016")

AwardsPlayers_2016_distinct <- 
  AwardsPlayers_2016 %>% 
  distinct(playerID) %>% 
  mutate("2016_award" = "Y")


top_names <- 
  top %>% 
  left_join(AwardsPlayers_2016_distinct, by = c("playerID"))

award_non_top <- 
  AwardsPlayers_2016_distinct %>% 
  anti_join(top, by = c("playerID"))
