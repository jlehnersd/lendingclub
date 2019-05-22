# Load relevant R packages
library(lubridate)
library(dplyr)
library(ggplot2)

# Source analysis functions
source("read_textfile.R")
source("get_rawdata.R")
source("load_csvdata.R")

# Get the URLs needed to download all of the data
url_files <- c("data/urls/accepted_urls.txt", "data/urls/rejected_urls.txt")
urls <- read_textfile(url_files)
names(urls) <- c("accepted", "rejected")

# Download and extract raw data if not already present in data directory
accepted_filenames <- read_textfile("data/filenames/accepted_filenames.txt")
accepted_filenames <- accepted_filenames[[1]]
rejected_filenames <- read_textfile("data/filenames/rejected_filenames.txt")
rejected_filenames <- rejected_filenames[[1]]

get_rawdata(urls$accepted, accepted_filenames)
get_rawdata(urls$rejected, rejected_filenames)

# Read in csv files and combine into single data frames
if(!exists("accepted_data_")){
    accepted_data_ <- load_csvdata(accepted_filenames)
}
accepted_data <- accepted_data_

if(!exists("rejected_data_")){
    rejected_data_ <- load_csvdata(rejected_filenames)
}
rejected_data <- rejected_data_

# Rename the variables with more descriptive names, but save original names too
rawnames_accepted <- read_textfile("data/varnames/rawnames_accepted.txt")
rawnames_accepted <- rawnames_accepted[[1]]
newnames_accepted <- read_textfile("data/varnames/newnames_accepted.txt")
newnames_accepted <- newnames_accepted[[1]]
names(accepted_data) <- newnames_accepted

rawnames_rejected <- read_textfile("data/varnames/rawnames_rejected.txt")
rawnames_rejected <- rawnames_rejected[[1]]
newnames_rejected <- read_textfile("data/varnames/newnames_rejected.txt")
newnames_rejected <- newnames_rejected[[1]]
names(rejected_data) <- newnames_rejected

# Create DECISION variable indicating loan application status
accepted_data <- mutate(accepted_data, decision = "accepted")
rejected_data <- mutate(rejected_data, decision = "rejected")

# Create YEAR and MONTH variables to replace original date variables
accepted_data <- accepted_data %>%
    rename(date = acceptance_date) %>%
    filter(date != "") %>%
    mutate(date = gsub("Jan","01",date), date = gsub("Feb","02",date),
           date = gsub("Mar","03",date), date = gsub("Apr","04",date),
           date = gsub("May","05",date), date = gsub("Jun","06",date),
           date = gsub("Jul","07",date), date = gsub("Aug","08",date),
           date = gsub("Sep","09",date), date = gsub("Oct","10",date),
           date = gsub("Nov","11",date), date = gsub("Dec","12",date),
           date = paste(date,"01", sep = "-")) %>%
    mutate(month = month(myd(date)), year = year(myd(date))) %>%
    select(-date)
rejected_data <- rejected_data %>%
    rename(date = application_date) %>%
    mutate(month = month(ymd(date)), year = year(ymd(date))) %>%
    select(-date)

# Modify debt_to_income_ratios to be numeric and to not be percentages
accepted_data <- accepted_data %>%
    mutate(debt_to_income_ratio = debt_to_income_ratio/100)
rejected_data <- rejected_data %>%
    mutate(debt_to_income_ratio = gsub("%","",debt_to_income_ratio),
           debt_to_income_ratio = as.numeric(debt_to_income_ratio),
           debt_to_income_ratio = debt_to_income_ratio/100)

# Determine number of total loan applications each year
accepted_apps <- accepted_data %>%
    select(year) %>%
    group_by(year) %>%
    summarize(total_accepted = n())
rejected_apps <- rejected_data %>%
    select(year) %>%
    group_by(year) %>%
    summarize(total_rejected = n())
total_apps <- bind_cols(accepted_apps, rejected_apps)
total_apps <- mutate(total_apps, total = total_accepted + total_rejected)

# Combine both data sets by common variables for comparative analysis
accepted <- accepted_data %>%
    select(decision,
           year,
           month,
           loan_amount,
           loan_title,
           debt_to_income_ratio,
           employment_length,
           state) %>%
    mutate(as.character(employment_length),
           employment_length = gsub("n/a","",employment_length))
rejected <- rejected_data %>%
    select(decision,
           year,
           month,
           loan_amount,
           loan_title,
           debt_to_income_ratio,
           employment_length,
           state) %>%
    mutate(employment_length = gsub("n/a","",employment_length))
both_data <- full_join(accepted, rejected)


# Determine proportion of loan applications that are accepted each year
yearly_acceptance <- both_data %>%
    group_by(year) %>%
    summarize(total_applications = n(),
              num_accepted = sum(decision == "accepted"),
              num_rejected = sum(decision == "rejected"),
              proportion_accepted = num_accepted/total_applications,
              ratio_accept_reject = num_accepted/num_rejected)

# Determine proportion of loan applications that are accepted by state each year
state_acceptance <- both_data %>%
    filter(state != "") %>%
    group_by(state, year) %>%
    summarize(total_applications = n(),
              num_accepted = sum(decision == "accepted"),
              num_rejected = sum(decision == "rejected"),
              proportion_accepted = num_accepted/total_applications,
              ratio_accept_reject = num_accepted/num_rejected) %>%
    arrange(proportion_accepted)

# Look at acceptance trends for overall low (TN) and high (CA) rate states
california <- state_acceptance %>%
    filter(state == "CA") %>%
    arrange(year)
tennessee <- state_acceptance %>%
    filter(state == "TN") %>%
    arrange(year)

# Compare annual debt-to-income ratios
debt_ratio <- both_data %>%
    # Remove outliers such as negative values and very large values
    filter(!is.na(debt_to_income_ratio),
           debt_to_income_ratio < 10 & debt_to_income_ratio >= 0) %>%
    group_by(decision, year) %>%
    summarize(avg_ratio = mean(debt_to_income_ratio))

# Find proportion of requested loan amounts that are funded for accepted loans
amount_funded <- accepted_data %>%
    select(year,
           month,
           state,
           debt_to_income_ratio,
           employment_length,
           loan_amount,
           funded_amount) %>%
    mutate(proportion = funded_amount/loan_amount) %>%
    mutate(fully_funded = ifelse(proportion == 1, "yes", "no")) %>%
    group_by(year,fully_funded) %>%
    summarize(number_in_range = n())