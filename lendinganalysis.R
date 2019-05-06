# Script for exploratory analysis of LendingClub data

# Load relevant R packages
library(lubridate)
library(dplyr)
library(ggplot2)

# Read in csv data files obtained from website
filename_accepted <- "data/LoanStats.csv"
filename_rejected <- "data/RejectStats.csv"

#accepted_raw <- read.csv(filename_accepted, skip = 1)
#rejected_raw <- read.csv(filename_rejected, skip = 1)

# Tidy the data
# Accepted loans
accepted_tidy <- accepted_raw %>%
    # Consider only variables relevant to exploratory analysis
    select(date = issue_d,
           amount_asked = loan_amnt,
           amount_lent = funded_amnt,
           debt_income_ratio = dti,
           employment_length = emp_length) %>%
    # Reformat the date variable to month-year-day (use 01 for day)
    mutate(date = as.character(date)) %>%
    mutate(date = gsub("Jan","01",date), date = gsub("Feb","02",date),
           date = gsub("Mar","03",date), date = gsub("Apr","04",date),
           date = gsub("May","05",date), date = gsub("Jun","06",date),
           date = gsub("Jul","07",date), date = gsub("Aug","08",date),
           date = gsub("Sep","09",date), date = gsub("Oct","10",date),
           date = gsub("Nov","11",date), date = gsub("Dec","12",date),
           date = paste(date,"01", sep = "-")) %>%
    # Create month and year variable from date variable
    mutate(month = month(myd(date)), year = year(myd(date))) %>%
    select(-date) %>%
    # Neglect single observation with incorrect date input
    filter(!is.na(year)) %>%
    # Create application decision variable
    mutate(decision = "accepted")

# Rejected loans
rejected_tidy <- rejected_raw %>%
    # Consider only variables relevant to exploratory analysis
    select(date = Application.Date,
           amount_asked = Amount.Requested,
           debt_income_ratio = Debt.To.Income.Ratio,
           employment_length = Employment.Length) %>%
    # Include columns indicating rejection of application
    mutate(amount_lent = 0L, decision = "rejected") %>%
    # Convert debt-to-income ratio to numeric data
    mutate(debt_income_ratio = as.character(debt_income_ratio),
           debt_income_ratio = gsub("%","",debt_income_ratio),
           debt_income_ratio = as.numeric(debt_income_ratio)) %>%
    # Create month and year variable from date variable
    mutate(month = month(ymd(date)), year = year(ymd(date))) %>%
    select(-date)

# Combine date into one table
#tidydata <- full_join(accepted_tidy, rejected_tidy)
    
# Determine acceptance rates from 2007 to 2015
acc_rate <- tidydata %>%
    group_by(year) %>%
    summarize(num_total = n(),
              num_accept = sum(decision == "accepted"),
              num_reject = sum(decision == "rejected"),
              rate_accept = num_accept/num_total,
              ratio_accp_rej = num_accept/num_reject)