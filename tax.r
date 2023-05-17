# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(scales)
# 
# zip_il_tax <- tidyxl::xlsx_cells("20zp14il.xlsx") #%>% 
#  # head(100000)
# 
# zip2 <-
#   zip_il_tax %>% 
#     #filter(is_blank == FALSE) %>%
#     # filter out top header
#     filter(row > 3) %>% 
#     behead("up-left", header) %>%
#     behead("up", subheader) %>% 
#     behead("left", zip_code) %>% 
#     behead("left", income_rng) %>% 
#     filter(is_blank == FALSE) #%>% sample_n(100) %>% view()
# 
# zip2 %>%
#   select(zip_code, income_rng, header, subheader, content, data_type) %>% 
#   filter(!is.na(zip_code)) %>% 
#   mutate(income_rng = replace_na(income_rng, "Total"))
#  # verified only one data type
#  # skimr::skim()
# 
# zip3 <-
#   zip2 %>%
#     select(zip_code, income_rng, header, subheader, content) %>% 
#     filter(!is.na(zip_code)) %>% 
#     mutate(income_rng = as_factor(replace_na(income_rng, "Total")),
#            subheader = replace_na(subheader, "value"),
#            content = as.numeric(content))

#zip3 %>% saveRDS("ziptaxtb.rds")
zip3 <- readRDS("ziptaxtb.rds")

il_total <-
  zip3 %>% 
    filter(zip_code == 0)

# number of returns different than sum of other return types
il_total %>% 
  filter(header %in% c("Number of returns [2]"),
         income_rng != "Total") %>% 
  ggplot(aes(x = income_rng, y = content)) +
  geom_col()


il_total %>% 
  filter(header %in% c("Number of single returns",
                       "Number of joint returns",
                       "Number of head of household returns"),
         income_rng != "Total") %>% 
  ggplot(aes(x = income_rng, y = content, fill = header)) +
  geom_bar(position = "stack", stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

zip3 %>% 
  filter(zip_code == 60618)

chi_total <-
  zip3 %>% 
    filter(zip_code >= 60600,
           zip_code < 60700,
           income_rng != "Total") %>% 
    group_by(income_rng, header, subheader) %>% 
    summarize(content = sum(content)) %>% 
    ungroup()

chi_total %>% 
  filter(header %in% c("Number of single returns",
                       "Number of joint returns",
                       "Number of head of household returns"),
         income_rng != "Total") %>% 
  ggplot(aes(x = income_rng, y = content, fill = header)) +
  geom_bar(position = "stack", stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) + 
  scale_y_continuous(labels = comma)#function(x) format(x, scientific = FALSE))

chi_total %>% 
  filter(header == "Number of returns [2]") %>%
  summarize(sum(content))

# more complicated breakdown of available headers
# Variable 1 - Number of Returns [2]
## Variable 2 to 4 - Breakdown by filer type
## Variable 5 to 6 - Breakdown by file type (electronic, paper)
## Variable 7 - Number with paid preparer's signature
## Variable 8 - Number with direct deposit
## Variable 9 - Number with virtual currency indicator
# Variable 10 - Number of individuals (filer(s) + dependents?) [3]
# Variable 11 - Total Number of Volunteer Prepared Returns [4]
## Variable 12 - Number of Volunteer Income Tax Assistance returns
## Variable 13 - Number of Tax Counseling for the Elderly returns
## Variable 14 - Number with earned income credit
# Variable 15 - Number of refund anticipation check returns [5]
# Variable 16 - Number of elderly returns [6]
# Variable 17 - Adjusted Gross Income (AGI) [7]
## Total Income
### Variable 18 - Number of Returns
### Variable 19 - Amount
## Salaries and wages in AGI
### Variable 20 - Number of Returns
### Variable 21 - Amount
## Taxable interest
### Variable 22 - Number of Returns
### Variable 23 - Amount
## Ordinary dividends
### Variable 24 - Number of Returns
### Variable 25 - Amount
## Qualified dividends [8]
### Variable 26 - Number of Returns
### Variable 27 - Amount
## State and local income tax refunds
### Variable 28 - Number of Returns
### Variable 29 - Amount
## Business or profession net income (less loss)
### Variable 30 - Number of Returns
### Variable 31 - Amount
## Net capital gain (less loss) in AGI
### Variable 32 - Number of Returns
### Variable 33 - Amount
## Taxable individual retirement arrangements distributions
### Variable 34 - Number of Returns
### Variable 35 - Amount
## Taxable pensions and annuities
### Variable 36 - Number of Returns
### Variable 37 - Amount
## Variable 38 - Number of farm returns
## Unemployment compensation [9]
### Variable 39 - Number of Returns
### Variable 40 - Amount
## Taxable Social Security benefits
### Variable 41 - Number of Returns
### Variable 42 - Amount
## Partnership/S-corp net income (less loss)
### Variable 43 - Number of Returns
### Variable 44 - Amount
## Total statuory adjustments
### Variable 45 - Number of Returns
### Variable 46 - Amount
## Educator expenses
### Variable 47 - Number of Returns
### Variable 48 - Amount
## Self-employed (Keogh) retirement plans
### Variable 49 - Number of Returns
### Variable 50 - Amount
## Self-employed health insurance deduction
### Variable 51 - Number of Returns
### Variable 52 - Amount
## Individual retirement arrangement payments
### Variable 53 - Number of Returns
### Variable 54 - Amount
## Student loan interest deduction
### Variable 55 - Number of Returns
### Variable 56 - Amount
## Charitable contributions (if took standard deduction)
### Variable 57 - Number of Returns
### Variable 58 - Amount
# Total Standard Deduction
## Variable 59 - Number of Returns
## Variable 60 - Amount
#-# Basic Standard Deduction ?
### Variable 61 - Number of Returns
### Variable 62 - Amount
# Additional Standard Deduction
## Variable 63 - Number of Returns
## Variable 64 - Amount
# Itemized Deduction
## Total
### Variable 65 - Number of Returns
### Variable 66 - Amount
### Variable 67 - Amount of AGI
## Total medical and dental expenses deductions
### Variable 68 - Number of Returns
### Variable 69 - Amount
## State and local income taxes
### Variable 70 - Number of Returns
### Variable 71 - Amount
## State and local general sales tax
### Variable 72 - Number of Returns
### Variable 73 - Amount
## Real estate taxes
### Variable 74 - Number of Returns
### Variable 75 - Amount
## Personal property taxes
### Variable 76 - Number of Returns
### Variable 77 - Amount
## Limited state and local taxes paid
### Variable 78 - Number of Returns
### Variable 79 - Amount
## Total taxes paid
### Variable 80 - Number of Returns
### Variable 81 - Amount
## Home mortgage interest paid
### Variable 82 - Number of Returns
### Variable 83 - Amount
## Home mortgage interest paid to personal seller
### Variable 84 - Number of Returns
### Variable 85 - Amount
## Deductible points
### Variable 86 - Number of Returns
### Variable 87 - Amount
## Qualified mortgage insurance premiums
### Variable 88 - Number of Returns
### Variable 89 - Amount
## Investment interest paid
### Variable 90 - Number of Returns
### Variable 91 - Amount
## Total charitable contributions
### Variable 92 - Number of Returns
### Variable 93 - Amount
## Other non-limited miscellaneous deduction
### Variable 94 - Number of Returns
### Variable 95 - Amount
## Qualified business income deduction
### Variable 96 - Number of Returns
### Variable 97 - Amount
# Taxable Income
## Variable 98 - Number of Returns
## Variable 99 - Amount
# Income tax before credits
## Variable 100 - Number of Returns
## Variable 101 - Amount
# Alternative minimum tax
## Variable 102 - Number of Returns
## Variable 103 - Amount
# Excess advance premium tax credit repayment
## Variable 104 - Number of Returns
## Variable 105 - Amount
# Total tax credit [10]
## Variable 106 - Number of Returns
## Variable 107 - Amount
## Foreign tax credit
### Variable 108 - Number of Returns
### Variable 109 - Amount
## Child and dependant care credit
### Variable 110 - Number of Returns
### Variable 111 - Amount
## Nonrefundable education credit
### Variable 112 - Number of Returns
### Variable 113 - Amount
## Retirment savings contributions credit
### Variable 114 - Number of Returns
### Variable 115 - Amount
## Child and other dependent credit
### Variable 116 - Number of Returns
### Variable 117 - Amount
## Residential energy tax credit 
### Variable 118 - Number of Returns
### Variable 119 - Amount
## Self-employment tax
### Variable 120 - Number of Returns
### Variable 121 - Amount
## Total premium tax credit
### Variable 122 - Number of Returns
### Variable 123 - Amount
## Advance premium tax credit
### Variable 124 - Number of Returns
### Variable 125 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount

# save for later
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount
## Total tax payments
### Variable 126 - Number of Returns
### Variable 127 - Amount






income_tax <- 
  function(income,
           brackets = c(18200, 37000, 80000, 180000, Inf),
           rates = c(0, .19, .325, .37, .45)) {        
    sum(diff(c(0, pmin(income, brackets))) * rates)
  }

chi_income_tax <- 
  function(income,
           brackets = c(100000, Inf),
           rates = c(0, 0.035)) {        
    sum(diff(c(0, pmin(income, brackets))) * rates)
  }
