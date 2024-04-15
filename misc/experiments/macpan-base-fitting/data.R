library(tidyverse)
library(McMasterPandemic)
#library(zoo)

## Section 1: Read Data Sources

### Get time series data from MLi's data repo; mobility data from Apple and Google.

tsdat_url <- "https://wzmli.github.io/COVID19-Canada/git_push/clean.Rout.csv"
# google_url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
# apple_url <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/apple_reports/applemobilitytrends.csv"

tsdat <- read_csv(tsdat_url)
# apple <- read_csv(apple_url)
# google <- read_csv(google_url)

## Section 2: Clean data
### Clean ts data
Ontario_dat <- (tsdat
								%>% filter(Province=="ON")
								%>% select(Province,Date,Hospitalization,ICU,Ventilator,deceased,newConfirmations,newTests)
								%>% mutate(newDeaths=c(NA,diff(deceased))
													 ## ON hosp includes ICU, our model compartment is just acute care
													 , Hospitalization=Hospitalization-ICU)
								%>% select(-deceased)
								%>% pivot_longer(names_to="var",-c(Date,Province))
								%>% setNames(tolower(names(.)))
								%>% ungroup()
)

## translate variable names to internally used values
## drop unused variables
keep_vars <- c("H","ICU","death","report","newTests")



clean_tsdata <- (Ontario_dat
								 %>% mutate_at("var", trans_state_vars)
								 %>% filter(var %in% keep_vars)
)

filter(clean_tsdata, var == "report", date < ymd(20201001)) |> ggplot() + geom_point(aes(date, value))
