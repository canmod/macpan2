library(dplyr)
library(tidyr)
library(McMasterPandemic)


## Section 1: Read Data Sources

### Get time series data from MLi's data repo; mobility data from Apple and Google.

tsdat_url <- "https://wzmli.github.io/COVID19-Canada/git_push/clean.Rout.csv"
google_url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
apple_url <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/apple_reports/applemobilitytrends.csv"

tsdat <- read.csv(tsdat_url)
apple <- read.csv(apple_url,check.names=FALSE)
google <- read.csv(google_url)

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

#filter(clean_tsdata, var == "report", date < ymd(20201001)) |> ggplot() + geom_point(aes(date, value))

### Clean mobility data

mobility_dat = (
  (apple
   %>% filter(alternative_name == "ON", transportation_type == "driving")
   %>% pivot_longer(cols=-c("geo_type","region","transportation_type","alternative_name","sub-region","country")
                    , names_to="date",names_transform = as.Date)
   # create relative percent change (to match google data)
   %>% mutate(value = value - 100)
   %>% select(date, value)
  ) 
  %>% full_join(google
                %>% filter(iso_3166_2_code == "CA-ON")
                %>% mutate(date = as.Date(date))
                %>% select(date,starts_with("retail_and_recreation"),starts_with("workplaces"))
  )
  %>% arrange(date)
  # compute 7 day moving average
  %>% mutate(across(where(is.numeric),~ stats::filter(.x, filter = rep(1/7, 7), sides = 2)))
  # scale to have pre-pandemic value of 1
  %>% mutate(across(where(is.numeric), ~ 1 + (.x/100)))
  # compute average of all mobility values
  %>% group_by(date)
  %>% summarize(mobility_ind = mean(c_across(where(is.numeric)),na.rm = TRUE))
  %>% ungroup()
  %>% na.omit()
)


saveRDS(clean_tsdata, system.file("starter_models","macpan_base","data","ts_data.RDS", package="macpan2"))
saveRDS(mobility_dat, system.file("starter_models","macpan_base","data","mobility_data.RDS", package="macpan2"))
