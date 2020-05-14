library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)




rm(list = ls())
"C:Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_daily_reports_us/
"
"C:Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_time_series/
"
path1 <- "C:/Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_daily_reports_us/"
path2 <- "C:/Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_time_series/"
path3 <- "C:/Users/Mark/Documents/COVID-19/docs/"
list.files(path2)
# 
# dailies <- list.files(path2, pattern = "csv$", full.names = T) %>%
#   set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
#   map_df(read_csv, .id = "filename") %>%
#   mutate(date_ = mdy(filename))

dailies <- list.files(path1, pattern = "[0-9]{2}-[0-9]{2}-[0-9]{4}.csv$", full.names = T) %>%
  set_names(nm = (basename(.))) %>%  # basename gets the filename
  map_df(read_csv, .id = "filename") %>%
  mutate(date_ = mdy(stringr::str_extract(filename, "[0-9]{2}-[0-9]{2}-[0-9]{4}" )))

 


glimpse(dailies)
  

case_ts <- read_csv(paste0(path2, "time_series_covid19_confirmed_US.csv"))

glimpse(case_ts)


# daily_ex <- read_csv(paste0(path2, "04-12-2020.csv"))
# glimpse(daily_ex)
# unique(daily_ex$Last_Update)
# daily_ex$date_ <- as.Date(daily_ex$Last_Update)

# deaths_ts <- read_csv(paste0(path1, "time_series_covid19_deaths_US.csv"))
# glimpse(deaths_ts)

# cases_greene <- case_ts %>%
#   filter(Province_State == "Ohio" & Admin2 == "Greene")

# deaths_greene <- deaths_ts %>%
#   filter(Province_State == "Ohio" & Admin2 == "Greene")

# write_csv(cases_greene, "cases_greene.csv")
# write_csv(deaths_greene, "deaths_greene.csv")

# Function to select data by dates and convert format from wide to long
# pivot_func <- function(df, date1 = "1/22/20", date2 = "5/5/20", named = "date_", valued = "cases"){
#   ts_pivot <- df %>% pivot_longer(
#     date1:date2, 
#     names_to = named,
#     values_to = valued
#   )
#   ts_pivot$date_ = mdy(ts_pivot$date_)
#   return(ts_pivot)
# }

# case_pivot <- pivot_func(case_ts)
# glimpse(case_pivot)

# deaths_pivot <- pivot_func(deaths_ts, valued = "deaths")
# glimpse(deaths_pivot)

# cases_deaths <- inner_join(case_pivot, deaths_pivot, by = c("UID" = "UID", "iso2" = "iso2", 
#                                                             "iso3" = "iso3", "code3" = "code3", 
#                                                             "FIPS" = "FIPS", "Admin2" = "Admin2",
#                                                             "Country_Region" = "Country_Region",
#                                                             "Province_State" = "Province_State", 
#                                                             "Lat" = "Lat", "Long_" = "Long_" , 
#                                                             "Combined_Key" = "Combined_Key", "date_" = "date_" ))
# glimpse(cases_deaths)

# oh <- cases_deaths %>%
#   filter(Province_State == "Ohio")
# Greene <- oh %>%
#   filter(Admin2 == "Greene")
# Greene_april <- Greene %>%
#   filter(date_ >= mdy("4/1/20") & date_ <= mdy("4/30/20"))

# write_csv(Greene_april, "Greene_april_combo.csv")

oh2 <- dailies %>%
  arrange(date_) %>%
  filter(Country_Region == "US" & Province_State  == "Ohio") %>%
  filter(date_ >= mdy("04-12-2020") & date_ <= mdy("05-05-2020"))
glimpse(oh2)

captive1 <- "Source: COVID-19 Data Repository by the Center for Systems Science \n and Engineering (CSSE) at Johns Hopkins University, \n https://github.com/CSSEGISandData/COVID-19,
Accessed May 6, 2020"

ts_plt_func <- function(df, xvar1 = date_, yvar1 = Active, yvar2 = Deaths, xlab = "Date", titled = "Corvid-19 Cases & Deaths in Greene County, OH", 
                        ylab1 = "Cases", ylab2 = "Deaths", captive = captive1, color1 = "blue", color2 = "red"){
  xvar1 <- enquo(xvar1)
  yvar1 <- enquo(yvar1)
  yvar2 <- enquo(yvar2)
  max_y1 <- df %>%
    select(!!yvar1) %>%
    summarize(max = max(.)) %>%
    as.numeric()
  
  max_y2 <- df %>%
    select(!!yvar2) %>%
    summarize(max = max(.)) %>%
    as.numeric()
  
  max_ratio <- max_y1/max_y2
  reciprocal <- 1/max_ratio

  plt1 <- df %>% ggplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.7),legend.title = element_blank()) +
    geom_line(mapping = aes(x = !!xvar1, y = !!yvar1, color = color2)) +  
    geom_line(mapping = aes(x = !!xvar1, y = !!yvar2 *max_ratio, color = color1)) +
    scale_x_date() +  scale_y_continuous(name = ylab1, 
                       sec.axis = sec_axis(~ . * reciprocal, name = ylab2)) +
    scale_color_manual(labels = c(ylab2, ylab1), values = c(color2, color1)) +
    labs(x= xlab, title = titled, caption = captive)
  print(plt1)
} 

plt1 <- ts_plt_func(oh2, titled = "Covid-19 Cases & Deaths in Ohio")
plt2 <- ts_plt_func(oh2, yvar1 = Testing_Rate, yvar2 = Mortality_Rate, ylab1 = "Testing Rate", ylab2 = "Mortality Rate",
            titled = "Covid-19 Testing & Mortality Rates in Ohio")

plt1
plt2
# ggsave(file = "Ohio_cases_morality_counts_April_May2020.png", plot = plt1, path = path3)
# ggsave(file = "Ohio_testing_mortality_rates_April_May2020.png", plot = plt2, path = path3)
