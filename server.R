library(tidyverse)
library(lubridate)

path1 <- "C:/Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_daily_reports_us/"
path2 <- "C:/Users/Mark/Documents/COVID-19/data/csse_covid_19_data/csse_covid_19_time_series/"

case_ts <-  read_csv(paste0(path2, "time_series_covid19_confirmed_US.csv"))

deaths_ts <-
  read_csv(paste0(path2, "time_series_covid19_deaths_US.csv"))

# Function to select data by dates and convert format from wide to long
pivot_func <-
  function(df,
           date1 = "1/22/20",
           date2 = "5/5/20",
           named = "date_",
           valued = "cases") {
    ts_pivot <- df %>% pivot_longer(date1:date2,
                                    names_to = named,
                                    values_to = valued)
    ts_pivot$date_ = mdy(ts_pivot$date_)
    return(ts_pivot)
  }

case_pivot <- pivot_func(case_ts)
deaths_pivot <- pivot_func(deaths_ts, valued = "deaths")

cases_deaths <-
  inner_join(
    case_pivot,
    deaths_pivot,
    by = c(
      "UID" = "UID",
      "iso2" = "iso2",
      "iso3" = "iso3",
      "code3" = "code3",
      "FIPS" = "FIPS",
      "Admin2" = "Admin2",
      "Country_Region" = "Country_Region",
      "Province_State" = "Province_State",
      "Lat" = "Lat",
      "Long_" = "Long_" ,
      "Combined_Key" = "Combined_Key",
      "date_" = "date_"
    )
  )

dailies <- list.files(path1, pattern = "[0-9]{2}-[0-9]{2}-[0-9]{4}.csv$", full.names = T) %>%
  set_names(nm = (basename(.))) %>%  # basename gets the filename
  map_df(read_csv, .id = "filename") %>%
  mutate(date_ = mdy(stringr::str_extract(filename, "[0-9]{2}-[0-9]{2}-[0-9]{4}" )))

# glimpse(dailies)

rm(case_ts, deaths_ts, case_pivot, deaths_pivot)


oh <- cases_deaths %>%
  filter(Province_State == "Ohio") %>%
  filter(date_ >= mdy("4/1/20") & date_ <= mdy("4/30/20"))

function(input, output, session) {
  
  
  
  updateSelectInput(session, 
                    "ohio_counties",
                    choices = unique(oh$Admin2))
  
  
  output$plot_01 <- renderPlot({
    oh %>%
      filter(Admin2 == input$ohio_counties) %>% 
      ggplot(aes(x = date_, y = cases)) + geom_line() +
      scale_y_continuous() + scale_x_date() + labs(x = "Date", y = "Cases", title = "April 2020")
    
  })
  
  updateSelectInput(session,
                    "select_state",
                    choices = unique(dailies$Province_State)
                    )
 
 
  output$state_data_chart <- renderPlot({
    dailies %>%
      filter(Province_State  == input$select_state) %>%
            
      ggplot(aes(x = date_, y = !!input$select_variable)) +
      geom_line() 
  })
}
