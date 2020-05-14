library(tidyverse)
library(DT)

# navbarPage("Data Selections",
#            tabPanel("Ohio Counties",
#                     fluidPage(selectInput("ohio_counties",
#                                           label = "Select a county",
#                                           choices = NULL),
#                               plotOutput("plot_01")
#                     )
#            )
# )

navbarPage("Covid-19 Data Selections",
           tabPanel("Ohio Counties",
                    fluidPage(selectInput("ohio_counties",
                                          label = "Select a county",
                                          choices = NULL),
                                                  plotOutput("plot_01")
                                        )
                               ) ,
           tabPanel("State",
                    fluidPage(
                      fluidRow(
                        column(
                          selectInput("select_state",
                                      label = "Select a State",
                                      choices = NULL
                                      ),
                          width = 6
                          ),
                        column(varSelectInput("select_variable",
                                           label = "Select an Indicator",
                                           selected = "Active",
                                           dailies,
                                           width = "100%"
                                           ),
                               width = 6
                               ),
                        )
                      ,
                      plotOutput("state_data_chart")
                      )
                    )
           )