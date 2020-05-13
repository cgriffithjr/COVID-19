library(tidyverse)
library(DT)

navbarPage("Data Selections",
           tabPanel("County Panel",
                    fluidPage(selectInput("ohio_counties",
                                          label = "Select a county",
                                          choices = NULL),
                              plotOutput("plot_01")
                    )
           )
)