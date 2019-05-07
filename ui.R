library(shiny)

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")

parkcols=colnames(DF_merged)

shinyUI(fluidPage(
  titlePanel("Parking Explorer"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "dateInput",
        "Which Date?",
        min= "2019-03-25", max = "2019-04-22",
        value = "2019-03-25"
      ),
      sliderInput("hourInput",
                  "Which hour?",
                  min=9, max=17,
                  value=8)
    ),
    mainPanel(
      plotOutput("plot", width = "100%",  height = "700px")
    )
  )
  
))