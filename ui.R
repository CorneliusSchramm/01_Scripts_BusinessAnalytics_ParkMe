library(shiny)

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")

parkcols=colnames(DF_merged)

shinyUI(fluidPage(
  navbarPage("ParkMe",
             tabPanel("Make & Explore Predictions",
                      tabsetPanel(
                        tabPanel("Where Do I want to go?",
                                 sidebarLayout(
                                   sidebarPanel("Please find the area number that corresponds to where you would like to park your car and proceed to the 'Give me a prediction' section."),
                                   mainPanel(
                                     plotOutput("plotW2Go", width = "100%",  height = "900px"))
                                 )),
                        tabPanel("Give me a prediction",
                                 sidebarLayout(
                                   sidebarPanel(
                                     numericInput(
                                       "clusterInput",
                                       "Which Area Number?",
                                       min = 1, max = 30,
                                       value= 1
                                     ),
                                     dateInput(
                                       "dateInput",
                                       "Which Date?",
                                       min= "2019-04-16", max = "2019-04-22",
                                       value = "2019-04-16"
                                     ),
                                     sliderInput("hourInput",
                                                 "Which hour?",
                                                 min=9, max=17,
                                                 value=8)
                                   ),
                                   mainPanel(
                                     textOutput("ClusterPred"),
                                     plotOutput("plotPred", width = "100%",  height = "900px")
                                   )
                                 ))
                      )),
             tabPanel("Explore Historic Data",
                      sidebarLayout(
                        sidebarPanel(
                          dateInput(
                            "dateInputH",
                            "Which Date?",
                            min= "2019-03-25", max = "2019-04-22",
                            value = "2019-03-25"
                          ),
                          sliderInput("hourInputH",
                                      "Which hour?",
                                      min=9, max=17,
                                      value=8)
                        ),
                        mainPanel(
                          plotOutput("plot", width = "100%",  height = "900px")
                          #textOutput("please")
                        )
                      ))
  )
))



