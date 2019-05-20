library(shiny)
library(rsconnect)
library(ggmap)
#Get map of Seattle via the Google API
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs") # Für Seattle map
load("../02_Business_Analytics_Data/df_set_02_merged.RData") # Für history plot
load("../02_Business_Analytics_Data/locationsToPlot.RData") # Für Blubberblasenplot
load("../02_Business_Analytics_Data/4SHINY.RData") # Für Preds und Preds plot

loc2 = data.frame(locations[!duplicated(locations[,"cluster"]),][,])
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
                                                 value=8, animate = T)
                                   ),
                                   mainPanel(
                                     h3(textOutput("ClusterPred")),
                                     br(),
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



