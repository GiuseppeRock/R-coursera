library(shiny)
library(dplyr)
library(quantmod)
library(leaflet)
library(plotly)
library(tibble)
library(webshot)
library(curl)
options(scipen=999)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("World best 50 restaurants"),
  
  # Sidebar with a slider input for number of restaurants
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider",
                  "Ranking:",
                  min = 1,
                  max = 50,
                  value = c(1,50))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Price Chart", plotlyOutput("chart"))
)))))