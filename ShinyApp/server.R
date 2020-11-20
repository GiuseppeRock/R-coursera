library(shiny)
library(dplyr)
library(quantmod)
library(leaflet)
library(plotly)
library(tibble)
library(webshot)
library(curl)
options(scipen=999)

dataload <- function() {
  destfile <- "50_best_restaurants_database.txt"
  fileurl <- "http://download1650.mediafire.com/bdx6umcizbkg/s5w876ee6h6ckqz/50+best+restaurants+database.txt"
  fileurl2 <- "http://download1326.mediafire.com/czkusv8im2jg/3xzg45r19bo8oi9/icones.txt"
  if(!file.exists(destfile)){
    download.file(fileurl, "50_best_restaurants_database.txt")
    download.file(fileurl2, "icons.txt")}
  restaurants <<- read.delim("50_best_restaurants_database.txt", na = "NA")
  icones <<- read.table("icons.txt", quote="\"", comment.char="")
}
dataload()


# popups for map

popups <- function() {
  items <<- NULL
  for (i in 1:50) {
    first <- paste("<b>#", restaurants$Ranking[i], "-", restaurants$Restaurant[i], if (!is.na(restaurants$Stars[i])) {strrep("*", restaurants$Stars[i])}, "</b></br>")
    second <- paste(restaurants$City[i], "-", restaurants$Country[i], "</br>")
    third <- paste(restaurants$Description[i], "</br>")
    fourth <- paste("Chef:", restaurants$Chef[i], "</br>")
    fifth <- paste("Website: <a href =", restaurants$Website[i], ">", restaurants$Website[i], "</a>")
    item <- paste(first, second, third, fourth, fifth)
    items <<- c(items, item)
  }}
popups()

# Exchange rates

# currencies

chartrend <- function() {
  exclist = NULL
  for (i in unique(restaurants$Currency)) {
    exc <- paste("EUR",i, sep = "/")
    exclist <- c(exclist, exc)
  }
  exrates = NULL
  for (i in exclist) {
    rate <- tail(getSymbols(Symbols = i, src = "oanda", from = Sys.Date()-5, auto.assign = FALSE),1)
    exrates <- c (exrates, rate)
  }
  extable <<- data.frame(currency = exclist, rate = exrates)
  
  menu_curr = c()
  
  for (i in 1:50) {
    if (restaurants$Currency[i] == "EUR") {
      menu_curr <- c(menu_curr, restaurants$Menu[i])
    }
    else {
      rateind <- which(extable$currency == paste("EUR", restaurants$Currency[i], sep = "/"))
      menu_curr <- c(menu_curr, round(restaurants$Menu[i] / extable$rate[rateind]))
    }
  }
  restaurants_new <<- restaurants %>% mutate(menu_loc = menu_curr)
  
  #plot
  chart <<- with(restaurants_new, plot_ly ( 
    x = Ranking, y = menu_curr, type = "scatter", mode = "markers", 
    color = as.factor(Stars), marker = list(size = 12, color = "pal")) 
    %>%
      layout(title = "50 best restaurants 2019", 
             xaxis = list(title = "Ranking"), 
             yaxis = list(
               title = "Full Tasting Menu Price (EUR)"),
             legend=list(title=list(text='Michelin stars')))
    %>% add_trace(text = restaurants_new$Restaurant, showlegend = F))
}

chartrend()


# Define server logic required to draw the map

shinyServer(function(input, output) {
  
  mapr <- reactive ({
      rmin = input$slider[1]
      rmax = input$slider[2]
      icone = list(iconUrl = icones$V1[rmin:rmax], iconWidth = 30, iconHeight = 30)
      mappar <<- restaurants %>% leaflet() %>% addTiles() %>% addMarkers(lng = restaurants$Lon[rmin:rmax], lat = restaurants$Lat[rmin:rmax], popup = items[rmin:rmax], icon = icone, clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
      mappar
      })
  

  
  output$map <- renderLeaflet({mapr()})
  
  output$chart <- renderPlotly(chart)
  
})