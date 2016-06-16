###server
library(shiny)
library(datasets)
library(RMySQL)
library(DT)
library(dygraphs)
library(lubridate)
library(googleVis)
library(metricsgraphics)
library(RJSONIO)
library(xts)
library(timeDate)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(reshape2)
######
library(tsDyn)
library(fGarch)
library(zoo)
library(tseries)
library(forecast)
########
datatable(iris)
Logged = FALSE;
US = NULL;

shinyServer(function(input, output, session) {
  
  source("path/Login.R",  local = TRUE)
  
  observe({
    if (USER$Logged == TRUE) {
      Username <- RUTA$US
      consulta =paste("SELECT ruta FROM usuarios WHERE user = '",Username, sep="","';")
      resp=dbGetQuery(con,consulta)
      x<-as.character(resp)
      
      source(x, local = TRUE)
      
      input$Login
      progress <- shiny::Progress$new()
      progress$set(message = "Espere Por Favor", value = 0)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      compute_data(updateProgress)
    }
  })
  
})

compute_data <- function(updateProgress = NULL) {
  dat <- data.frame("1,2,3,4,5,6,7,8,9,10")
  
  for (i in 1:10) {
    Sys.sleep(0.35)
    resultado = i * 10 
    if (is.function(updateProgress)) {
      text <- paste0(":",resultado,"%")
      updateProgress(detail = text)
    }
    dat <- rbind(dat, i)
  }
  dat
}