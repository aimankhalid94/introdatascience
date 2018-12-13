library(shiny)
library(leaflet) #https://rstudio.github.io/leaflet/raster.html
library(dplyr)
library(gdata)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(DT)
library(funModeling)
library(reshape2)

df<-read.csv("withzipcode.csv")
df$zipcode<-as.factor(df$zipcode)
df$month<-df$month %>% as.Date("%Y-%m-%d")
crime<-as.list(unique(df$primary.description))
zipcode<-as.list(unique(df$zipcode))

# Negeri<-as.list(unique(df$NEGERI))

##color for maps
cols = brewer.pal(8,"Set2")
cols = colorRampPalette(cols)(df$primary.description %>% unique() %>% length())
pal = colorFactor(cols,unique(df$primary.description))

##slider
datetransfrom = function(x){
  x = as.POSIXlt(x)
  x$mday = "01"
  x = as.Date(x)
}

tidy = theme(panel.background = element_blank(), axis.line = element_line("grey"),axis.ticks = element_blank())
shinyUI(fluidPage(
  #Print function
  fluidRow(
    column(4,"Jenayah Map",style="font-size:36pt"),
    column(2,sliderInput(inputId = "month", label = "Month", min = min(df$month), max = max(df$month), value = c(as.Date("2018-01-01") ,max(df$month)), timeFormat = "%b %Y")),
    column(2,selectInput(inputId = "crimetype", label = "Crime Type", choices = crime,selected = "MOTOR VEHICLE THEFT",multiple = T)),
    column(2,selectInput(inputId = "zipcode", label = "zipcode", choices = zipcode,selected = "60616",multiple = T))
  ),
  fluidRow(
    column(7,leafletOutput("mymap",height=800)),
    fluidRow(
      column(5,plotOutput("plot1")),
      column(5,plotOutput("plot2"))
    )
  )
)
)

