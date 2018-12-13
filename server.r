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

shinyServer(function(input,output){
  
  sliderDate = reactiveValues()
  
  observe({
    sliderDate$date1 = datetransfrom(input$month[1])
    sliderDate$date2 = datetransfrom(input$month[2])
  })
  
  map.data<- reactive({
    df[df$primary.description %in% input$crimetype & df$month >= sliderDate$date1 & df$month <= sliderDate$date2 &df$zipcode %in% input$zipcode, ] %>% 
      group_by(latitude,longitude,location.description,primary.description,block,month,zipcode,time) %>% summarise(count=n())
  })
  
  #giving the instrution to the variable "mymap"
  output$mymap <- renderLeaflet({
    
    isolate({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
        fitBounds(min(map.data()$longitude), min(map.data()$latitude), max(map.data()$longitude), max(map.data()$latitude))
      #fitBounds(min(map.data()$latitude),min(map.data()$longitude),max(map.data()$latitude),max(map.data()$longitude))
    })
  })
  
  # output$mymap <- renderLeaflet({
  #   # Use leaflet() here, and only include aspects of the map that
  #   # won't need to change dynamically (at least, not unless the
  #   # entire map is being torn down and recreated).
  #   leaflet(df) %>% addTiles() %>%
  #     fitBounds(~min(df$longitude), ~min(df$latitude), ~max(df$longitude), ~max(df$latitude))
  # })
  
  observe({
    leafletProxy("mymap") %>% clearShapes() %>% addCircles(radius = map.data()$count*40, map.data()$longitude, map.data()$latitude, color = pal(map.data()$primary.description),fillOpacity = 90, popup = paste("Location: ",map.data()$location.description,"time :",map.data()$time)) %>%
      clearControls() %>% addLegend("topright",pal=pal,values=map.data()$primary.description)
  })
  
  #chart 1
  
  plot.data.1 =   reactive({
    df[df$primary.description %in% input$crimetype & df$zipcode %in% input$zipcode,] %>% group_by(month,zipcode) %>% summarise(count = n())
  })
  
  output$plot1 = renderPlot({
    p = ggplot(plot.data.1(),aes(x = month,y = count,group = zipcode,colour = zipcode,alpha = ifelse(month >= sliderDate$date1 & month <= sliderDate$date2,1,0.5)))
    p = p + geom_line(stat = "identity",size = 2) 
    p = p + tidy + scale_color_brewer(palette = "Set1") + guides(alpha = FALSE) 
    p = p + theme(legend.position = "top",legend.key = element_blank(),legend.text = element_text(size = 15),legend.title = element_blank()) + ylab ("Count - All Crimes")
    print(p)
  })
  
  #chart 2
  
  plot.data.2 =   reactive({
    df[df$month >= sliderDate$date1 & df$month <= sliderDate$date2 & df$zipcode %in% input$zipcode,] %>% group_by(primary.description,zipcode) %>% summarise(count = n()) %>% melt() %>% arrange(desc(value))
  })
  
  
  output$plot2 = renderPlot({
    p = ggplot(plot.data.2(),aes(x = reorder(primary.description,value),y = value,fill = zipcode,alpha = ifelse(primary.description %in% input$crimetype,1,0.5))) 
    p = p + geom_bar(stat = "identity",position = "dodge") + guides(fill = FALSE) + guides(alpha = FALSE)  + coord_flip() 
    p = p + tidy + scale_fill_brewer(palette = "Set1") + xlab ("Crime Type")
    print(p) 
    })
})