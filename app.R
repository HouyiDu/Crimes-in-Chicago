#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(ggmap)
library(plotly)
library(rgdal)

#read the shape file
Neighborhoods=readOGR(dsn="community_boundaries",layer = "geo_export_da8f554f-91fe-486b-9ffc-b5a51c4fe348")

# process the data
Crimes_2001_to_present=read.csv("Crimes_-_2001_to_present.csv")
head(Crimes_2001_to_present)
df = data.frame(year = Crimes_2001_to_present$Year, community = Crimes_2001_to_present$Community.Area, type = Crimes_2001_to_present$Primary.Type)
df$count = rep(1,length = nrow(df))


result = aggregate(df$count, by=list(df$community,df$year,df$type), FUN=sum, na.rm=TRUE)
colnames(result) = c("community", "year", "type", "count")
head(result)

allcrime = aggregate(df$count, by=list(df$community,df$year), FUN=sum, na.rm=TRUE)
colnames(allcrime) = c("community", "year", "count")
head(allcrime)

allcrime=allcrime[which(allcrime$community!=0),]



draw_map = function(Neighborhoods,Year,Type){
  if(Type=="TOTAL"){
    bins <- c(0,600,1200,1700,2200,2900,4000,5500,7500,Inf)
    pal <- colorBin("YlOrRd", domain =data.frame(allcrime[which(allcrime$year==Year),])$count , bins = bins)
    m <- leaflet(Neighborhoods) %>% addTiles() %>% addPolygons(fillColor = ~pal(data.frame(allcrime[which(allcrime$year==Year),])$count), 
                                                               weight = 2, 
                                                               smoothFactor = 0.5,
                                                               opacity = 1.0, 
                                                               fillOpacity = 0.7,
                                                               color="white",
                                                               dashArray="3",
                                                               highlightOptions = highlightOptions(color = "#666", 
                                                                                                   weight = 5,
                                                                                                   dashArray="",
                                                                                                   bringToFront = TRUE),
                                                               popup = paste("Number:",Neighborhoods$area_numbe,"<br>","Name:",Neighborhoods$community)) %>%
      addLegend(pal = pal, values = ~data.frame(allcrime[which(allcrime$year==Year),])$count, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    m
  } else{
    bins <- c(0,5,20,80,200,800,1200,Inf)
    pal <- colorBin("YlOrRd", domain =data.frame(result[which(result$year==Year & result$type==Type),])$count , bins = bins)
    
    m <- leaflet(Neighborhoods) %>% addTiles() %>% addPolygons(fillColor = ~pal(data.frame(result[which(result$year==Year & result$type==Type),])$count), 
                                                               weight = 2, 
                                                               smoothFactor = 0.5,
                                                               opacity = 1.0, 
                                                               fillOpacity = 0.7,
                                                               color="white",
                                                               dashArray="3",
                                                               highlightOptions = highlightOptions(color = "#666", 
                                                                                                   weight = 5,
                                                                                                   dashArray="",
                                                                                                   bringToFront = TRUE),
                                                               popup = paste("Number:",Neighborhoods$area_numbe,"<br>","Name:",Neighborhoods$community)) %>%
      addLegend(pal = pal, values = ~data.frame(result[which(result$year==Year & result$type==Type),])$count, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    m
  }
}

#plot the changing of a type of a specific community
typegraph = function(crime, result, comm) {
  presult = prediction(comm, crime, c(2018, 2019, 2020))
  if (crime == "TOTAL") {
    total = aggregate(result$count, by = list(result$year, result$community), FUN = sum, na.rm = TRUE)
    if (comm == 0) {
      tempall = aggregate(total[[3]], by = list(total[[1]]), FUN = sum, na.rm = TRUE)
      ggplot() + geom_line(data = tempall, aes(x = tempall[[1]], y = tempall[[2]])) + geom_point(data = presult, aes(x = year, y = count), color = 'red') + labs(x = "Year") + labs(y = crime) + ggtitle("ALL COMMUNITY ALL CRIME TYPE")
    } else {
      tempcomm = total[total[[2]] == comm,]
      ggplot() + geom_line(data = tempcomm, aes(x = tempcomm[[1]], y = tempcomm[[3]])) + geom_point(data = presult, aes(x = year, y = count), color = 'red') + labs(x = "Year") + labs(y = crime) + ggtitle("ALL CRIME TYPE")
    }
  }
  else {
    temp = result[result$type == crime,]
    if (comm == 0) {
      tempall = aggregate(temp$count, by = list(temp$year), FUN = sum, na.rm = TRUE)
      ggplot() + geom_line(data = tempall, aes(x = tempall[[1]], y = tempall[[2]])) + geom_point(data = presult, aes(x = year, y = count), color = 'red') + labs(x = "Year") + labs(y = crime) + ggtitle("ALL COMMUNITY")
    } else {
      tempcomm = temp[temp$community == comm,]
      ggplot() + geom_line(data = tempcomm, aes(x = year, y = count)) + geom_point(data = presult, aes(x = year, y = count), color = 'red') + labs(x = "Year") + labs(y = crime)
    }
  }
}





#pridict the crime
prediction = function(commp, crimep, yearp) {
  pred_result = result[which(result$year>=2012),]
  if (crimep == "TOTAL") {
    total = aggregate(pred_result$count, by = list(pred_result$year, pred_result$community), FUN = sum, na.rm = TRUE)
    colnames(total) = c("year", "comm", "count")
    if (commp == 0) {
      tempall = aggregate(total$count, by = list(total$year), FUN = sum, na.rm = TRUE)
      colnames(tempall) = c("year", "count")
      model = lm(count ~ year^2, data = tempall)
      dftry = data.frame(year = c(yearp))
      presult = data.frame(yearp, predict(model, newdata = dftry))
      colnames(presult) = c("year", "count")
      presult$year = as.integer(presult$year)
      return(presult)
    } else {
      tempcomm = total[total$comm == commp,]
      tempcomm$comm = as.factor(tempcomm$comm)
      model1 = lm(count ~ year^2, data = tempcomm)
      dftry1 = data.frame(year = c(yearp))
      presult = data.frame(yearp, predict(model1, newdata = dftry1))
      colnames(presult) = c("year", "count")
      presult$year = as.integer(presult$year)
      return(presult)
    }
  } else {
    if (commp == 0) {
      result2 = aggregate(pred_result$count, by = list(pred_result$year, pred_result$type), FUN = sum, na.rm = TRUE)
      colnames(result2) = c("year", "type", "count")
      result2 = result2[result2$type == crimep,]
      model_fit1 = lm(count ~ year^2, data = result2)
      dfpred = data.frame(year = c(yearp))
      presult = data.frame(yearp, predict(model_fit1, newdata = dfpred))
      colnames(presult) = c("year", "count")
      presult$year = as.integer(presult$year)
      return(presult)
    } else {
      result1 = pred_result[which(pred_result$community == commp & pred_result$type == crimep),]
      result1$community = as.factor(result1$community)
      model_fit = lm( count ~ year^2, data = result1)
      dfpre = data.frame(year = c(yearp))
      presult = data.frame(yearp, predict(model_fit, newdata = dfpre))
      colnames(presult) = c("year", "count")
      presult$year = as.integer(presult$year)
      return(presult)
    }
  }
}


# Define UI for application that draws a histogram
ui = shinyUI(
  fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("Crime in Chicago"),
    fluidRow(
      column(3,
             wellPanel(
               selectInput("year",
                           "Choose a year:",
                           choices = c(2001:2017)),
               selectInput("type",
                           "Choose crime type:",
                           choices = append(as.vector(unique(result$type)),"TOTAL",0)),
               selectInput("community_number",
                           "Choose a community:",
                           choices = c(0:77)),
               h6("* 0 represents the entire city"),
               submitButton("Show")
        ),
        wellPanel(
          h6("* The prediction is based on the community and crime type which are selected above(use the data from 2012 to 2017)"),
          h3("Prediction"),
          h4(tableOutput("Pri"))
          )
        ),
    column(9,
           wellPanel(
             h3("Crime Heatmap in Chicago"),
             leafletOutput("View"),
             h3("Quantity of Crime"),
             plotOutput("summary")
             )
           )
    )
  )
)

server = shinyServer(function(input,output){
  active_dataset = reactive({
    input$year
  })
  
  output$View = renderLeaflet({
    draw_map(Neighborhoods=Neighborhoods,Year = active_dataset(),Type = input$type)
  })
  
  output$summary = renderPlot({
    typegraph(crime=input$type, result=result, input$community_number)
  })
  output$Pri = renderTable({
    prediction(commp = input$community_number, crimep=input$type, yearp = c(2018,2019,2020))
  },colnames = FALSE)
})



# Run the application 
shinyApp(ui = ui, server = server)





