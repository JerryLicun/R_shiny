# Using some libraries
library(tidyr)
library(dplyr)
library(shiny)
library(leaflet)
library(ggplot2)

# Read the Data & Define some variables
data_location <- read.csv("Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")
data_count   <-  read.csv("Pedestrian_Counting_System_2019 (Exercise 2).csv")

data_location$average_count <- 0
data_location_len <- nrow(data_location)
#---------------------------------------------------------------------------

# Visual the data 
# To find the unique data of the sensor name in two csv file.
a <- unique(data_count$Sensor_Name) %>% data.frame()
b <- unique(data_location$sensor_name) %>% data.frame()

Total_sensor <-rbind(a,b) %>% unique()
#---------------------------------------------------------------------------

# Data Wrangling

# Change the Sensor name in two csv files to the same format
data_count$Sensor_Name[which(data_count$Sensor_Name =="Pelham St (S)")]  <- "Pelham St (South)"
data_count$Sensor_Name[which(data_count$Sensor_Name =="Lincoln-Swanston(West)")]  <- "Lincoln-Swanston (West)"
data_count$Sensor_Name[which(data_count$Sensor_Name =="Flinders la - Swanston St (West) Temp")]  <- "Flinders La - Swanston St (West) Temp"

for ( i in 1:data_location_len){
  dat<- data_location[i,]
  if (dat$sensor_name == "Building 80 RMIT"){
    data_location[i,]$sensor_name= "Swanston St - RMIT Building 80"
  }
  if (dat$sensor_name == "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)"){
    data_location[i,]$sensor_name= "Melbourne Central-Elizabeth St (East)"
  }
  if (dat$sensor_name == "RMIT Building 14"){
    data_location[i,]$sensor_name= "Swanston St - RMIT Building 14"
  }
  
  if (dat$sensor_name == "Collins Street (North)"){
    data_location[i,]$sensor_name= "Collins St (North)"
  }
  if (dat$sensor_name == "Flinders La - Swanston St (West) Temporary"){
    data_location[i,]$sensor_name= "Flinders La - Swanston St (West) Temp"
  }
}

#---------------------------------------------------------------------------

# Calculate the average hourly count over the entire year of 2019 
# and save it in data_location
for ( i in 1:data_location_len){
  da<- data_location[i,]
  name<- da$sensor_name
  
  filter_name <- filter(data_count, data_count$Sensor_Name == name)
  data_location[i,]$average_count <- sum(filter_name$Hourly_Counts)/nrow(filter_name)
}

# Delete the NULL values
data_location <-na.omit(data_location)
data_location_len <- nrow(data_location)

unique_sensor_name <- unique(data_count$Sensor_Name)
#--------------------------------------------------------------------------------------

# UI design
ui <- fluidPage(
  # Map display
  leafletOutput("myMap"),
  
  # Select bar display
  sidebarLayout(sidebarPanel = sidebarPanel(
    selectInput("MySlider",
                label = "Choose you Input:",
                choices = unique_sensor_name
                )
  ),
  # Line graph
  mainPanel = mainPanel(
    plotOutput("myPlot")
  )
  )
)
#---------------------------------------------------------------------------------------

#Server Design
server <- function(input, output, session) {
  
  # Map design: Using circle markers and display the sensor name
  output$myMap <- renderLeaflet(
    {
      leaflet(data = data_location) %>% addTiles() %>%
        addMarkers(~longitude, ~latitude, label = ~as.character(sensor_name)) %>%
        addCircleMarkers(~longitude, ~latitude,data_location$average_count/50)
    }
  )
  
  # Create a variate(n1) to save sensor name was been clicked
  n1 <- reactive({
    (data_location$sensor_name[which(data_location$latitude==input$myMap_marker_click[[3]])])
  })

  # Using variate(n1) to update the selectInput
  observe({
    updateSelectInput(session, "MySlider",choices = unique_sensor_name,
                      selected = n1()
    )})
  
  # Create a faceted line chart
  output$myPlot <- renderPlot({
    # Ordered the day of the week appropriately
    neworder <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    
    filter_data <- filter(data_count, data_count$Sensor_Name == input$MySlider) 
    filter_data <- arrange(transform(filter_data,
                                     Day=factor(Day,levels=neworder)),Day)
    
    g<- ggplot(filter_data, aes(Time, Hourly_Counts)) + ylab("Average pedestrian count") +
      stat_summary(fun=mean, geom = "line") +facet_grid(~Day)
    g
  })
}

shinyApp(ui, server)
