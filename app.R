#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(htmltools)
library(leaflet.extras)
library(priceR)
library(crosstalk)



rates <- exchange_rate_latest('USD')%>%
  rename(Currency = currency)

selected_columns <- c('Name','Longitude','Latitude','Location','Award','Cuisine','PhoneNumber','Address','Url','MinPrice','Currency','WebsiteUrl')
columns_drop_with_na <- c('Name','Longitude','Latitude','Location','Award','Cuisine','Address','Url','MinPrice','Currency')




michelin <- read_csv('michelin_my_maps.csv')%>%
  select(selected_columns)%>%
  drop_na(columns_drop_with_na)%>%
  left_join(rates)%>%
  mutate(usd_minprice = round(MinPrice/one_usd_is_equivalent_to ))%>%
  mutate(cuisines = as.list(strsplit(Cuisine,split=', ')))


ct_michelin <- SharedData$new(michelin, key = ~Name)

filter_data <- function(data,mstars,pricerange){
  data%>%
    
    
    filter(Award %in% mstars)%>%

    #mutate(selected = sum(input$cuisines %in% unlist(cuisines)))%>%

    filter(usd_minprice>=pricerange[1] & usd_minprice<=pricerange[2])%>%
    mutate(label = paste('Restaurant: ', Name,
                         '<br>Award: ', Award,
                         '<br>Cuisine Type: ', Cuisine,
                         '<br>Minimum Spend (USD): $', usd_minprice))%>%
    mutate(icon_color = case_when(
      Award == '3 MICHELIN Stars' ~ "orange",
      Award == '2 MICHELIN Stars' ~ "purple",
      Award == '1 MICHELIN Star' ~ "blue",
      Award == 'Bib Gourmand' ~ "green"))
}

cuisine_list <- pull(michelin, cuisines) %>%
  unlist()%>%
  unique()%>%
  sort()

award_list <- pull(michelin, Award) %>%
  unique()

icon_color_func <- function(df){
  awesomeIcons(
    icon = 'nothing',
    iconColor = 'black',
    markerColor = df$icon_color
  )}


mapping_func <- function(df){
  leaflet(df, options = leafletOptions(worldCopyJump = TRUE, minZoom = 1))%>%  
    addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions())%>%
    addAwesomeMarkers(popup = paste('Restaurant: ', df$Name,
                             '<br> Minimum Spend (Local Currency): ', df$MinPrice,df$Currency,
                             '<br> Phone: ', df$PhoneNumber,
                             '<br> Michelin Review: ', "<a href='",df$Url,"'>",'Link',"</a>",
                             '<br> Restaurant Website: ', "<a href='",df$WebsiteUrl,"'>",'Link',"</a>",
                             '<br> Google Map Directions: ', "<a href='https://www.google.com/maps/dir/?api=1&destination=",df$Latitude,"%2C",df$Longitude,"'>",'Link',"</a>"
                             ),
               label = lapply(df$label, htmltools::HTML),
               icon = icon_color_func(df),
               clusterOptions = markerClusterOptions(maxClusterRadius=20)
               )%>%
    addFullscreenControl()%>%
    addMiniMap(toggleDisplay = TRUE)%>%
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
    addLegend(colors =c("orange","purple","blue","green"),
              labels = c("3 Stars","2 Stars","1 Star","Bib Gourmand"),
              title = "Restaurant Award",
              opacity = 0.8)
}




ui <- fluidPage(

    
    titlePanel("2021 Michelin Restaurants"),
    fluidRow(
      
    ),
    sidebarLayout(
        sidebarPanel(
         # textOutput("debug"),
          checkboxGroupInput("mstars", "Michelin Stars", award_list, award_list),
          sliderInput("pricerange", label = "Min Price Range (USD)", min = 0, max = max(michelin$usd_minprice), value = c(0, max(michelin$usd_minprice))),
          actionLink("selectall","Clear All / Select All") ,
          selectInput("cuisines", "Cuisine", cuisine_list, cuisine_list,multiple = TRUE),
          
        ),

        mainPanel(
          p('Welcome to my Michelin Starred restaurants of 2021 visualization, feel 
            free to look at some of the award-receiving restaurants by panning around 
            the map using left click, and scrolling to zoom in/out.'),
          
          p('You can further refine your search for the ultimate culinary experience 
            using some of the interactivity options to the left. There are selections 
            for 1-3 Michelin stars, or Bib Gourmand, a more budget friendly option. 
            Underneath, theres a slider for price range, and further below that is 
            a selection box for cuisine styles. If you know the name of the restaurant, 
            you can search in the datatable below and click it to select it or multiple.'),
          
          p('Once you find the restaurant(s) you want to visit, you can click the 
            marker to open up more info, such as expected spend in local currency
            and links, including one that will show the route from your location
            to the restaurant.'),
          
          p('On the map, you will find a reset zoom button that looks like a globe, 
            a crosshair button that zooms into your location, and a minimap to 
            quickly navigate the globe.'),
          
          p('Enjoy your stay! and I hope you find what you\'re looking for!'),
          leafletOutput("map"),
          DTOutput('dt')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactive({
      michelin%>%

        rowid_to_column('index')%>%
        group_by(index)%>%
        filter(Award %in% input$mstars)%>%
        mutate(summed = sum(input$cuisines %in% unlist(cuisines))>0)%>%
        #mutate(selected = sum(input$cuisines %in% unlist(cuisines)))%>%
        filter(summed==TRUE)%>%
      filter_data(input$mstars,input$pricerange)
    })
    
    ct_michelin <- SharedData$new(data, key = ~Name)
    
    output$map <- renderLeaflet({
      mapping_func(ct_michelin$data(withSelection = TRUE)%>%
                     filter(selected_ | is.na(selected_))%>%
                     mutate(selected_ = NULL))
    })
    
    output$dt <- renderDT({
      datatable(ct_michelin,width = "100%", options=list(columnDefs = list(list(visible=FALSE, 
                                                                 targets=c('Longitude','Latitude','Location','Award','PhoneNumber','Url','MinPrice','Currency','WebsiteUrl','index','one_usd_is_equivalent_to','usd_minprice','cuisines','summed','label','icon_color')))))
    }, server = FALSE)
    
    observe({
      if(input$selectall > 0){
        
        
        if (input$selectall%%2 == 0)
        {
          updateSelectInput(session,"cuisines","Choose Cuisine(s):",choices=cuisine_list,selected=cuisine_list)
        }
        else
        {
          updateSelectInput(session,"cuisines","Choose Cuisine(s):",choices=cuisine_list,selected=c())
        }
      } 
    })
    
    #output$debug <- renderText({
    #  paste0(typeof(input$cuisines))
    #})
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
