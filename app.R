#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(sf)
library(tidyverse)
library(magrittr)
library(janitor)

#read in spatial data
eng_wales<- st_read(dsn = "./Data", layer = "Counties_and_Unitary_Authorities_December_2015_Generalised_Clipped_Boundaries_in_England_and_Wales")

#filter to just show england
eng <-  eng_wales %>% filter(objectid < 153)


#read in excel business data
all_enterprises<- read_excel("./Data/ukbusinessworkbook2017.xls", sheet = "Table 1", skip = 5)

#remove the digits in their
names(all_enterprises) <- gsub("[[:digit:]]", '', names(all_enterprises), fixed = FALSE)

#use the clean names ifrom the janitor package to remove spaces, colons etc
#rename the variables beginning with x
all_enterprises %<>% clean_names() %>% rename("la_code" = "x", "la" = "x_2")

#join together
eng<- left_join(eng, all_enterprises, by = c("ctyua15cd" = "la_code"))

# transform coordinates
eng<- sf::st_transform(eng, crs = 4326)

##employment data to be added ---------


# Define UI for application -------------
ui <- dashboardPage(skin = "red",
  
  dashboardHeader(title = "Business data 2017"),
  
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu") # we use this for the server to render the dashboard tabs there - we can select by default that way
    )
  ),
  
  dashboardBody(
    tabItems(
      
    tabItem(tabName = "dashboard", selected = TRUE,
  
fluidPage(
   
   # Application title
   titlePanel("Number of VAT or PAYE enterprises"),
   
   # Sidebar with a selectInput 
   sidebarLayout(
      sidebarPanel(
         selectInput("business_type",
                     "Select enterprise type:", 
                     choices = names(all_enterprises)[3: length(names(all_enterprises))],
                     selected = "total"
                      ),
         
         checkboxInput("percentage", label = "Show map as percentages", value = FALSE )
       ), # end of sidebarPanel
      
      # Show a plot of the map
      mainPanel(
         leafletOutput("map")
      )# end main panel
   )# end side panel
  )# end fluid page
    ),#end of tabItem1

  tabItem(tabName = "enterpriseSize",
          
          fluidPage(
            
            h4("To be updated")
            
          )# end of fluid page tabItem 2
    
  )#end of tabItem 2
    )# end of tabItems
 )# dashboard body
)# End of dashboard page

# Define server logic required to create maps
server <- function(input, output,session) {
   
  #To render the dashboard tabs and keep the one you want selected
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Dashboard all enterprises", tabName="dashboard", icon = icon("dashboard")),
      menuItem("Dashboard enterprise size", tabName = "enterpriseSize", icon = icon("database"))
    )
  })
  #isolate({updateTabItems(session, "tabs", "dashboard")})

  
  col_name<- reactive({input$business_type})
  
 # map_reactive<- reactive({
  

    
 # }) # end of map reactive
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    if(input$percentage == TRUE){
      
      eng$percentage <- round(eng[[col_name()]] / sum(eng[[col_name()]], na.rm =TRUE) * 100,2)
      
      #set colour palette
      perc_pal<- colorNumeric(palette = ("Blues"),eng$percentage)
      
      #create the popup
      pop_up<- paste0("Local authority: ",
                      eng$la,
                      "<br>",
                      "Percent enterprises: ",
                      prettyNum(eng$percentage, big.mark = ","))
      
      leaflet("map", data = eng) %>%
        addTiles() %>%
        addPolygons(popup = pop_up, fillColor = ~perc_pal(eng$percentage), fillOpacity = 6, stroke = FALSE) %>%
        addLegend( pal = perc_pal, values = ~eng$percentage,
                   title = paste("Percent", col_name()))
      
      
    } else {
      
      ## leaflet plot
      
      #set colour palette
      total_pal<- colorNumeric(palette = ("Blues"),eng[[col_name()]])
      
      #create the popup
      pop_up<- paste0("Local authority: ",
                      eng$la,
                      "<br>",
                      "Total enterprises: ",
                      prettyNum(eng[[col_name()]], big.mark = ","))
      
      
      #plot leaflet map
      # [[]] this notation tells R to refer to something in a list, which is part of the tilda transformation
      leaflet("map", data = eng) %>%
        addTiles() %>%
        addPolygons(popup = pop_up, fillColor = ~total_pal(eng[[col_name()]]), fillOpacity = 6, stroke = FALSE) %>%
        addLegend( pal = total_pal, values = ~eng[[col_name()]], 
                   title = paste("Total", col_name()))
      
    } # end of if statement for map   
  })
  
     #observe({
      # generate bins based on input$bins from ui.R
     #leafletProxy("map", en())
  
   #}) # end of renderLeaflet
}

# Run the application 
shinyApp(ui = ui, server = server)

