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


#create list of LA tables to be read in
la_sheet_list<- list(la_enterprises = "Table 1",la_ent_size = "Table 10",
                     la_turnover = "Table 11", la_industry = "Table 16", 
                     la_lu_size = "Table 22")

#read in all LA table sheets using the map function
la_sheets<- map(la_sheet_list, read_excel,
                path ="./Data/ukbusinessworkbook2017.xls", skip = 5)



# remove the numbers and clean the names from enterprise type date frames
# use the map function to go through selected list objects
la_sheets[c("la_enterprises",
            "la_industry")] <- map(la_sheets[c("la_enterprises",
                                               "la_industry")],
                                   function (x) { 
                                     colnames(x) <- gsub("[[:digit:]]", "", colnames(x))
                                     return(x)
                                   }) %>% map(clean_names) %>%
  map(rename,"la_code" = "x", "la" = "x_2")

## Rename varbales in other columns     
la_sheets[c("la_ent_size",
            "la_turnover",
            "la_lu_size")] %<>% map(rename, "la_code" = "X__1", "la" = "X__2") %>%
  
  map(function(x) { colnames(x)<- tolower(colnames(x))
  return(x)}) %>%
  
  map(function (x) { 
    colnames(x)[4:length(colnames(x))-1] <- paste0("from_"
                                                   , colnames(x)[4:length(colnames(x))-1])
    return(x) 
  })

#create df
df<-  la_sheets[["la_enterprises"]]

#join together
eng<- left_join(eng, df, by = c("ctyua15cd" = "la_code"))

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
                     choices = names(df)[3: length(names(df))],
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
server <- function(input, output, session) {
 
  #To render the dashboard tabs and keep the one you want selected
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Dashboard all enterprises", tabName="dashboard", icon = icon("dashboard")),
      menuItem("Dashboard enterprise size", tabName = "enterpriseSize", icon = icon("database"))
    )
  })
  #isolate({updateTabItems(session, "tabs", "dashboard")})


  #create variable based on column selected
  col_name<- reactive({input$business_type})
  
 #
  eng_update<- eventReactive(input$percentage,{
    eng$percentage<- round(eng[[col_name()]] / sum(eng[[col_name()]], na.rm =TRUE) * 100,2)
    eng
    
  })

    
 # 
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).

    
    #join together
    
      
      ## leaflet plot
    #set colour palette
    total_pal<- colorNumeric(palette = ("Blues"),eng[["total"]])
    
    #create the popup
     pop_up<- paste0("Local authority: ",
                    eng[["la"]],
                   "<br>",
                  "Percent enterprises: ",
                   prettyNum(eng[["total"]], big.mark = ","))
   
      #plot leaflet map
      # [[]] this notation tells R to refer to something in a list, which is part of the tilda transformation
      leaflet(data = eng) %>%
        addTiles() %>%
        addPolygons(popup = pop_up, fillColor = ~total_pal(eng$total), fillOpacity = 6, stroke = FALSE) %>%
        addLegend( pal = total_pal, values = ~eng$total, 
                   title = "Total")

      
    #} # end of if statement for map   
  })# end of renderLeaflet
  
  observe({
    if(input$percentage == TRUE){
      
      #set colour palette
      perc_pal<- colorNumeric(palette = ("Blues"),eng_update()[["percentage"]])
      
      #create the popup
      pop_up<- paste0("Local authority: ",
                      eng_update()[["la"]],
                      "<br>",
                      "Percent enterprises: ",
                      prettyNum(eng_update()[["percentage"]], big.mark = ","))
      
      leafletProxy("map", data = eng_update()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(popup = pop_up, fillColor = ~perc_pal(eng_update()[["percentage"]]), fillOpacity = 6, stroke = FALSE) %>%
        addLegend( pal = perc_pal, values = ~eng_update()[["percentage"]],
                   title = paste("Percent", col_name()))
    
    }
   
    }) # end of observe for percentage true
  
  observe({
    if(input$percentage == FALSE){
      
      #set colour palette
      total_pal<- colorNumeric(palette = ("Blues"),eng[[col_name()]])
      
      #create the popup
      pop_up<- paste0("Local authority: ",
                      eng[["la"]],
                      "<br>",
                      "Total enterprises: ",
                      prettyNum(eng[[col_name()]], big.mark = ","))
      
      leafletProxy("map", data = eng)%>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(popup = pop_up, fillColor = ~total_pal(eng[[col_name()]]), fillOpacity = 6, stroke = FALSE) %>%
        addLegend( pal = total_pal, values = ~eng[[col_name()]], 
                   title = paste("Total", col_name()))
         }
    
  }) # end of observe for percentage false
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

