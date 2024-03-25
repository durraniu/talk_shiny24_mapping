library(shinydashboard)
library(ggplot2)
library(dplyr)


# Global------------------------------------------------------------------------

## Connect to redis database
R <- redux::hiredis()


## Load the road network
load("data/df.Rda")


## Load the complex route
load(file = "data/pos_df.Rda")


## Load Music files
addResourcePath("Music", "Music")
audio_file1 <- "Music/l1.mp3"



# User Interface ---------------------------------------------------------------
ui <- dashboardPage(skin = "black", 
                    dashboardHeader(title = "Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                       menuItem("Music", tabName = "music", icon = icon("music")),
                       menuItem("Maps", tabName = "navigation", icon = icon("compass"))
                      )),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "navigation",
                                fluidRow(
                                  tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
                                  plotOutput("plot1")
                                ),
                                
                                fluidRow(
                                  column(width = 8, valueBoxOutput("text"),
                                         tags$style("#text {width:600px;}"), 
                                         tags$style(".small-box.bg-yellow { background-color: #ecf0f5 !important; color: #000000 !important; }")),
                                  column(width = 2, imageOutput("myImage"))
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "music",
                                h2("Music"),
                                fluidRow(
                                  tags$audio(src = audio_file1, type = "audio/mp3", autoplay = FALSE, controls = TRUE)
                                )    
                                  
                        )
                      )
                    )
)


# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  
  ## Get the live position data
  
  timer <- reactiveTimer(5)
  
  
  ### Getting the position table every 1 sec
  position <- reactive({
    
    timer()
    
    popped <- R$RPOP("position")
    
    jsonlite::fromJSON(popped) 
    
  })
  
  

  ## Plot
  
  output$plot1 <- renderPlot({
    
    ### Get the zoomed-in surroundings of the position
    xl1 <-  position()$ED_x - 500
    xl2 <-  position()$ED_x + 500
    
    yl1 <-  position()$ED_y - 300
    yl2 <-  position()$ED_y + 300
    
    
    ggplot() +
      geom_path(data = df,
                aes(x, -y,
                    group = interaction(elem_idx, path_idx)),
                color = "grey50") +
      
      geom_path(data = pos_df, mapping = aes(ED_x, ED_y),
                color="skyblue", size = 2, alpha = 0.6) +
      geom_point(data = position(),
                 aes(ED_x, ED_y),
                 fill = "#4285F4", 
                 color = "white", 
                 size = 5, 
                 pch=21,
                 stroke = 3) +
      coord_equal(    xlim = c(xl1, xl2),
                      ylim = c(yl1, yl2)) +
      theme_void()
    
  })
  

  
  
  
  ## Generate a message
  msg <- reactive({
    
    
    
    if (position()$ED_x > 31307 & position()$ED_x < 31392 & position()$ED_y > -4050 & position()$ED_y < -2925 ) {
      
      msg <- "Turn right at the next intersection"
    
    
    } else if (position()$ED_x > 34569.43 & position()$ED_x < 35314.04 & position()$ED_y < -2950 ) {
      
      msg <- "Turn left at the next intersection"
      
      
      
      
      
    } else if (position()$ED_x > 35936 & position()$ED_x <  36004 & position()$ED_y > 250.668 & position()$ED_y < 986.373 ) {
      
      msg <- "Turn right at the next intersection"
      
      
      
      
      
    } else if (position()$ED_x > 37620 & position()$ED_x < 38615 & position()$ED_y < 1160) {
      
      msg <- "Turn left at the next intersection"
      
      
      
      
    } else if (position()$ED_x > 39120 & position()$ED_x < 39336  & position()$ED_y > 908 & position()$ED_y < 2282.4727 ) {
      
      msg <- "Turn right at the next intersection"
      
      
      
      
    } else if (position()$ED_x > 39840 & position()$ED_x < 40603.00 & position()$ED_y > 2282 & position()$ED_y < 2363) {
      
      msg <- "Turn left at the next intersection"
      
      
      
      
    } else if (position()$ED_y > 5390 & position()$ED_y < 6261.8098) {
      
      msg <- "Turn left and park in the parking lot"
    
      
      } else {
      
      msg <- "Continue straight"
    }
    
    # print(msg)
    
    
    
  })
  
  
  ## Show the message
  output$text <- renderValueBox({
    
    
    valueBox(
      value = div(tags$p(msg(), style = "font-size: 90%;"),
                  tags$p(paste0(position()$ED_x, ", ", position()$ED_y))),
      subtitle = "",
      color =  "yellow",
      width = NULL
    )
  })

  
  
  
  
 
  
  
  ## Show the image
  output$myImage <- renderImage({
    
    if (msg() %in% c("Turn right",
                     "Turn right at the next intersection")) {
      
      return(
        
        # Generate the PNG
        list(src = "turn_right.png",
             width = 100,
             height = 100)
        
      )
    } else if (msg() %in% c("Turn left at the next intersection",
                            "Turn left",
                            "Turn left and park in the parking lot")) {
      
      
      
      
      return(
        
        # Generate the PNG
        list(src = "turn_left.png",
             width = 100,
             height = 100)
        
      )
    } else {
      
      return(
        
        # Generate the PNG
        list(src = "continue.png",
             width = 100,
             height = 100)
        
      )
      
    }
    
  }, deleteFile=FALSE)
  
}

shinyApp(ui, server)