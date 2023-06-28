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


Permits_MSA= read.csv("https://www2.census.gov/econ/bps/Metro/ma2305y.txt",skip=1)
FinalTable_MSA <- Permits_MSA[,5:17]
FipsToState <- read_csv("Data/FipsToState.csv")
# Prepping the Permits table from the census
colnames(FinalTable_MSA)<-c("Area", 'Single-family Permits Bldgs','Single-family Permits Units'
                        ,'Single-family Permits Value','Two-family Permits Bldgs','Two-family Permits Units'
                        ,'Two-family Permits Value','3-4 family Permits Bldgs','3-4 family Permits Units'
                        ,'3-4 family Permits Value','5+ family Permits Bldgs','5+ family Permits Units'
                        ,'5+ family Permits Value')
Permits_Countys=read.csv('https://www2.census.gov/econ/bps/County/co2305y.txt',skip=1)
FinalTable_County <- Permits_Countys[,c(2,6:18)]
FinalTable_County %>% 
  left_join(FipsToState,by=c("State"="FIPS_Code")) %>% 
  mutate(State=Postal_Abbr.) %>% 
  select(c(1:14))->FinalTable_County
FinalTable_County$Area<-paste(FinalTable_County$Name,FinalTable_County$State,sep="")
FinalTable_County=FinalTable_County[,-c(1:2)]
FinalTable_County = relocate(FinalTable_County,Area)
colnames(FinalTable_County)<-c("Area", 'Single-family Permits Bldgs','Single-family Permits Units'
                            ,'Single-family Permits Value','Two-family Permits Bldgs','Two-family Permits Units'
                            ,'Two-family Permits Value','3-4 family Permits Bldgs','3-4 family Permits Units'
                            ,'3-4 family Permits Value','5+ family Permits Bldgs','5+ family Permits Units'
                            ,'5+ family Permits Value')
FinalDataFrame <- rbind(FinalTable_MSA,FinalTable_County)

# Define UI for application that creates table for Permits at County or MSA Level
ui <- fluidPage(

    # Application title
    titlePanel("Local HBA Data"),

    # Sidebar with a input and output definitions
    sidebarLayout(
        sidebarPanel(
          # Input: Choose Data set
            selectizeInput("dataset",                   # Once Incorporate Multiple Selections
                        label=NULL,choices=NULL,multiple = TRUE,options=list(maxOptions=20
                                                                                          ,placeholder="Select a MSA or County")) 
        ),
        # Button 
        downloadButton("downloadData","Click Here to Download",class = "btn-lg btn-success")),
        
    # main panel to display output
        mainPanel(textOutput("table"),
                  tableOutput("finalTable")),
    
    )
# Display of the table 
server <- function(input, output, session) {
    updateSelectizeInput(session,"dataset",choices=FinalDataFrame$Area,server=TRUE)
    data <- reactive({
      FinalDataFrame[FinalDataFrame$Area %in% input$dataset,]
    })

  output$table <- renderText({
    paste(paste(input$dataset,sep=","),"Permits YTD as of May 2023. (Valuation in thousands for MSA, County Valuation is USD)")
  })
  
  output$finalTable <- renderTable({
      data()
      
  })
  output$downloadData <- downloadHandler(
    filename = function(){paste("Permits",Sys.Date(),".csv",sep="")},
    content = function(file){write.csv(data(),file)}
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
