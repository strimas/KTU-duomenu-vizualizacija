library(shiny)
library(dplyr)
library(tidyverse)
# data input
sodra <- read_csv("lab_sodra.csv")
sodra = sodra[sodra$ecoActCode == 692000,]

# front end
ui = fluidPage(
  #Application title
  titlePanel("Jusu ekonomines veiklos srities pavadinimas"),
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Choose company", distinct(sodra, sodra$code))
    ),
    
    #Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
# back end
server = function(input, output, session){
  output$distPlot = renderPlot({
    sodra[sodra$code ==input$company,] %>%
      select(month, avgWage) %>%
      ggplot(aes(x = month, y = avgWage))+
      geom_point() +
      scale_x_continuous("Month",breaks=202101:202112,limits=c(202101,202112)) + 
      geom_line() +
      theme_light()
  })
}
shinyApp(ui, server)
