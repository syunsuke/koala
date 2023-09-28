library(shiny)
library(DT)
library(leaflet)
library(tidyverse)
library(lubridate)
library(retiex)


# UIはtab形式で構成
ui <- navbarPage(
  "KOALA",
  mymain_Tab("main"),
  mydetail_Tab("detail")
)

server <- function(input, output) {
  all_data <- base_data("all")
  mymain_server("main",all_data)
  mydetail_server("detail",all_data)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
