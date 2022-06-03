# 広域マップmapAのUI
mapA_UI <- function(id){
  
  l_css <- sprintf("#%s {height: calc(95vh - 50px) !important;}", NS(id,"mapA"))
  
  tagList(
  tags$style(type="text/css", l_css),
  leafletOutput(NS(id,"mapA")))
  
}

##############################################################
# Server
##############################################################

# leafletの初期表示
mapA_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 初期マップの設定
    output$mapA <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        setView(lng = 135.526001, lat = 34.688047, zoom = 10)
    })
  
  })
}

# mapAの中心にあるデータをobjから選ぶ 
mapA_center_pos <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    reactive({
      
      if(is.null(input$mapA_center)){
        return(NULL)
      }
      
      centerpos <- 
        input$mapA_center %>% 
        unlist()
      
      return(centerpos)
      
    })
  })
}

# mapAのマーカーを更新
mapA_markerUpdate <- function(id, targetdata_server) {
  moduleServer(id, function(input, output, session) {
  
    observe({
      k_data <- targetdata_server()
      
      leafletProxy("mapA") %>%
        add_marker(k_data)
    })
    
  })
}

# mapAのマーカーのクリックされたものidから
# データを選ぶ
mapA_clickedMarkerData <- function(id, data_s) {
  moduleServer(id, function(input, output, session) {
  
    reactive({
      
      obj <- data_s()
      
      click_obj <- input$mapA_marker_click
      
      if(is.null(click_obj)){return()}
      
      ans <- obj %>%
        filter(bind_id == click_obj$id)
      
      return(ans)
    })
    
  })
}
