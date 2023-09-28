##############################################################
# MAP関連の処理
##############################################################

# mapの表示
# output$klmap
koala_map <- function(id) {
  moduleServer(id, function(input, output, session) {
    # map名は「klmap」
    
    # 初期マップの設定
    # TODO 初期位置の設定
    output$klmap <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        setView(lng = 135.526001, lat = 34.688047, zoom = 10)
      
    })
    
    outputOptions(output, "klmap", suspendWhenHidden = FALSE)
    
  })
}

# mapでフォーカス移動
# selectors_UIの結果表示TDをクリックしてフォーカス
# output$klmap
# input$areadata (表示されているデータ)
koala_map_forcusing_target  <- function(id, group_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      ans_list <- input$areadata_cell_clicked
      
      if(is.null(ans_list)){ return() }
      
      tmp <- group_data()[ans_list$row,]
      
      leafletProxy("klmap") %>% 
        flyTo(tmp$long, tmp$lat, zoom = 17)
    })

  })
}

# map上でクリックされたーマーカーに紐付いたデータ
# output$klmap
# 表示されているデータ(選択年次の全国データ)
koala_map_clickedMarkerData <- function(id, data_s) {
  moduleServer(id, function(input, output, session) {

    reactive({
      obj <- data_s()
      click_obj <- input$klmap_marker_click
      
      if(is.null(click_obj)){return()}
      
      ans <- obj %>%
        filter(bind_id == click_obj$id)
      return(ans)
    })
    
  })
}

# koala_mapのマーカー表示をアップデート
# output$select_map
# 表示されるデータ(選択年次の全国データ)
# TODO utilを改善
# add_marker は　util_marker
koala_map_marker_update <- function(id, targetdata_server) {
  moduleServer(id, function(input, output, session) {
    observe({
      k_data <- targetdata_server()
      leafletProxy("klmap") %>%
        add_marker(k_data)
    })
  })
}

# mapのディテールマーカー表示をアップデート
# output$klmap
# 表示されるデータ
koala_map_detail_marker_update <- function(id, targetdata) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      ans <- targetdata()
      
      if(nrow(ans) == 0){return()}
      
      leafletProxy("klmap") %>%
        clearGroup("dmap") %>% 
        addCircleMarkers(ans$long, ans$lat, group = "dmap",
                         stroke = FALSE,
                         radius = 3,
                         opacity = 1,
                         weight = 1,
                         fill = TRUE,
                         fillColor = "red",
                         fillOpacity = 1,
                         label = ans$`標準地番号`,
                         labelOptions =
                           labelOptions(noHide = TRUE,
                                        textOnly = TRUE,
                                        style = list(color = "red"),
                                        textsize = "25px",
                                        direction = "bottom")) %>%
        setView(ans$long, ans$lat, zoom = 18)
      
    })
    
  })
}


# mapのタイルをアップデート
# output$klmap
# 表示されるデータ
koala_map_tile_update <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      #if(is.null(input$mapchooser)){return()}
      
      if(input$mapchooser == 1){
        leafletProxy("klmap") %>% clearTiles() %>% addTiles()
        
      }else if(input$mapchooser == 2){
        # 国土地理院
        atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        leafletProxy("klmap") %>%
          clearTiles() %>%
          addTiles("http://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",attribution = atr)
        
      }else if(input$mapchooser == 3){
        # 国土地理院
        atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        leafletProxy("klmap") %>%
          clearTiles() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",attribution = atr)
        
      }else if(input$mapchooser == 4){
        # 国土地理院（航空写真）
        atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        leafletProxy("klmap") %>%
          clearTiles() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.jpg",attribution = atr)
        
      }else{
        leafletProxy("klmap") %>%
          clearTiles() %>%
          addTiles()
      }
    }) 
    
  })
}


