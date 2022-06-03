##############################################################
# UI
##############################################################
maptile_select_UI <- function(id){
  fluidRow(
    radioButtons(NS(id,"mapchooser"), label = "地図の背景",
                 choices = list("オープンストリートマップ" = 1,
                                "国土地理院:標準地図" = 2,
                                "国土地理院:淡色地図" = 3,
                                "国土地理院:航空写真" = 4), 
                 selected = 1)
    
  )
}


##############################################################
# Server
##############################################################

maptile_select_Server <- function(id, mapid) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      if(input$mapchooser == 1){
        leafletProxy(mapid) %>% 
          clearTiles() %>% 
          addTiles()
        
      }else if(input$mapchooser == 2){
        # 国土地理院
        atr <- 
          "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        
        leafletProxy(mapid) %>%
          clearTiles() %>%
          addTiles("http://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
                   attribution = atr)
        
      }else if(input$mapchooser == 3){
        # 国土地理院
        atr <- 
          "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        
        leafletProxy(mapid) %>%
          clearTiles() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",
                   attribution = atr)
        
      }else if(input$mapchooser == 4){
        # 国土地理院（航空写真）
        atr <- 
          "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
        
        leafletProxy(mapid) %>%
          clearTiles() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.jpg",
                   attribution = atr)
        
      }else{
        leafletProxy(mapid) %>%
          clearTiles() %>%
          addTiles()
      }
    }) 
    
  })
}
