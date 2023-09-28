#////////////////////////////////////////////////////////////
# ポップアップの中身を作る例
#////////////////////////////////////////////////////////////
make_kouji_popup <- function(k_data){
  
  popup_str <- 
    paste(sep = "<br>",
          paste0(k_data$`年次`,"年 地価公示"),
          k_data$`標準地番号`,
          paste("単価:", keta_str(k_data$`標準地価格`,"円／㎡"),"(",k_data$`対前年比`,"%)"),
          paste("所在:", k_data$`所在並びに地番`),
          paste("地積:", keta_str(k_data$`地積`,"㎡")),
          paste("現況:", k_data$`利用の現況`, k_data$`建物構造`),
          paste0("道路: ", 
                 k_data$`前面道路の方位区分`,
                 " ", 
                 k_data$`前面道路の幅員`,
                 "m", " ",
                 k_data$`前面道路区分`),
          paste("駅　:", k_data$`駅名`, keta_str(k_data$`駅距離`,"m")),
          paste("用途地域:",  k_data$`用途地域`, 
                paste0("(",  ifelse(k_data$`建ぺい率` == 0, 100, k_data$`建ぺい率`), "/", k_data$`容積率`,")")),
          paste("外部マップ:", 
                gm_search_link(k_data),
                ",",
                zcmap_view_link(k_data),
                ",",
                mlit_view_link(k_data))
    )
  
  return(popup_str)
  
}

make_kouji_value <- function(k_data){
  ans <- c(keta_str(k_data$price,"円／㎡"),
           k_data$`所在`,
           keta_str(k_data$`地積`,"㎡"),
           paste(k_data$`利用の現況`, k_data$`建物構造`),
           paste(k_data$`用途地域`, 
                 paste0("(",  ifelse(k_data$`建ぺい率` == "-", 100, k_data$`建ぺい率`), "/", k_data$`容積率`,")"))
  )
  return(ans)
}


# 全国地価マップへのリンク
# 世界測地系から日本測地系が必要
# https://www.jalan.net/jw/jwp0200/jww0203.do
# jy = wy * 1.000106961 - wx * 0.000017467 - 0.004602017
# jx = wx * 1.000083049 + wy * 0.000046047 - 0.010041046
# ※ wy：世界測地系 緯度、wx：世界測地系 経度、jy：日本測地系 緯度、jx：日本測地系 経度
# https://www.chikamap.jp/chikamap/Map?mid=319&mpx=135.653427&mpy=34.811166
zcmap_view_link <- function(df){
  ans <- sprintf('<a href="https://www.chikamap.jp/chikamap/Map?mid=319&mpx=%f&mpy=%f&mps=1500" target="_blank">地価マップ</a>',
                 df$long * 1.000083049 + df$lat * 0.000046047 - 0.010041046,
                 df$lat * 1.000106961 - df$long * 0.000017467 - 0.004602017)
}

# 国土交通省基準地、標準地検索システムへのリンク
mlit_view_link <- function(df){
  b_url <- "https://www.land.mlit.go.jp/landPrice/FullDataServlet?"
  ans <- sprintf('<a href="%sCLASS=0&MOD=1&NO1=%d&NO2=%d&NO3=%d&YER=%d" target="_blank">国交省</a>',
                 b_url,
                 df$`所在地コード` %>% as.numeric(),
                 df$`用途` %>% as.numeric(),
                 df$`連番` %>% as.numeric(),
                 df$`年次` %>% as.numeric())
}


# グーグルマップへのリンク
gm_search_link <- function(df){
  
  
  b_url <- "https://www.google.com/maps/search/?"
  ans <- sprintf('<a href="%sapi=1&query=%s,%s" target="_blank">GMaps</a>',
                 b_url,
                 df$lat %>% as.character(),
                 df$long %>% as.character())
  return(ans)
}

##########################################################
# icon color 
##########################################################
icon_color <- function(v){
  ans <- ifelse(v == "000", "green",
                ifelse(v == "005", "orange",
                       ifelse(v == "009", "blue","purple")))
  return(ans)
}

##########################################################
# icon shape
##########################################################
icon_shape <- function(v){
  ans <- ifelse(v == "000", "home",
                ifelse(v == "005", "shopping-cart",
                       ifelse(v == "009", "industry","home")))
  return(ans)
}


##########################################################
# add_my_marker
##########################################################
add_marker <- function(map_obj, k_data){
  
  if(nrow(k_data) == 0){
    message("add_maker fail")
    return(map_obj %>% clearGroup("kouji_m"))
  }
  
  #popup_strs <- make_kouji_popup(k_data)
  
  # addMarkerレイヤーを適用
  ans_obj <- 
    map_obj %>% 
    clearGroup("kouji_m") %>% 
    addAwesomeMarkers(data = k_data,
                      group = "kouji_m",
                      layerId = ~bind_id,
                      icon = awesomeIcons(
                        icon = icon_shape(k_data$`用途`),
                        markerColor = icon_color(k_data$`用途`),
                        library = 'fa'),
                      lng = ~long, lat = ~lat,
                      popup = make_kouji_popup(k_data),
                      label = ~`標準地番号`,
                      labelOptions = labelOptions(noHide = TRUE),
                      clusterOptions = markerClusterOptions())
  
  return(ans_obj)
}


keta_str <- function(v, s = "㎡", nsmall = 0){
  paste0(format(as.numeric(v), big.mark = ",",nsmall = nsmall, scientific = F), s)
}
