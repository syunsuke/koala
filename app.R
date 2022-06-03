library(shiny)
library(DT)
library(leaflet)
library(tidyverse)
library(lubridate)

#devtools::load_all(".")

##############################################################
# UI
##############################################################
ui <- 
  navbarPage(
    "KOALA",
    mainTab_UI("main"),
    settingTab_UI("main"),
    aboutTab_UI("main")
  )


##############################################################
# Server
##############################################################
server <- function(input, output) {
  
  #**************************
  # データアップデート
  #**************************
  
  # 読み込みデータ全体
  all_data <- data_server("main")
  
  # マップ中心部の座標
  center_pos <- 
    mapA_center_pos("main")
  
  # mapAの一番中心に近いデータのbind_id
  center_data <- 
    data_centerID("main", all_data, center_pos)
  
  # セレクターでセレクトされたデータ群
  filtered_data <- 
    filter_filtered_data("main", all_data)
  
  #**************************
  # 表示アップデート
  #**************************
  
  # map作成
  mapA_Server("main")
  
  # センターデータに基づくselectorのアップデート
  filter_UI_update_server("main", all_data, center_data)
  
  # エリアデータのテーブル出力アップデート
  filter_filtered_datatable_update("main", filtered_data)
  
  # マップタイルの入れ替え
  maptile_select_Server("main","mapA")
  
  # マーカーのアップデート
  mapA_markerUpdate("main", all_data)
  
  # データテーブルでクリックしたデータのマーカーへフォーカス移動
  filter_forcusing_marker("main", "mapA", filtered_data)
  
  # 分布グラフとサマリー出力
  summary_plot_update("main", filtered_data)
  summary_text_update("main", filtered_data)
  
}





##############################################################
# Run the application 
##############################################################
shinyApp(ui = ui, server = server)
