##############################################################
# 「Main」タブの設計
# tab　UIとそれに関するリアクティブサーバーを定義して
# app.Rに書き込む
#
# mymain_Tab(id)
# mymain_Server(id)
##############################################################


##############################################################
# UI
##############################################################

mymain_Tab <- function(id){
  
  tabPanel("標準地選択",
           fluidRow(
             
             # 左map
             column(width = 7,
                    # 広域マップ
                    map_UI(id)
             ),
             
             # 右menu
             column(width = 5,
                    
                    fluidRow(
                      # 都道府県、市町村、用途セレクタ
                      # 及び、結果表示テーブル
                      selectors_UI(id)),
                    
                    hr(),
                    
                    fluidRow(
                      column(width = 1),
                      column(width = 10,summary_UI(id))),
                      column(width = 1)

             )
             )
           )
}

##############################################################
# Server
##############################################################

mymain_server <- function(id, data) {
  
  # data系
  all_data <- data
  ydata <- year_data(id, all_data)
  kdata <- selected_data(id, ydata)
  
  # map系
  koala_map(id)
  koala_map_marker_update(id, ydata)
  koala_map_forcusing_target(id, kdata)
  koala_map_tile_update(id)

  # UI書き換え系
  update_cityselector_by_map_center_pos(id, ydata)
  
  # 表示アップデート系
  ui_selected_datalist_update(id, kdata)
  ui_summary_update(id, kdata)
  
  }
    