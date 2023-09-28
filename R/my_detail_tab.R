##############################################################
# 「詳細」タブの設計
# tab　UIとそれに関するリアクティブサーバーを定義して
# app.Rに書き込む
#
# mydetail_Tab(id)
# mydetail_Server(id)
##############################################################

##############################################################
# Tab UI
# koara_module_UIに定義されたUIを元にTabを定義
##############################################################

mydetail_Tab <- function(id){
  
  tabPanel("詳細",
           
           # 上段
           fluidRow(
             
             # 左詳細
             column(width = 2,
                    stdpoint_detail_UI(id)
             ),
             
             # 中map
             column(width = 6,
                    # 広域マップ
                    map_UI(id, 60)
             ),
             
             # 右menu
             column(width = 4,
                    # 都道府県、市町村、用途セレクタ
                    # 及び、結果表示テーブル
                    selectors_UI(id)
                    
             )
           ),
           
           hr(),
           
           # 下段
           sameid_stdpoint_display_UI(id)
  ) 
}



##############################################################
# Reactive or observe Server
# ここで定義したUIに関連するリアクティブを定義
##############################################################

mydetail_server <- function(id, data) {
  
  # data系
  all_data <- data
  ydata <- year_data(id, all_data)
  kdata <- selected_data(id, ydata)
  
  std_data <- same_id_data_as_clicked(id, kdata, all_data)
  target_data <- pickedup_point_data(id, kdata)
  
  # map系
  koala_map(id)
  koala_map_forcusing_target(id, kdata)
  koala_map_tile_update(id)

  # UI書き換え系
  update_cityselector_by_map_center_pos(id, ydata)
  
  # 表示アップデート系
  ui_selected_datalist_update(id, kdata)
  ui_stdpoint_detail_display_update(id, target_data)
  ui_price_line_plot_update(id, target_data, std_data, all_data)
  ui_sameid_datalist_update(id, std_data)
  
  koala_map_detail_marker_update(id, target_data)
  pricefile_download(id, all_data, target_data) 
  
}






