
mainTab_UI <- function(id){
  
  tabPanel("広域",
           fluidRow(
             
             # 左map
             column(width = 7,
                    # 広域マップ
                    mapA_UI(id)
             ),
             
             # 右menu
             column(width = 5,
                    # 都道府県、市町村、用途セレクタ
                    # 及び、結果表示テーブル
                    filter_UI(id),
                    
                    hr(),
                    
                    # 分布可視化グラフと数値サマリ
                    summary_UI(id)
             )
           )
  ) 
}

settingTab_UI <- function(id){
  
  tabPanel("設定",
           fluidRow(
             column(width = 1),
             column(width = 5,
             # マップタイルの選択
             maptile_select_UI(id)
             )
           )
  ) 
}

#///////////////////////////////////////////////
# お知らせ
#///////////////////////////////////////////////
aboutTab_UI <- function(id){
  
  tabPanel("about",
           
           fluidRow(
             column(2),
             column(8,
                    h1("扱っているデータについて"),
                    p("国土交通省国土政策局「国土数値情報」をもとに
                                大阪府不動産鑑定士協会に所属する不動産取引価格情報活用小委員会が
                           編集・加工をおこなったデータを使用しています"),
                    
                    tags$ol(
                      tags$li(a(href="http://nlftp.mlit.go.jp/ksj/", target="_blank", "「国土数値情報」")), 
                      tags$li(a(href="https://rea-osaka.github.io/", target="_blank", "「不動産取引価格情報活用小委員会」")) 
                    )
             ),
             column(2)
           )
  )
}           

