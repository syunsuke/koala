##############################################################
# UI
##############################################################

# mapの描画
# output$klmap
map_UI <- function(id, height = 95){
  l_css <- sprintf("#%s {height: calc(%dvh - 50px) !important;}", NS(id,"klmap"), height)
  tagList(
    tags$style(type="text/css", l_css),
    leafletOutput(NS(id,"klmap")))
}

# 年次、都道府県、市町村、種別等の選択用UI
# 選別結果表示UI
# input$yearSelect,,,,,
# output$areadata
selectors_UI <- function(id){
  
  tagList(
    
    fluidRow(
      column(width = 8, 
             tagList(
               
               # 都道府県、市町村のセレクタ
               fluidRow(
                 column(width = 4, 
                        # 標準地の絞り込み
                        selectInput(NS(id,"yearSelect"),
                                    "年次",
                                    choices = year_list,
                                    selected = 2023)),
                 
                 column(width = 4, 
                        # 標準地の絞り込み
                        selectInput(NS(id,"prefectureSelect"),
                                    "都道府県",
                                    choices = c())),
                 
                 column(width = 4,
                        selectInput(NS(id,"areaSelect"),
                                    "地域",
                                    choices = c()))
               ), #fluidRow_end
               
               # 用途のラジオボタン
               fluidRow(
                 column(3,checkboxInput(NS(id,"checkR"),"住",value = FALSE)),
                 column(3,checkboxInput(NS(id,"checkC"),"商",value = FALSE)),
                 column(3,checkboxInput(NS(id,"checkF"),"工",value = FALSE)),
                 column(3,checkboxInput(NS(id,"checkA"),"他",value = FALSE))
               )
             ) #taglist_end
      ), 
      
      column(width = 1),
      column(width = 3, select_map_UI(id))
      
    ), #fluidRow_end
    
    # 出力用テーブル
    DTOutput(NS(id,"areadata"))
  )
  
}

# マップタイルの選択
# input$
# output$areadata
select_map_UI <- function(id){
  
  tagList(
    
    # マップタイルの選択
    fluidRow(
      radioButtons(NS(id, "mapchooser"), label = "地図を選ぶ",
                   choices = list("Open Street Map" = 1,
                                  "国土地理院:標準地図" = 2,
                                  "国土地理院:淡色地図" = 3,
                                  "国土地理院:航空写真" = 4), selected = 1)
    )
   
    
     
  )
  
}

# 統計量出力UI
# output$summary
summary_UI <- function(id){
  
  tagList(
    fluidRow(
      # 分布グラフ
      plotOutput(NS(id,"plot"), height="200px") 
    ),
    fluidRow(
      DTOutput(NS(id,"summary"))
    )
  )
}

# 同一標準地の各年データ一覧
# output$sameid_point_list
# TODO 各出力は独立させるべき？
sameid_stdpoint_display_UI <- function(id){
  
  tagList(
    
    fluidRow(
      
      column(width = 4, 
             # 同一IDの標準地一覧
             DTOutput(NS(id,"sameid_point_list")) ),

      column(width = 6, 
             # 価格推移のグラフ
              plotOutput(NS(id,"price_line"), height = 300) ),
      
      column(width = 2,
             # 指数の標準年
             selectInput(NS(id,"baseyear"),
                         "指数の標準年",
                         choices = year_list,
                         selected = 2008),
             p(),
             p(),
             
             # 期間調整スライダー
             sliderInput(NS(id,"termslider"), "グラフの期間",
                         min = 1983, max = 2023, value = c(2000,2023)) )
      )
    )
}


# 個別の標準地の詳細データ
# output$point_title,,,,
# TODO download処理はバグがある
stdpoint_detail_UI <- function(id){
  
  tagList(
    # ターゲットの標準値番号
    uiOutput(NS(id,"point_title")),
    
    hr(),
    
    tableOutput(NS(id,"point_detail")),
    
    hr(),
    
    downloadButton(NS(id,"mydownloadData"), 'Download CSV file') 
    
  )
  
}


# 年次セレクターコンテンツ
year_list = list(
  "2023 R05" = 2023,
  "2022 R04" = 2022,
  "2021 R03" = 2021,
  "2020 R02" = 2020,
  "2019 H31/R1" = 2019,
  "2018 H30" = 2018,
  "2017 H29" = 2017,
  "2016 H28" = 2016,
  "2015 H27" = 2015,
  "2014 H26" = 2014,
  "2013 H25" = 2013,
  "2012 H24" = 2012,
  "2011 H23" = 2011,
  "2010 H22" = 2010,
  "2009 H21" = 2009,
  "2008 H20" = 2008,
  "2007 H19" = 2007,
  "2006 H18" = 2006,
  "2005 H17" = 2005,
  "2004 H16" = 2004,
  "2003 H15" = 2003,
  "2002 H14" = 2002,
  "2001 H13" = 2001,
  "2000 H12" = 2000,
  "1999 H11" = 1999,
  "1998 H10" = 1998,
  "1997 H9" = 1997,
  "1996 H8" = 1996,
  "1995 H7" = 1995,
  "1994 H6" = 1994,
  "1993 H5" = 1993,
  "1992 H4" = 1992,
  "1991 H3" = 1991,
  "1990 H2" = 1990,
  "1989 H1/S64" = 1989,
  "1988 S63" = 1988,
  "1987 S62" = 1987,
  "1986 S61" = 1986,
  "1985 S60" = 1985,
  "1984 S59" = 1984,
  "1983 S58" = 1983)

