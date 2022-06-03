##############################################################
# UI
##############################################################
filter_UI <- function(id){
  
  tagList(
    
    # 都道府県、市町村のセレクタ
    fluidRow(
      
      column(width = 6, 
             # 標準地の絞り込み
             selectInput(NS(id,"prefectureSelect"),
                         "都道府県",
                         choices = c())),
      
      column(width = 6,
             selectInput(NS(id,"areaSelect"),
                         "地域",
                         choices = c()))
    ),
    
    # 用途のラジオボタン
    fluidRow(
      column(3,checkboxInput(NS(id,"checkR"),"住",value = FALSE)),
      column(3,checkboxInput(NS(id,"checkC"),"商",value = FALSE)),
      column(3,checkboxInput(NS(id,"checkF"),"工",value = FALSE)),
      column(3,checkboxInput(NS(id,"checkA"),"他",value = FALSE))
    ),
    
    # 出力用テーブル
    DTOutput(NS(id,"areadata"))
  )
  
}

##############################################################
# Server
##############################################################

filter_filtered_data <- function(id, data_s){
  moduleServer(id, function(input, output, session) {
    
    # kind_filter_data() ####
    # 用途でフィルタリング
    # チェック種類でフィルタする
    # 何も入っていない場合は、すべて表示
    kind_filter_data <- reactive({
      
      all_data <- data_s()
      ans <- NULL
      if(any(input$checkR, input$checkC, input$checkF, input$checkA)){
        
        
        if(input$checkR){
          tmpR <- all_data %>% filter(`用途` == "000")
          ans <- bind_rows(ans,tmpR)
        }
        
        if(input$checkC){
          tmpC <- all_data %>% filter(`用途` == "005")
          ans <- bind_rows(ans,tmpC)
        }
        
        if(input$checkF){
          tmpF <- all_data %>% filter(`用途` == "009")
          ans <- bind_rows(ans,tmpF)
        }
        
        if(input$checkA){
          tmpA <- all_data %>% filter(`用途` %in% c("003","007","010","013"))
          ans <- bind_rows(ans,tmpA)
        }
        
      }else{
        ans <- all_data
      }
      
      return(ans)
      
    })
    
    reactive({
      pref <- input$prefectureSelect
      area <- input$areaSelect
      
      if(is.null(pref) | is.null(area)){return(NULL)}
      
      a_data <- kind_filter_data() %>%
      #a_data <- obj %>%
        filter(`都道府県` == pref, `市区町村名` == area)
      
      return(a_data)
    })
    
  })
}


# 中心の変更でセレクタの内容をアップデートする
# center_id関数は、任意の地図の中心データを返すもの
filter_UI_update_server <- function(id, data_s, center_id_s){
  
  moduleServer(id, function(input, output, session) {
    
    # 都道府県セレクトアップデート #####
    observe({
      obj <- data_s()
      
      p_list <-
        obj %>% .$`都道府県` %>% unique()
      
      co <- center_id_s()
      
      if(!is.null(co)){
        center_pref <-
          obj %>% 
          filter(bind_id == co) %>%
          .$`都道府県`
      }else{
        center_pref <- ""
      }
      
      updateSelectInput(session, 
                        "prefectureSelect", 
                        choices = p_list, 
                        selected = center_pref)
      
    })
    
    # 市町村セレクトアップデート ####
    observe({
      obj <- data_s()
      co <- center_id_s()
      
      area_list <-
        obj %>%
        filter(`都道府県` == input$prefectureSelect) %>%
        .$`市区町村名` %>% unique()
      
      if(!is.null(co)){
        center_area <- 
          obj %>% 
          filter(bind_id == co) %>%
          .$`市区町村名`
      }else{
        center_area <- ""
      }
      
      updateSelectInput(session, 
                        "areaSelect", 
                        choices = area_list,
                        selected = center_area)
      
    })
    
  })
  
}


filter_forcusing_marker  <- function(id, mapid, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      ans_list <- input$areadata_cell_clicked
      
      str(ans_list)
      
      if(is.null(ans_list)){
        return()
      }
      
      tmp <- filtered_data()[ans_list$row,]
      
      leafletProxy(mapid) %>% 
        flyTo(tmp$long, tmp$lat, zoom = 17)
    })
    
  })
}

# selectorで選択されたデータのテーブル表示
filter_filtered_datatable_update <- function(id, filter_data_s) {
  moduleServer(id, function(input, output, session) {
    
    output$areadata <- renderDT({
      
      # 一覧は、セレクタUIで指定される地域に従う
      # ページ描画中等、UIがない場合には、NULLを返す
      a_data <- filter_data_s()
      if(is.null(a_data)){return(NULL)}
      
      # 表示するためのデータの整形
      ans_data <- a_data %>% 
        mutate(`公示価格` = keta_str(`標準地価格`, "円／㎡")) %>% 
        select(`標準地番号`,`公示価格`,`対前年比`,`所在並びに地番`)
      
      return(ans_data)
    },
    selection = 'none',
    rownames=NULL,
    extensions = 'Scroller',
    options = list(scrollY = 250,
                   pageLength = 200,
                   dom = "RtiS",
                   scrollCollapse = TRUE)
    )

  })
}
