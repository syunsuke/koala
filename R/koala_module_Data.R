##############################################################
# DATAの更新制御
##############################################################

# 全体のデータ
# TODO ファイルで用意するだけでなく、データベースから引っ張れるようにする
# TODO データの構成（仕様？）をまとめる
# TODO データファイルを作成するスクリプトを同梱する
base_data <- function(id){
  moduleServer(id, function(input, output, session) {
    reactive({
      load("data/chikakouji_all_data.RData")
      return(chikakouji_all_data)
      #load("data/small_test_data.rda")
      #return(small_test_data)
    })
  })
}

# 年次選択セレクタで指定された年のデータ取り出し
# input$yearSelect
# 列 年次 入力データに必須
# 列 bind_id 出力データに通し番号
year_data <- function(id, data){
  moduleServer(id, function(input, output, session) {
    reactive({
      ans <- data() %>% 
        dplyr::filter(`年次` == input$yearSelect) 
      ans <-  ans %>% mutate(bind_id = 1:nrow(ans))
      return(ans)
    })
  })
}

# selectors_UIによる選択肢(年次以外)で選別されたデータを返す
# input$check[RCFA],input$prefectureSelect,input$areaSelect
# 入力列  `用途` `都道府県` `市区町村名`
selected_data <- function(id, data){
  moduleServer(id, function(input, output, session) {
    
    # kind_filter_data() ####
    # 用途でフィルタリング
    # チェック種類でフィルタする
    # 何も入っていない場合は、すべて表示
    kind_filter_data <- reactive({
      
      all_data <- data()

      ans <- NULL
      if(any(input$checkR, input$checkC, input$checkF, input$checkA)){

        if(input$checkR){
          tmpR <- all_data %>% filter(`用途` == "000")
          ans <- bind_rows(ans,tmpR) }

        if(input$checkC){
          tmpC <- all_data %>% filter(`用途` == "005")
          ans <- bind_rows(ans,tmpC) }

        if(input$checkF){
          tmpF <- all_data %>% filter(`用途` == "009")
          ans <- bind_rows(ans,tmpF) }

        if(input$checkA){
          tmpA <- all_data %>% filter(`用途` %in% c("003","007","010","013"))
          ans <- bind_rows(ans,tmpA) }
        
      }else{ ans <- all_data }
      
      return(ans)
      
    })

    # セレクタで都道府県、市町村を選別
    reactive({
      pref <- input$prefectureSelect
      area <- input$areaSelect
      if(is.null(pref) | is.null(area)){return(NULL)}
      a_data <- kind_filter_data() %>%
        dplyr::filter(`都道府県` == pref, `市区町村名` == area)
      return(a_data)
    })
    
  })
}

# 選別結果一覧表(selecotrs_UI)でクリックされたデータと同じidのデータ
# 入力データは、一覧表に表示されているデータ（selector_UIの結果）
# TODO selected_data()を中で生成しても良い？
# input$areadata
pickedup_point_data <- function(id, gropu_data){
  moduleServer(id, function(input, output, session) {

    eventReactive(input$areadata_cell_clicked, {
      info <-  input$areadata_cell_clicked
      ans <- gropu_data()[info$row,]
      return(ans)
    })
    
  })
}


# 選別結果一覧表でクリックされた行のデータと同じポイントのデータ群を返す
# クリックされたデータと同じidのデータ
# input$areadata
same_id_data_as_clicked <- function(id, group_data, adata){
  moduleServer(id, function(input, output, session) {
    
    eventReactive(input$areadata_cell_clicked, {
      info <-  input$areadata_cell_clicked
      pid <- group_data()[info$row,]$point_id
      ans <- adata() %>% 
        dplyr::filter(point_id == pid)
      return(ans)
    })
    
  })
}
