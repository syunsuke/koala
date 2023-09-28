##############################################################
# React
# reactive系のもの
##############################################################

#*******************************************
# UI等での値変更による
# リアクティブ処理
# また、リアクティブ処理によるUI値アップデート
#*******************************************

# 中心の変更で地域等セレクタの内容をアップデートする
# input$klmap
# input$prefectureSelect,input$areaSelect
update_cityselector_by_map_center_pos <- function(id, data_s){
  moduleServer(id, function(input, output, session) {
    
    
    # center position
    pos <-   reactive({
      centerpos <- input$klmap_center 
      if(is.null(centerpos)){ return(NULL) }
      ans <- centerpos %>% unlist()
      return(ans)
    })
    
    
    # center_id
    center_id <- reactive({
      
      centerpos <- pos()
      
      # このnullチェックは必要
      if(is.null(centerpos)){ return(NULL) }
      
      ans <- 
        data_s() %>% 
        dplyr::mutate(dist_from_center = 
                        (.$long - centerpos[1])^2 + (.$lat - centerpos[2])^2) %>% 
        arrange(dist_from_center)
      
      return(ans$bind_id[1])
      
    })
    
    # 都道府県セレクトアップデート #####
    # mapセンターが変化したとき発動
    # 年データが変更されたときに発動
    observe({
      obj <- data_s()
      co <- center_id()
      
      p_list <-
        obj %>% .$`都道府県` %>% unique()

      if(!is.null(co)){
        center_pref <- obj %>%  dplyr::filter(bind_id == co) %>% .$`都道府県`
      }else{
        center_pref <- ""
      }
      
      updateSelectInput(session, 
                        "prefectureSelect", 
                        choices = p_list, 
                        selected = center_pref)
    })
    
    # 市区町村セレクターのアップデート
    # input$prefectureSelect で発動
    # mapの中心変更で発動
    # 年次変更でのデータ変更で発動
    observe({
      
      obj <- data_s()
      co <- center_id()
      
      area_list <-
        obj %>%
        dplyr::filter(`都道府県` == input$prefectureSelect) %>%
        .$`市区町村名` %>% unique()
      
      if(!is.null(co)){
        center_area <-  obj %>%  filter(bind_id == co) %>% .$`市区町村名`
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

#*******************************************
# result output
# UIへの結果表示系
#*******************************************

# 同一地点価格推移のグラフを表示、アップデート
# 表示範囲や指数の基礎となる年により変化
# oubput$price_line
ui_price_line_plot_update <- function(id, target_data, same_id_data, all_data) {
  moduleServer(id, function(input, output, session) {
    
    output$price_line <- renderPlot({
      # 同一標準地データ集合
      if(is.null(same_id_data())){return()}
      
      # 指数の標準年
      b_year <- input$baseyear %>% as.numeric()
      
      # クリックされた対象データ
      g_data <- 
        same_id_data() %>% 
        dplyr::select(`年次`,`標準地価格`) %>% 
        dplyr::mutate(category = "対象地",price = `標準地価格`)
      
      # 同一市区町村データ
      same_city <- 
        all_data() %>% 
        dplyr::filter(`所在地コード` %in% same_id_data()$`所在地コード`) %>% 
        dplyr::filter(`用途` == target_data()$`用途`) %>% 
        group_by(`年次`) %>% 
        summarise(price = mean(`標準地価格`, na.rm = TRUE)) %>% 
        mutate(category = same_id_data()$`市区町村名`[1])
      
      # 同一都道府県データ
      same_pref <-
        all_data() %>% 
        filter(`都道府県` %in% same_id_data()$`都道府県`) %>% 
        filter(`用途` == target_data()$`用途`) %>% 
        group_by(`年次`) %>% 
        summarise(price = mean(`標準地価格`, na.rm = TRUE)) %>% 
        mutate(category = same_id_data()$`都道府県`[1])
      
      g_data <- bind_rows(g_data, same_city, same_pref)        
      
      # グラフデータの集合範囲
      g_data <- g_data %>%  
        filter(input$termslider[[1]] <= `年次`,  `年次` <= input$termslider[[2]] )
      
      # グラフの描画        
      ggplot(g_data) + 
        geom_vline(xintercept = b_year, color = "red") +
        geom_line(mapping = aes(x=`年次`, y=price, color= category, linetype = category)) +
        scale_y_continuous(labels =
                             function(v){
                               retiex::style_yen(v, 3, "千円／㎡")
                             }) +
        scale_x_continuous(labels =
                             function(v){
                               paste0(v, "年")
                             }) +
        coord_cartesian(xlim = c(input$termslider))
    })
    
  })
}

# 統計量表示のアップデート
# output$summary, output$plot
ui_summary_update <- function(id, groupdata) {
  moduleServer(id, function(input, output, session) {
    
    # summaryグラフ
    output$plot <- renderPlot({
      
      d <- groupdata() %>% 
        mutate(`用途` = ifelse(`用途` == "000", "住宅",
                             ifelse(`用途` == "005", "商業",
                                    ifelse(`用途` == "007", "工業", 
                                           ifelse(`用途` == "009", "工業", "その他")))))
      
      
      ggplot(d) + 
        geom_density(aes(x=`標準地価格`, color=`用途`)) +
        xlab(label = "公示価格 （千円／㎡）") +
        scale_x_continuous(labels =
                             function(v){
                               retiex::style_yen(v, 3, "")
                             }) 
        #theme(axis.text.x = element_text(angle = 23, hjust = 1))
        
        
        
        
    },res = 96)
    
    # summaryDT
    output$summary <- renderDT({
      
      d <- groupdata() %>% 
        mutate(`用途` = ifelse(`用途` == "000", "住宅",
                             ifelse(`用途` == "005", "商業",
                                    ifelse(`用途` == "007", "工業", 
                                           ifelse(`用途` == "009", "工業", "その他")))))
      
        d %>% 
          group_by(`用途`) %>% 
          summarise(mean = mean(`標準地価格`) %>% as.integer(),
                    sd = sd(`標準地価格`) %>% as.integer(),
                    count = n(),
                    min = min(`標準地価格`),
                    max = max(`標準地価格`),
                    median = median(`標準地価格`)) %>% 
          mutate(`平均` = retiex::style_yen(mean, 0, "円"),
                 `標準偏差` = retiex::style_yen(sd, 0, "円"),
                 `最大` = retiex::style_yen(max, 0, "円"),
                 `中央値` = retiex::style_yen(median, 0, "円"),
                 `最小` = retiex::style_yen(min, 0, "円"),
                 `地点数` = retiex::style_yen(count, 0, "件") ) %>% 
          select(`用途`,`平均`:`地点数`)
        },
        selection = 'none',
        rownames=NULL,
        extensions = 'Scroller',
        options = list(scrollY = 150,
                       pageLength = 200,
                       dom = "RtiS",
                       scrollCollapse = TRUE)
      )
  })
}

# 年次や都道府県、市町村、種別で選別された結果の
# 一覧データの表示をアップデート
# output$areadata
ui_selected_datalist_update <- function(id, data_s){
  moduleServer(id, function(input, output, session) {
    
    # area display
    output$areadata <- 
      renderDT({
        # 一覧は、セレクタUIで指定される地域に従う
        # ページ描画中等、UIがない場合には、NULLを返す
        a_data <- data_s()
        if(is.null(a_data)){return(NULL)}
        
        # 表示するためのデータの整形
        ans_data <- a_data %>% 
          mutate(`公示価格` = keta_str(`標準地価格`, "円／㎡")) %>% 
          select(`標準地番号`,`公示価格`,`対前年比`,`所在並びに地番`)
        return(ans_data) },
        
        # DTのオプション
        selection = 'none',
        rownames=NULL,
        extensions = 'Scroller',
        options = list(scrollY = 200,
                       pageLength = 200,
                       dom = "RtiS",
                       scrollCollapse = TRUE)
      )
  })
}

# 同一IDデータ群の一覧表示をアップデート
# 同一データ群データを入力
# output$areadata
ui_sameid_datalist_update <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ### 表 markerdata ####
    output$sameid_point_list <- renderDT({
      
      # 一連のデータを得る
      org_data <- data() %>%  
        dplyr::arrange(desc(`年次`))   
      
      # 指数の標準年の切り替え
      base_index <- which(org_data$`年次` == as.numeric(input$baseyear))
      
      pr <- org_data$`標準地価格`
      
      # 変動率計算
      rate_v <- vector("double", length(pr))
      index_v <- vector("double", length(pr))
      
      for(i in seq_along(rate_v)){
        rate_v[i] <- pr[i] / pr[i + 1]
        index_v[i] <- pr[i] / pr[base_index]
      }
      
      # 見やすく加工
      print_data <- 
        org_data %>%
        dplyr::mutate(`公示年` = paste0(`年次`,"年(", `和暦`, ")"),
                      `公示価格` = keta_str(`標準地価格`,"円／㎡"),
                      `対前年比` = keta_str(round(rate_v * 100 - 100,1),""),
                      `指数` = keta_str(round(index_v * 100,1),"")
        ) %>%
        dplyr::select(`公示年`,`公示価格`,`対前年比`,`指数`)
      
      return(print_data)
      
    },
    
    # DTのオプション
    selection = 'none',
    rownames=NULL,
    extensions = 'Scroller',
    options = list(scrollY = 200,
                   pageLength = 200,
                   dom = "RtiS",
                   scrollCollapse = TRUE)
    )
    
  })
}

# 選択された標準地の詳細を表示するUIをアップデート
# output$point_title, output$point_detail
ui_stdpoint_detail_display_update <- function(id, target_data) {
  moduleServer(id, function(input, output, session) {
    
    # 選択された標準地名の表示
    output$point_title <- renderUI({
      d <- target_data()
      if(is.null(d)){return("")}
      
      ans <- tagList(
        h4(paste0(d$`年次`,"年 地価公示")),
        h2(d$`標準地番号`)
      )
      
      return(ans)
    })
    
    # 標準地の詳細を項目で表示
    output$point_detail <- renderTable({
      
      k_data <- target_data()
      if(is.null(k_data)){return("")}
      
      mykey <- c("単価","所在","地積","現況","駅","用途","施設")
      myvalue <- c(keta_str(k_data$`標準地価格`,"円／㎡"),
                   str_replace(k_data$`住居表示`,".*　",""),
                   keta_str(k_data$`地積`,"㎡"),
                   paste(k_data$`利用の現況`, k_data$`建物構造`),
                   paste(k_data$`駅名`, keta_str(k_data$`駅距離`,"m")),
                   paste(k_data$`用途地域`, 
                         paste0("(",  ifelse(k_data$`建ぺい率` == "-", 100, k_data$`建ぺい率`), "/", k_data$`容積率`,") ", k_data$`防火区分`)),
                   k_data$sisetu)
      ans <- tibble("項目" = mykey, "内容" = myvalue) 
      return(ans)
    })
    
  })
}


# データをcsvファイルとしてダウンロードする処理
# バグがあるらしくhtmlファイル（見ている画面のUI）がダウンロードされる
# mydownloadDataボタンで反応
pricefile_download <- function(id, all_data, target_data) {
  moduleServer(id, function(input, output, session) {
    
    output$mydownloadData <-  downloadHandler(
      filename = "pricedata.csv",
      content = function(file) {
        
        target_id <- target_data()$`point_id`
        ans <- all_data() %>% 
          dplyr::filter(point_id == target_id)
        
        write.csv(ans, file, row.names = FALSE) 
      }
    )
    #outputOptions(output, "mydownloadData ", suspendWhenHidden = FALSE)
    
  })
} 


