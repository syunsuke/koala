
# summaryUI
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


summary_plot_update <- function(id, area_s) {
  moduleServer(id, function(input, output, session) {
    
    # 初期マップの設定
    output$plot <- renderPlot({
      data <- area_s()
      
      ggplot(data) + geom_density(aes(x=`標準地価格`, color=`用途`))
    },res = 96)
  
  })
}

summary_text_update <- function(id, area_s) {
  moduleServer(id, function(input, output, session) {
    
    # 初期マップの設定
    output$summary <- renderDT({
      
      area_s() %>% 
        group_by(`用途`) %>% 
        summarise(mean = mean(`標準地価格`) %>% as.integer(),
                  sd = sd(`標準地価格`) %>% as.integer(),
                  count = n(),
                  min = min(`標準地価格`),
                  max = max(`標準地価格`),
                  median = median(`標準地価格`)
                  
                    )
      
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