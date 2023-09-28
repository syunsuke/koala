
center_pos_test <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive({
      
      centerpos <- input$klmap_center 
        
      if(is.null(centerpos)){ return(NULL) }
      
      ans <- centerpos %>% unlist()
      
      return(ans)
      
    })
  })
}
