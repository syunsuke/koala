
##############################################################
# Server
##############################################################

data_server <- function(id){
  moduleServer(id, function(input, output, session) {
    reactive({
      load("data/k_data.rda")
      return(k_data)
    })
  })
}


data_centerID <- function(id, data, center_pos){
  moduleServer(id, function(input, output, session) {
    
    reactive({
      
      centerpos <- center_pos()
      obj <- data()
      
      if(is.null(centerpos)){
        return(NULL)
      }
      
      ans <- 
        obj %>% 
        mutate(dist_from_center = 
                 (.$long - centerpos[1])^2 + (.$lat - centerpos[2])^2) %>% 
        arrange(dist_from_center)
      
      return(ans$bind_id[1])
      
    })
    
  })
}

