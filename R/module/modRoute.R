## UI Module ##
routeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Rotas", width = 12, id = "tabRoute", 
        tabPanel("Histórico de Rotas", 
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeHistoric")))
                 ),
        
        tabPanel("Importar Rotas",
                 p("Para importar as rotas, selecione o Service Center e os arquivos .json, em seguida, clique em 'Importar rotas'."),
                 selectInput(ns("inSel_serviceCenter"), "Service Center *", choices = vec_serviceCenter),
                 fileInput(ns("inFile_routes"), "Arquivo .json das rotas",
                           multiple = T,
                           accept = c("text/plain", ".json"),
                           buttonLabel = "Buscar...",
                           placeholder = "Nenhum arquivo selecionado"),
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeUpload"))), br(),
                 actionButton(ns("btn_importRoutes"), "Importar rotas")
                 ),
        
        tabPanel("Histórico de Importação",
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeUploaded")))
                 )
      )
    )
  )
}


## SERVER Module ##
routeServer <- function(input, output, session, RV, rac_routeDB) {
  
  ## OUTPUT Histórico de Rotas
  output$tbl_routeHistoric <- DT::renderDT({
    req(rac_routeDB())
    df <- rac_routeDB()
    
    datatableTy(df, nome = "Rotas",
                class = "nowrap",
                extensions = "FixedColumns",
                escape = 0,
                options = list(scrollX = T,
                               fixedColumns = list(leftColumns = 2)),
                callback = JS("table.order([5, 'asc']).draw();")) %>% 
      formatCurrency(names(df) %in% c("DistEstimada", "DistRealizada"),
                     currency = "", mark = ".", dec.mark = ",", digits = 2) %>%
      formatCurrency(names(df) %in% c("Performance"),
                     currency = "", mark = ".", dec.mark = ",", digits = 3) %>%
      formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>%
      formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
  })
  
  
  ## OUTPUT Upload de Rotas (para verificar data antes de importar)
  output$tbl_routeUpload <- DT::renderDT({
    req(input$inFile_routes)
    req(input$inSel_serviceCenter)
    
    df <- input$inFile_routes %>%
      .$datapath %>%
      map_df(~fromJSON(., flatten = T)) %>%
      transmute(
        Data = (`initDate` /24/60/60) %>% as.Date(origin = "1970-01-01") %>% as.character()
      ) %>%
      count(Data) %>%
      transmute(`ServiceCenter` = input$inSel_serviceCenter,
                Data = Data %>% as.Date(),
                Rotas = n) %>%
      as.data.frame()
    
    datatableTy(df, nome = "Upload", dom = "rt",
                class = "nowrap",
                options = list(pageLength = nrow(df)),
                callback = JS("table.order([1, 'desc']).draw();")) %>%
      formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR")
  })
  
  
  ## BOTAO Salva uploads no BD
  observeEvent(input$btn_importRoutes, {
    files <- input$inFile_routes
    if (is.null(files)) {
      return()
    } else {
      nfiles = nrow(files)
      for (i in 1:nfiles) {
        tryCatch({
          df <- etl_routeJson2Df(fileJson = files$datapath[i],
                                 SC = input$inSel_serviceCenter)
          
          # Verifica rotas já existentes
          vet_diff <- tryCatch({ 
            vet_diff <- rac_routeDB() %>% 
              filter(Data == date(df$Data[1])) %>% 
              .$Rota %>% 
              intersect(df$Rota)
          }, error = function(e) {
            numeric()
          })
          
          if (length(vet_diff) == 0) {
            set_route(df, user = RV$user)
            shinyalertTy("Sucesso na importação", paste0("Arquivo: ", files$name[i]), type = "success")
            RV$upd_route <- RV$upd_route + 1
          } else {
            shinyalertTy("Falha na importação", 
                         paste0("Rotas ", paste0(head(vet_diff, 3), collapse = ", "), " etc já existem!\n", 
                                "Arquivo: ", files$name[i]), type = "error")
          }
        }, error = function(e) {
          flog.error("btn_importRoutes (E) | %s", e$message)
          shinyalertTy("Erro na importação", paste0("Arquivo: ", files$name[i]), type = "error")
        })
      }
    }
  })
  
  
  ## OUTPUT Histórico de Uploaded de Rotas (para verificar rotas importadas)
  output$tbl_routeUploaded <- DT::renderDT({
    RV$upd_route
    df <- get_routeUploadHistory()
    req(df)
    
    df <- df %>% 
      arrange(desc(CriadoEm)) %>%
      as.data.frame()
    df <- df %>% 
      mutate(Excluir = fshinySetInput(actionButton, paste0(df$SC, "_", format(df$Data, "%Y%m%d")), "btn_delRouteUploaded",
                                      label = icon("times"),
                                      onclick = paste0('Shiny.setInputValue("', session$ns(""),
                                                       'selBtn_delRouteUploaded", this.id, {priority: "event"})')))

    datatableTy(df, nome = "Importação de Rotas",
                class = "nowrap",
                escape = ncol(df) - 1,
                callback = JS("table.order([1, 'desc']).draw();")) %>%
      formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>% 
      formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
  })
  
  
  ## BOTAO Exclui upload de rotas
  observeEvent(input$selBtn_delRouteUploaded, {
    selId <- strsplit(input$selBtn_delRouteUploaded, "-")[[1]][-1]
    SC <- strsplit(selId, "_")[[1]][1]
    Data <- strsplit(selId, "_")[[1]][2] %>% ymd()
    
    df_validate <- validate_del_routeUpload(data = Data, SC = SC)
    if (is.null(df_validate)) {
      del_route(data = Data, SC = SC, user = RV$user)
      shinyalertTy("Sucesso na exclusão", paste0("Service Center ", SC, " do dia ", format(Data, "%d/%m/%Y")), type = "success")
      RV$upd_route <- RV$upd_route + 1
    } else {
      shinyalertTy("Falha na exclusão", 
                   paste0("Eventos das rotas ", paste0(head(df_validate$Rota, 3), collapse = ", "), " etc já existem!"), type = "error")
    }
  })
  
  
  return(RV)
}