
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(highcharter)
library(igraph)
library(Matrix)
library(plotly)
library(reactable)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(stringr)
library(visNetwork)


load("../data/kesernetwork.RData")
source("func/utils.R")
source("func/network.R")
source("func/sunburst.R")
source("func/circularStatic.R")
source("func/circularInteractive.R")

interested_df <- data.frame("nodeID"=colnames(cos.list[[1]]),
             "Description"=str_wrap(dict.combine$Description[match(colnames(cos.list[[1]]),dict.combine$Variable)], width = 35))


#  ui ---------------------------
ui <- dashboardPage(
  dashboardHeader(title = "KESER Network",
                  leftUi = tagList(
                    includeCSS("www/style.css"),
                    dropdownButton(
                      inputId = "controls",
                      label = "Controls",
                      icon = icon("sliders"),
                      status = "primary",
                      circle = FALSE,
                      checkboxInput("cluster", "Cluster by groups*", value = FALSE),
                      checkboxInput("label_network", "Hide the labels", value = TRUE),
                      selectInput("network_layout", "The layout of network",
                                  choices = c("layout_nicely", "layout_with_mds", "layout_with_graphopt", "layout_with_lgl"), selected = "layout_nicely"),
                      selectInput("Focus", label = "Focus on node:",
                                  choices = "All", width = "100%"),
                      sliderInput("scale_id","Focus scale:", min=1,max=10,value=5,width = "100%"),
                      sliderInput("slider_h", "Graph height:",
                                  min = 100, max = 1500, value = 750,width = "100%")
                    ),
                    downloadButton("downloadData",
                                 " Download",
                                 icon = icon("download"),
                                 class="btn btn-primary header-button",
                                 width = "100px",
                                 style = "padding: 6px;"
                    ),
                    downloadButton("instruct", " About",
                                 icon = icon("book"),
                                 class="btn btn-primary header-button",
                                 width = "100px",
                                 style = "padding: 6px 20px 6px 20px;"),
                    actionButton("help", " Help",
                                   icon = icon("question"),
                                   class="btn btn-primary header-button",
                                   width = "100px",
                                   style = "padding: 6px 20px 6px 20px;")


                  ),
                  titleWidth = "310pt"
  ),
  dashboardSidebar(collapsed = TRUE,
    width = "310pt",
    introjsUI(),
    uiOutput("ui_table"),

    div(
    checkboxGroupInput("inCheckboxGroup2", "Candidates",
                       choiceValues = c("PheCode:008.5", "PheCode:008.6", "PheCode:008.7", "PheCode:008", "PheCode:010", "PheCode:031"  , "PheCode:038.1", "PheCode:038.2", "PheCode:038.3", "PheCode:038", "PheCode:041.1", "PheCode:041.2", "PheCode:041.4", "PheCode:041.8", "PheCode:041.9", "PheCode:041", "PheCode:053.1", "PheCode:053"  , "PheCode:054"),
                       choiceNames = c("PheCode:008.5", "PheCode:008.6", "PheCode:008.7", "PheCode:008", "PheCode:010", "PheCode:031"  , "PheCode:038.1", "PheCode:038.2", "PheCode:038.3", "PheCode:038", "PheCode:041.1", "PheCode:041.2", "PheCode:041.4", "PheCode:041.8", "PheCode:041.9", "PheCode:041", "PheCode:053.1", "PheCode:053"  , "PheCode:054"),
                       selected = c("PheCode:008.5", "PheCode:008.6", "PheCode:008.7", "PheCode:008", "PheCode:010", "PheCode:031"  , "PheCode:038.1", "PheCode:038.2", "PheCode:038.3", "PheCode:038", "PheCode:041.1", "PheCode:041.2", "PheCode:041.4", "PheCode:041.8", "PheCode:041.9", "PheCode:041", "PheCode:053.1", "PheCode:053"  , "PheCode:054"),
                       width = "100%"),
    id = "divcheckboxgroups"),





    fluidRow(column(7, selectInput("selectmethod", label = "Select data from:",
                                   choices = list("MGB local" = 1,"VA local" = 2,
                                                  "MGB integrative" = 3,"VA integrative" = 4),
                                   selected = 1,width = '100%')),
             column(5, fluidRow(actionButton("goButton", "Show", width = "100px",
                                               icon = tags$i(class = "far fa-play-circle",
                                                             style="font-size: 10px"),
                                               class = "btn-success"),
                                actionButton('refresh', 'Unselect', width = "100px",
                                               icon = tags$i(class = "fa fa-refresh",
                                                             style="font-size: 10px"))))),

    minified = FALSE
  ),

  dashboardBody(
    shinybrowser::detect(),
    uiOutput("network"),
    bsModal(
      id = "selectednode", title = "Node infomation", trigger = FALSE,
      size = "large",
      fluidRow(
        column(8,
          htmlOutput("clicked_node_info")
        ),
        column(3,
          div(actionButton("addButton", "Add to candidates",class="btn-primary active", width = "157px"),
              align = "center", style = "margin-top: 23px;"),
          # br(),
          div(actionButton("infoButton", class="btn-primary active", width = "157px",
                                 tags$a("More information",href="https://celehs.github.io/KESER/", target="_blank")), align = "center", style = "margin-top: 5px;"),
          # br(),
          div(uiOutput("tophecodemap"), align = "center", style = "margin-top: 5px;")
        )
      ),
      # br(),
      hr(),
      tabsetPanel(
        id = "hidden_tabs",
        tabPanel(title = "Circular plot",
                 br(),
                 h5("*Bar height reflects cosine similarity"),
                 uiOutput("circularplot")),
        tabPanel(title = "Sunburst plot",
                 br(),
                 fluidRow(column(6,
                                 sliderTextInput("changeline","max Text length on each line (set as 99 if not breaking lines:)",
                                                 choices = c(5,10,15,20,25,99),selected = 10,grid=TRUE,width = "100%"),
                                 pickerInput(
                                   inputId = "rotatelabel",
                                   label = "The orientation of text inside sectors",
                                   choices = c("Radial", "Tangential")
                                 )
                 ),
                 column(6,sliderInput("scale_sungh","Graph height:",
                                      min=500,max=1000,value=750,width = "100%"))
                 ),
                 div(uiOutput("sun_ui"),align="center")
                 ),
        tabPanel(title = "Drugs information",
                 br(),
                 uiOutput("ui_drugs"))
      )
    ),



    bsModal(
      id = "unlisted_node", title = "Node infomation", trigger = FALSE,
      size = "small",
      htmlOutput("unlisted_node_info")
    ),


      bsModal(
        id = "instruction", title = "Instruction", trigger = "instruct",
        size = "large",
        includeMarkdown("www/documentation.md")
      )
  )
)



# server ---------------------

server <- function(input, output, session) {

  observeEvent(input$help, {
    introjs(session,
            options = list(steps=steps[, -1],
                           showBullets = FALSE))})

  cluster = reactive({input$cluster})

  selected_lines <- reactive({input$df_table_rows_selected})

  selected_nodes = eventReactive(input$goButton,{
    input$inCheckboxGroup2
    },
    ignoreNULL = FALSE)

  method = eventReactive(input$goButton,{
    as.numeric(input$selectmethod)
  },
  ignoreNULL = FALSE)

  node_id <- reactive({input$current_node_id$nodes[[1]]})

  CosMatrix = reactive({
    cos.list[[method()]]
  })

  interested <- colnames(cos.list[[1]])

  not_intereted <- reactive({
    setdiff(rownames(CosMatrix()), interested)
  })

  observeEvent(node_id(), {
    if (node_id() %in% colnames(CosMatrix())){
      toggleModal(session, "selectednode", toggle = "open")
    } else {
      toggleModal(session, "unlisted_node", toggle = "open")
    }
  })

  output$unlisted_node_info <- renderUI({
    clickedNodeText(node_id(), CosMatrix(), dict.combine)
  })

  output$clicked_node_info <- renderUI({
    clickedNodeText(node_id(), CosMatrix(), dict.combine)
  })


  output$network <- renderUI({
    if (length(selected_nodes()) > 0){
      # shinycssloaders::withSpinner(
        visNetworkOutput("network_proxy_nodes",
                         height =  paste0(max(input$slider_h,(shinybrowser::get_height()) - 50),"px"))
      # , type = 6
      # )
    } else{
      div(tags$span("Try to click some rows in "),
          tagList(icon("table")),
          tags$spa(" to specify your nodes"),
          align = "center",
          style = "padding-top: 40px; font-size: 30px;")
    }

  })

  # interested_df <- reactive({
  #   data.frame("Node id"=interested,
  #              "Description"=str_wrap(dict.combine$Description[match(interested,dict.combine$Variable)], width = 35))
  # })

  output$ui_table <- renderUI({
    # withSpinner(
      DTOutput("df_table")
      # ,type = 6)
  })

  output$df_table <-renderDT(datatable({interested_df},
                                       rownames = FALSE,
                                       # width = "250px",
                                       options = list(
                                         paging = FALSE,
                                         scrollY = "300px",
                                         scrollCollapse = TRUE,
                                         bInfo = FALSE
                                       ),
                                       selection = list(mode = 'multiple', selected = c(1:20)),
                                       escape = FALSE
  )%>%
    formatStyle(columns=colnames(interested_df),
                backgroundColor = '#222d32', color = "white"))



  observeEvent(input$refresh, {
    isolate({
      reloadData(
        dataTableProxy('df_table'),
        resetPaging = TRUE,
        clearSelection = "all")
    })
  })


  observe({
    if(length(selected_lines())!=0){
      x = interested[selected_lines()]
      x.neighbor = sapply(x, function(xx){
        sum(CosMatrix()[,xx]!=0)
      })
      x.name = dict.combine$Description[match(x,dict.combine$Variable)]
      x.neighbor = paste0(x.name," (" ,x.neighbor," neighbors)")
    }else{
      x = x.name = x.neighbor = character(0)  # Can use character(0) to remove all choices
    }

    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = paste(length(x), " candidate nodes:"),
                             choiceValues = x,
                             choiceNames = x.neighbor,
                             selected = x
    )
  })


  id <- NULL

  observeEvent(input$goButton, {
    # If there's currently a notification, don't add another
    if (!is.null(id))
      return()
    if(length(selected_nodes())>=10){
      id <<- showNotification(paste("You've chosen ", length(selected_nodes())," nodes. It will take a while to finish plotting..."),
                              duration = max(5,3*length(selected_nodes())), type = "message")

    }
    # Save the ID for removal later
  })


  draw.data <- eventReactive(selected_nodes(),{
    if(length(selected_nodes())!=0){
      input.correct = selected_nodes()[1:min(50,length(selected_nodes()))]
      root.node = match(input.correct, colnames(CosMatrix()))
      dataNetwork(root.node=root.node, CosMatrix(), dict.combine, attrs)
    } else {
      NA
    }
  })

  output$network_proxy_nodes <- renderVisNetwork({
    plot_network(selected_nodes(), cluster(), draw.data(), input$label_network, CosMatrix(), dict.combine, attrs, input$network_layout)
  })

  ## Generate sunburst plot using plotly =======================================
  output$sun_ui <- renderUI({
    # shinycssloaders::withSpinner(
      plotlyOutput("sun",width="auto",
                   height=paste0(input$scale_sungh,"px"))
    # , type = 6
    # )

  })

  output$sun <- renderPlotly({
    changeline = input$changeline
    rotatelabel = input$rotatelabel
    scale_sungh = input$scale_sungh
    sunburstPlot(thr_cos=0.01,
                   changeline, rotatelabel, scale_sungh,
                   node_id(), CosMatrix(), dict.combine, attrs$cap_color)
  })


#
#   ## Generate circular plot using highcharter =======================================
#   output$circularplot <- renderUI({
#     if (node_id() %in% colnames(CosMatrix())){
#       div(highchartOutput("circular_highcharter",
#                       width = "100%",
#                       height = "700px"), align = "center")
#     } else {
#       h3("Not interested node.")
#     }
#   })
#
#
#   output$circular_highcharter <- renderHighchart({
#     circularInteractive(0.01, node_id(), CosMatrix(),
#                         dict.combine, attrs$cap_color)
#   })
#








  ## Generate circular plot using ggplot =======================================
  output$circularplot <- renderUI({
    if (node_id() %in% colnames(CosMatrix())){

      div(plotOutput("circular",
                      width = "100%",
                      height = "700px"), align = "center")
    } else {
      h3("Not interested node.")
    }
  })



  output$circular <- renderPlot({
    circularBar(thr_cos_pop=0.01,
                   node_id(), CosMatrix(), dict.combine, attrs)
  })

  observe({
    input$addButton
    isolate({
      AddToCandidate(input$df_table_rows_selected, selected_nodes(),
                     node_id(), CosMatrix(), session, dict.combine)

    })
  })

  output$downloadData <- WriteData(selected_nodes(), draw.data())

  observeEvent(node_id(),{
    if (node_id() %in% phecode$Phecode){
      phe_id = gsub(".+:", "", node_id(), perl = TRUE)
      href = paste0("http://app.parse-health.org/phecode-map/?phecode=", phe_id)
      output$tophecodemap <- renderUI({
        actionButton(inputId = "tomap", class="btn-primary", width = "157px",
                     tags$a("Phecode map to ICD",href=href, target="_blank"))
      })

    } else {
      output$tophecodemap <- renderUI({""})
    }
  })

  df_drugs <- reactive({
    RXNORM_drug[RXNORM_drug$feature_id == node_id(), ]
  })

  drugs_info <- reactive({

    if (sum(!is.na(df_drugs()$LocalDrugNameWithDose)) == 0) {
      tags$div(
        # create the tabs with titles as a ul with li/a
        tags$ul(
          # class="nav nav-tabs",
          # role="tablist",
          lapply(
            df_drugs()$Code,
            function(x){
              tags$li(
                tags$b("Code: "), x
              )
            }
          )
        )
      )
    } else {
      # df_drugs[is.na(df_drugs)] <- ""
      reactableOutput("reac_tb")
    }
  })

  output$reac_tb <- renderReactable({
    reactable(df_drugs()[, 2:3],
              groupBy = "Code", pagination = FALSE, height = 700
    )
  })

  output$ui_drugs <- renderUI({
    drugs_info()
  })

  observeEvent(node_id(),{
    if (node_id() %in% RXNORM_drug$feature_id){
      showTab(inputId = "hidden_tabs", target = "Drugs information")
    } else {
      hideTab(inputId = "hidden_tabs", target = "Drugs information")
    }
  })

  observe({
    if(length(selected_nodes())!=0){

      x = dict.combine$Description_s[match(selected_nodes()[1:min(50,length(selected_nodes()))],
                                           dict.combine$Variable)]
      x = c("All",x)
      updateSelectInput(session, "Focus","Choose one node to focus on:",
                        choices = x, selected = "All")

    }

  })
  observe({
    if(input$Focus != "All"){
      id = dict.combine$Variable[match(input$Focus,dict.combine$Description_s)]
      visNetworkProxy("network_proxy_nodes") %>%
        visFocus(id = id, scale = input$scale_id/10)
    } else{
      visNetworkProxy("network_proxy_nodes")
    }
  })
}

shinyApp(ui = ui, server = server)

