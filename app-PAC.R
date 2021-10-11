# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(markdown)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Portable air cleaner (PAC) calculator",
                           
                           tabPanel("Home",
                                    
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        h4("Select units"),
                                        
                                        radioButtons("units_type", NULL,
                                                     choices = c('International System of Units (SI)' = 'SI',
                                                                 'Inch-Pound (IP)' = 'IP'),
                                                     selected = 'SI'
                                        ),
                                        h4("Select task"),
                                        
                                        radioButtons("input_type", NULL,
                                                     choices = c('Estimate the effectiveness' = 'Efficacy',
                                                                 'Determine PAC numbers' = 'Needed PAC numbers'),
                                                     selected = 'Efficacy'
                                        ),
                                        
                                        h4("Input parameters"),
                                        
                                        # selectInput("k", label = "Particle type", 
                                        #             choices = list("Ultrafine particle" = 1.64, 
                                        #                            "PM2.5" = 1.67, 
                                        #                            "PM10" = 3.35), 
                                        #             selected = 1.67),
                                        
                                        uiOutput("ui1"),
                                        uiOutput("CADR_lim"),
                                        
                                        uiOutput("height_lim"),  
                                        uiOutput("area_lim"),  
                                        
                                        radioButtons("venti_selection", label = p(HTML(paste0("Ventilation rate (", 
                                                                                  a("air changes per hour", 
                                                                                    href="https://en.wikipedia.org/wiki/Air_changes_per_hour", 
                                                                                    target = "_blank"),
                                                                                  ", ACH)" ) )), 
                                                     choices = c('Select default ventilation rate' = 'venti_type',
                                                                 'Manually type in' = 'venti_type_in'),
                                                     selected = 'venti_type'),
                                        uiOutput("ui3"),
                                        uiOutput("V_Watt")#,
                                        # actionButton("submitbutton", "Calculate", 
                                        #              class = "btn btn-primary")
                                      ),
                                      mainPanel( plotOutput('plot1'), 
                                                 h4(htmlOutput("txtout") )
                                      )
                                    )
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    # titlePanel("About"), 
                                    div(includeMarkdown("about-app-PAC.md"),
                                        align="justify")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  fun_effectiveness <- function(a,k,CADR,V,N) 1-(a+k)/(a+k+N*CADR/V)
  
  fun_num_PAC <- function(a,k,CADR,V,eff) (eff/(100-eff) ) * (a+k) * V/CADR
  
  global_pac_number <- reactiveValues(numVal = 5, numMin = 0, numMax = 50) 
  numVal_pac_number <- reactive({
    if(!is.null(input$N)){
      if(!is.na(input$N)){
        if(input$N < global_pac_number$numMin) return(global_pac_number$numMin)
        if(input$N > global_pac_number$numMax) return(global_pac_number$numMax)     
        return(input$N)
      }else{
        return(NA)
      }
    }else{
      return(global_pac_number$numVal)
    }
  })
  
  
  global_eff <- reactiveValues(numVal = 50, numMin = 0, numMax = 100) 
  numVal_eff <- reactive({
    if(!is.null(input$N)){
      if(!is.na(input$N)){
        if(input$N < global_eff$numMin) return(global_eff$numMin)
        if(input$N > global_eff$numMax) return(global_eff$numMax)     
        return(input$N)
      }else{
        return(NA)
      }
    }else{
      return(global_eff$numVal)
    }
  })
  
  
  
  # output$ui1 <- renderUI({
  #   if (is.null(input$input_type))
  #     return()
  #   switch(input$input_type,
  #          "Efficacy" =  numericInput("N", 
  #                                     label = "Number of PACs deployed", 
  #                                     value = numVal_pac_number(),
  #                                     min = global_pac_number$numMin,
  #                                     max = global_pac_number$numMax),
  #          "Needed PAC numbers" = numericInput("eff", 
  #                                              label = "Effectiveness target (%)", 
  #                                              value = 50)
  #   )
  # })
  
  
  output$ui1 <- renderUI({
    if (is.null(input$input_type))
      return()
    switch(input$input_type,
           "Efficacy" =  numericInput("N", 
                                      label = "Number of PACs deployed", 
                                      value = numVal_pac_number(),
                                      min = global_pac_number$numMin,
                                      max = global_pac_number$numMax),
           "Needed PAC numbers" = numericInput("eff", 
                                               label = "Effectiveness target (%)", 
                                               value = numVal_eff(),
                                               min = global_eff$numMin,
                                               max = global_eff$numMax)
    )
  })
  
  
  
  output$ui3 <- renderUI({
    if (is.null(input$venti_selection))
      return()
    
    switch(input$venti_selection,
           "venti_type" =  selectInput("a", label = NULL,
                                       choices = list("Mechenical ventilation, 1.17 ACH" = 1.17,
                                                      "Infiltration, 0.22 ACH" = 0.22),
                                       selected = 1.17),
           "venti_type_in" =  numericInput("a", 
                                           # label = "Ventilation rate (air changes per hour)", 
                                           label = NULL, 
                                           value = 1.2)
           
    )
  })
  
  global_cadr_si <- reactiveValues(numVal = 444, numMin = 1, numMax = 1000)
  
  numVal_cadr_si <- reactive({
    
    if(!is.null(input$CADR)){
      if(!is.na(input$CADR)){  
        if(input$CADR < global_cadr_si$numMin) return(global_cadr_si$numMin)
        if(input$CADR > global_cadr_si$numMax) return(global_cadr_si$numMax)
        return(input$CADR)
      }else{
        return(NA)
      }
      
    }else{
      return(global_cadr_si$numVal)
    }
  })
  
  
  global_cadr_ip <- reactiveValues(numVal = 250, numMin = 1, numMax = 600)
  
  numVal_cadr_ip <- reactive({
    
    if(!is.null(input$CADR)){
      if(!is.na(input$CADR)){  
        if(input$CADR < global_cadr_ip$numMin) return(global_cadr_ip$numMin)
        if(input$CADR > global_cadr_ip$numMax) return(global_cadr_ip$numMax)
        return(input$CADR)
      }else{
        return(NA)
      }
      
    }else{
      return(global_cadr_ip$numVal)
    }
  })
  
  output$CADR_lim <- renderUI({
    if (is.null(input$units_type))
      return()
    
    switch(input$units_type,
           "SI" =  numericInput("CADR", 
                                label = p(a("Clean air delivery rate, CADR,", href="https://en.wikipedia.org/wiki/Clean_air_delivery_rate", target = "_blank"),
                                          HTML("for PM2.5 of one PAC (m<sup>3</sup>/h)") ), 
                                value =  numVal_cadr_si(),
                                min = global_cadr_si$numMin,
                                max = global_cadr_si$numMax),
           "IP" =  numericInput("CADR", 
                                label = p(a("Clean air delivery rate, CADR,", href="https://en.wikipedia.org/wiki/Clean_air_delivery_rate", target = "_blank"),
                                          HTML("for PM2.5 of one PAC (ft<sup>3</sup>/minute)") ), 
                                value =  numVal_cadr_ip(),
                                min = global_cadr_ip$numMin,
                                max = global_cadr_ip$numMax )
           
    )
  })
  
  
  global_height_si <- reactiveValues(numVal = 2.7, numMin = 0.5, numMax = 30)
  
  numVal_height_si <- reactive({
    # if(input$submitbutton > 0 ){
    if(!is.null(input$height)){
      if(!is.na(input$height)){  
        if(input$height < global_height_si$numMin) return(global_height_si$numMin)
        if(input$height > global_height_si$numMax) return(global_height_si$numMax)
        return(input$height)
      }else{
        return(NA)
      }
      
    }else{
      return(global_height_si$numVal)
    }
  })
  
  global_height_ip <- reactiveValues(numVal = 8.9, numMin = 1, numMax = 100)
  
  numVal_height_ip <- reactive({
    # if(input$submitbutton > 0 ){
    if(!is.null(input$height)){
      if(!is.na(input$height)){  
        if(input$height < global_height_ip$numMin) return(global_height_ip$numMin)
        if(input$height > global_height_ip$numMax) return(global_height_ip$numMax)
        return(input$height)
      }else{
        return(NA)
      }
      
    }else{
      return(global_height_ip$numVal)
    }
  })
  
  output$height_lim <- renderUI({
    if (is.null(input$units_type))
      return()
    
    switch(input$units_type,
           "SI" =  numericInput("height", 
                                label = HTML("Room ceiling height (m)"), 
                                value =  numVal_height_si(),
                                min = global_height_si$numMin,
                                max = global_height_si$numMax),
           "IP" =  numericInput("height", 
                                label = HTML("Room ceiling height (ft)"), 
                                value =  numVal_height_ip(),
                                min = global_height_ip$numMin,
                                max = global_height_ip$numMax)
           
    )
  })
  
  
  global_area_si <- reactiveValues(numVal = 100, numMin = 1, numMax = 5000)
  
  numVal_area_si <- reactive({
    # if(input$submitbutton > 0 ){
    if(!is.null(input$area)){
      if(!is.na(input$area)){  
        if(input$area < global_area_si$numMin) return(global_area_si$numMin)
        if(input$area > global_area_si$numMax) return(global_area_si$numMax)
        return(input$area)
      }else{
        return(NA)
      }
      
    }else{
      return(global_area_si$numVal)
    }
  })
  
  global_area_ip <- reactiveValues(numVal = 1000, numMin = 1, numMax = 50000)
  
  numVal_area_ip <- reactive({
    # if(input$submitbutton > 0 ){
    if(!is.null(input$area)){
      if(!is.na(input$area)){  
        if(input$area < global_area_ip$numMin) return(global_area_ip$numMin)
        if(input$area > global_area_ip$numMax) return(global_area_ip$numMax)
        return(input$area)
      }else{
        return(NA)
      }
      
    }else{
      return(global_area_ip$numVal)
    }
  })
  
  output$area_lim <- renderUI({
    if (is.null(input$units_type))
      return()
    
    switch(input$units_type,
           "SI" =  numericInput("area", 
                                label = HTML("Room floor area (m<sup>2</sup>)"), 
                                value =  numVal_area_si(),
                                min = global_area_si$numMin,
                                max = global_area_si$numMax),
           "IP" =  numericInput("area", 
                                label = HTML("Romm floor area  (ft<sup>2</sup>)"), 
                                value =  numVal_area_ip(),
                                min = global_area_ip$numMin,
                                max = global_area_ip$numMax)
           
    )
  })
  
  
  global_V_Watt <- reactiveValues(numVal = 80, numMin = 0, numMax = 500) 
  
  numVal_V_Watt <- reactive({
    if(!is.null(input$W)){
      if(!is.na(input$W)){
        if(input$W < global_V_Watt$numMin) return(global_V_Watt$numMin)
        if(input$W > global_V_Watt$numMax) return(global_V_Watt$numMax)     
        return(input$W)
      }else{
        return(NA)
      }
    }else{
      return(global_V_Watt$numVal)
    }
  })
  
  output$V_Watt <- renderUI({
    numericInput("W", 
                 label = "Energy comsumption of one PAC (W)", 
                 value = numVal_V_Watt(),
                 min = global_V_Watt$numMin,
                 max = global_V_Watt$numMax)
  })
  
  # calculation---------------------
  datasetInput <- reactive({
    
    energy_eff <- as.numeric(input$CADR)/as.numeric(input$W)
    k <- 1.67
    
    if(input$input_type == "Efficacy") {
      if (input$units_type == "SI") {
        Output <- fun_effectiveness( as.numeric(input$a),
                                     k,
                                     as.numeric(input$CADR),
                                     as.numeric(input$height) * as.numeric(input$area),
                                     as.numeric(input$N)
        ) 
        if ( !(is.na(Output)|is.na(input$W)) ) {
          paste0("Effectiveness: ", as.character( round(Output*100, digits = 1) ), "%","<br/>","<br/>","<br/>",
                 "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                 "Total operating power consumption: ",round(as.numeric(input$W)*as.numeric(input$N), digits = 1)," W", "<br/>","<br/>",
                 "Power density: ",round(as.numeric(input$W)*as.numeric(input$N)/as.numeric(input$area), digits = 1)," W/m<sup>2</sup>")  
        } else {
          "Please check for missing values in any input parameters!"
        }
      } else {
        Output <- fun_effectiveness( as.numeric(input$a),
                                     k,
                                     as.numeric(input$CADR)*1.69901,
                                     as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                     as.numeric(input$N)
        ) 
        if (!is.na(Output)) {
          paste0("Effectiveness: ", as.character( round(Output*100, digits = 1) ), "%","<br/>","<br/>","<br/>",
                 "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                 "Total operating power consumption: ",round(as.numeric(input$W)*as.numeric(input$N), digits = 1)," W", "<br/>","<br/>",
                 "Power density: ",round(as.numeric(input$W)*as.numeric(input$N)/as.numeric(input$area), digits = 1)," W/ft<sup>2</sup>")  
        } else {
          "Please check for missing values in any input parameters!"
        }
      }
      
    } else {
      if (input$units_type == "SI") {
        Output <- fun_num_PAC(as.numeric(input$a),
                              k,
                              as.numeric(input$CADR),
                              as.numeric(input$height) * as.numeric(input$area),
                              as.numeric(input$eff))
        if (!is.na(Output)) {
          if (ceiling(Output) < 2) {
            paste(as.character( ceiling(Output) ),
                  "portable air cleaner is needed to achieve",
                  round(input$eff, digits = 0), "% effectiveness","<br/>","<br/>","<br/>",
                  
                  "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                  "Total operating power consumption: ",round(as.numeric(input$W)*ceiling(Output), digits = 1)," W", "<br/>","<br/>",
                  "Power density: ",round(as.numeric(input$W)*ceiling(Output)/as.numeric(input$area), digits = 1)," W/m<sup>2</sup>")   
          }else {
            paste(as.character( ceiling(Output) ),
                  "portable air cleaners are needed to achieve",
                  round(input$eff, digits = 0), "% effectiveness" ,"<br/>","<br/>","<br/>",
                  
                  "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                  "Total operating power consumption: ",round(as.numeric(input$W)*ceiling(Output), digits = 1)," W", "<br/>","<br/>",
                  "Power density: ",round(as.numeric(input$W)*ceiling(Output)/as.numeric(input$area), digits = 1)," W/m<sup>2</sup>")   
          } 
        } else {
          "Please check for missing values in any input parameters!"
        }
        
      } else {
        Output <- fun_num_PAC(as.numeric(input$a),
                              k,
                              as.numeric(input$CADR)*1.69901,
                              as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                              as.numeric(input$eff))
        
        if (!is.na(Output)) {
          if (ceiling(Output) < 2) {
            paste(as.character( ceiling(Output) ),
                  "portable air cleaner is needed to achieve",
                  round(input$eff, digits = 0), "% effectiveness","<br/>","<br/>","<br/>",
                  
                  "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                  "Total operating power consumption: ",round(as.numeric(input$W)*ceiling(Output), digits = 1)," W", "<br/>","<br/>",
                  "Power density: ",round(as.numeric(input$W)*ceiling(Output)/as.numeric(input$area), digits = 1)," W/ft<sup>2</sup>")   
          }else {
            paste(as.character( ceiling(Output) ),
                  "portable air cleaners is needed to achieve",
                  round(input$eff, digits = 0), "% effectiveness" ,"<br/>","<br/>","<br/>",
                  
                  "Energy efficiency of the PAC (CADR/W): ",round(energy_eff, digits = 1), "<br/>","<br/>",
                  "Total operating power consumption: ",round(as.numeric(input$W)*ceiling(Output), digits = 1)," W", "<br/>","<br/>",
                  "Power density: ",round(as.numeric(input$W)*ceiling(Output)/as.numeric(input$area), digits = 1)," W/ft<sup>2</sup>")   
          } 
        } else {
          "Please check for missing values in any input parameters!"
        }
      }
    }
  })
  
  
  # plot-------------------------
  k <- 1.67
  
  pp_si <- reactive({
    deposition_df <- data.table(
      k = c(1.67),
      parti_type = c("PM2.5")
    )
    
    df <- data.table(expand.grid(N = c(1:50),  k = c(1.67)))[deposition_df, on = "k"]
    
    df[, eff := fun_effectiveness(as.numeric(input$a), 
                                  k,
                                  as.numeric(input$CADR),
                                  as.numeric(input$height) * as.numeric(input$area),
                                  N)]
    
    if(input$input_type == "Efficacy") {
      df1 <- data.table(N = as.numeric(input$N),
                        
                        eff = fun_effectiveness(as.numeric(input$a), 
                                                k,
                                                as.numeric(input$CADR),
                                                as.numeric(input$height) * as.numeric(input$area),
                                                as.numeric(input$N))* 100)
      
      df2 <- data.table(expand.grid(N = c(as.numeric(input$N),0),
                                    
                                    eff = fun_effectiveness(as.numeric(input$a), 
                                                            k,
                                                            as.numeric(input$CADR),
                                                            as.numeric(input$height) * as.numeric(input$area),
                                                            as.numeric(input$N))* 100) )
      
      g_line <- geom_line(data = df2, aes(N, eff)) 
      g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
      
    }else{
      df1 <- data.table(N = fun_num_PAC(as.numeric(input$a),
                                        k,
                                        as.numeric(input$CADR),
                                        as.numeric(input$height) * as.numeric(input$area),
                                        as.numeric(input$eff)),  
                        eff = as.numeric(input$eff) )
      df2 <- data.table(expand.grid(N = fun_num_PAC(as.numeric(input$a),
                                                    k,
                                                    as.numeric(input$CADR),
                                                    as.numeric(input$height) * as.numeric(input$area),
                                                    as.numeric(input$eff)),  
                                    eff = c(as.numeric(input$eff),min(df$eff)) 
      )
      ) 
      
      g_line <- geom_line(data = df2, aes(N, eff)) 
      g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
    }
    
    ggplot(df) +
      geom_line(aes(N, eff * 100, color = forcats::fct_inorder(parti_type)),size = 1, alpha = 0.7)+
      # geom_line(aes(N, eff * 100, color = parti_type  ) )+
      g_line +
      g_point + 
      
      scale_color_manual("Particle type", values = c("#DCC575") ) +
      scale_y_continuous("Effectiveness (%)", expand = c(0,0)) +
      scale_x_continuous("Number of portable air cleaners", expand = c(0,0)) +
      guides(colour=FALSE) +
      # theme_bw() +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            text = element_text( size = 16),
            legend.key = element_rect(colour = NA, fill = "white"))
    
  })
  
  
  pp_ip <- reactive({
    
    deposition_df <- data.table(
      k = c(1.67),
      parti_type = c("PM2.5")
    )
    df <- data.table(expand.grid(N = c(1:50),  k = c(1.67)))[deposition_df, on = "k"]
    
    df[, eff := fun_effectiveness(as.numeric(input$a), 
                                  k,
                                  as.numeric(input$CADR)*1.69901,
                                  as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                  N)]
    
    if(input$input_type == "Efficacy") {
      df1 <- data.table(N = as.numeric(input$N),
                        
                        eff = fun_effectiveness(as.numeric(input$a), 
                                                k,
                                                as.numeric(input$CADR)*1.69901,
                                                as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                                as.numeric(input$N))* 100)
      
      df2 <- data.table(expand.grid(N = c(as.numeric(input$N),0),
                                    
                                    eff = fun_effectiveness(as.numeric(input$a), 
                                                            k,
                                                            as.numeric(input$CADR)*1.69901,
                                                            as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                                            as.numeric(input$N))* 100) )
      
      g_line <- geom_line(data = df2, aes(N, eff)) 
      g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
      
    }else{
      df1 <- data.table(N = fun_num_PAC(as.numeric(input$a),
                                        k,
                                        as.numeric(input$CADR)*1.69901,
                                        as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                        as.numeric(input$eff)),  
                        eff = as.numeric(input$eff) )
      df2 <- data.table(expand.grid(N = fun_num_PAC(as.numeric(input$a),
                                                    k,
                                                    as.numeric(input$CADR)*1.69901,
                                                    as.numeric(input$height) * as.numeric(input$area)*0.0283168,
                                                    as.numeric(input$eff)),  
                                    eff = c(as.numeric(input$eff),min(df$eff)) 
      )
      ) 
      
      g_line <- geom_line(data = df2, aes(N, eff)) 
      g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
    }
    
    ggplot(df) +
      geom_line(aes(N, eff * 100, color = forcats::fct_inorder(parti_type)),size = 1, alpha = 0.7)+
      # geom_line(aes(N, eff * 100, color = parti_type  ) )+
      g_line +
      g_point + 
      
      scale_color_manual("Particle type", values = c( "#DCC575") ) +
      scale_y_continuous("Effectiveness (%)", expand = c(0,0)) +
      scale_x_continuous("Number of portable air cleaners", expand = c(0,0)) +
      guides(colour=FALSE) +
      # theme_bw() +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            text = element_text( size = 16),
            legend.key = element_rect(colour = NA, fill = "white"))
    
  })
  
  
  output$plot1<-renderPlot({
    if (input$units_type == "SI") {
      pp_si()
    } else {
      pp_ip()
    }
  }, width = "auto", height = "auto")
  
  
  # } , width = 6, height = 6)
  
  
  # 
  # output$tabledata <- renderTable({
  # 
  #     isolate(datasetInput()) 
  #   
  # })
  
  output$txtout <- renderText({
    
    # if (input$submitbutton>0) { 
    HTML(paste( datasetInput() ))
    # } 
    
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)