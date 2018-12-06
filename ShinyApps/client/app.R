rm(list=ls())

set.seed(144)

library(shiny)
library(RMySQL)
library(httr)
library(dplyr)
library(jsonlite)
library(taucharts)
library(highcharter)
library(reshape2)
library(shinyauthr)
library(shinyjs)
library(RagGrid)

config.file <- readLines('~/creds/creds.txt')
stats <- readRDS("~/stats.rds")
qry <- function(string, ip, loc){
  
  library(RMySQL)
  
  if(loc == "client"){
    hst <-ip
    usr <- "shane"
    pwd <- "S13240sx91"
    dbTab <- "dynamic"
    prt <- 6603
  } else if (loc == "rds"){
    hst <-gsub(".*=","",config.file[config.file %>% grepl("db.host",.)])
    usr <- gsub(".*=","",config.file[config.file %>% grepl("db.username",.)])
    pwd <- gsub(".*=","",config.file[config.file %>% grepl("db.pwd",.)])
    dbTab <- "lucerna"
    prt <- 3306
  }
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  result<- dbGetQuery(con, paste0(string))
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
  return(result)
  
}
create_dashboard <- function(stats, travel.date, pred = NULL){
  
  
}

# dataframe that holds usernames, passwords and other user data
{
user_base <- data.frame(
  user = c("intercity", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE
)
}

el <- -c(seq(from = 10, to=3, length.out = 15),rep(3,15),runif(30,10,10),rep(10,100))*runif(160,.8,1.2)

#######CLIENT VIEW
total <- data.frame(day = seq(Sys.Date(),Sys.Date() + 365,by = 1))

x <- seq(0,2*pi,length.out=nrow(total))
total$Quantity <- (cos(x)* 50+50) * runif(nrow(total),.65,1.35)
options(shiny.trace = TRUE, shiny.sanitize.errors = FALSE)

total$type <- "Demand"
df <- rbind(total, data.frame(Quantity = if_else(total$Quantity/1.15 < 35, 35, total$Quantity/1.15),
                           day = total$day,
                           type = "Price"))

forecast_demand <- function(trip.dt){
  start.dt <- Sys.Date() 
  #trip.dt <- Sys.Date() + 20
  n.days <- trip.dt - start.dt
  
  T<-50
  N0<-runif(1,0,50)
  K<-runif(1,130,170)
  rmax.mean<-0.2
  rmax.sd<-0.2
  rmax<- runif(T,0,.2)#rnorm(T,rmax.mean,rmax.sd)
  t<-N<-array(dim=T+1)
  
  #first element is initial value
  N[1]<-N0
  t[1]<-0
  for (i in 1:T)
  {
    N[i+1]<-N[i]+rmax[i]*N[i]*(1-N[i]/K)
    t[i+1]=t[i]+1
  }
  #make it pretty
  t <- seq(trip.dt - 50,trip.dt,"1 day")
  df <- data.frame(Date=t,Quantity=N)
  
  return(df)
  
}

toggle <- "<input type='checkbox' id='inventory_type' checked data-toggle='toggle' data-on='On' 
                                   data-off='Off' data-onstyle='success' data-offstyle='danger' data-width='120'>"

settings <- c("Service has sold out", 
              "Service is 80% Capacity", 
              "Pricing Model has been updated", 
              "Demand forecasts updated",
              "Forecasted Demand & Price")

toggle <- data.frame(Option = settings, Toggle = toggle, Settings = NA)
toggle <- capture.output(print(xtable::xtable(toggle),type="html", 
                               html.table.attributes="class=settings-table",
                               include.rownames = FALSE, 
                               sanitize.text.function = function(x){x}))

ui <- fluidPage(
  tags$head(
    HTML('<link href="https://fonts.googleapis.com/css?family=Kodchasan" rel="stylesheet">
          <link href=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css\" rel=\"stylesheet\">
          <script src=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js\"></script>
         '),
    tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
  ),
  div(class='header',
      HTML('<div id="content">
           <img src="https://static1.squarespace.com/static/5b19e1c23917ee2defbc2e1a/t/5b1a0436575d1f6a61b137cd/1539567080718/?format=1500w" class="ribbon"/>
           </div>'),
      HTML("<div style='padding-top: 4px, padding-bottom:1px;'>
           <h2><b>Client Dashboard</b></h2>
           </div>")),
  shinyjs::useShinyjs(),
  shinyauthr::loginUI(id = "login"),
  conditionalPanel("output.showpanel",
  tabsetPanel(id = "activeTab",
              tabPanel("Overview", value="manage",
                       fluidRow(
                         column(width=1),
                         column(width=8,
                         HTML("<b>Select Date:</b><br>"),
                         div(style="display: inline-block;vertical-align:top;",
                            actionButton("leftDt", "",icon = icon('caret-left'))
                         ),
                         div(style="display: inline-block;vertical-align:top;  width: 120px;",
                            dateInput("forecastDate",label=NULL, value=Sys.Date()+1, min=Sys.Date()+1)
                         ),
                         div(style="display: inline-block;vertical-align:top;",
                            actionButton("rightDt", "",icon = icon('caret-right'))
                         )), 
                         br(),br(),HTML("<span style='color:#00cc00'> Current Sold</span>, <span style='color:#009900'>Forcasted Total</span>")
                       ),
                       fluidRow(
                          column(width=1),
                          column(width=11,
                            tableOutput("forecastTable")
                          )
                       ),
                       fluidRow(
                          column(width=1),
                          column(width=5,
                            HTML("<div id='graph-container'>"),
                            highchartOutput("forecastDay", height="487px"),
                            HTML("</div>")),
                          column(width=5,
                            HTML("<div id='graph-container2'>"),
                            highchartOutput("forecastBreakdown", height="487px"),
                            HTML("</div>")),
                          column(width=1)
                       ),br(),
                       fluidRow(
                          column(width=1),
                          column(width=10,
                          HTML("<div id='graph-container'>"), 
                          highchartOutput("lineGraph"),
                          HTML("</div>"))),
                          column(width=1)),
              tabPanel("Download", value="explorer",
                       column(width=1),
                       column(width=10,
                              div(style="display: inline-block;vertical-align:top;",
                              sliderInput("dlRange",
                                          "Select Date Range:",
                                          min = Sys.Date() + 1,
                                          max = Sys.Date() + 100,
                                          value=c(Sys.Date(),Sys.Date() + 14),
                                          timeFormat="%Y-%m-%d")),
                              div(style="display: inline-block;vertical-align:top;",
                                  br(style="line-height:20px;"),HTML("&nbsp&nbsp&nbsp"),downloadButton("dlData")),
                              tableOutput("dlTable")
                       )),
               tabPanel("Simulate", value="simulator",
                        div(style="display: inline-block;vertical-align:top;",
                          dateInput('prevDate',"Travel Date:",Sys.Date())
                          ),
                        div(style="display: inline-block;vertical-align:top;",
                          numericInput('capacity',"Select Capacity:",15,5,20)
                          ),
                        div(style="display: inline-block;vertical-align:top;",
                          selectInput('prevType',"Select Simulation Type:", c("prediction", "price"))
                        ),
                        div(style="display: inline-block;vertical-align:top;",
                            br(),actionButton('simulate', "Run")
                          ),
                        br(),br(),
                        fluidRow(
                          column(1),
                          column(10,
                                 h2("Forecasted Final Demand: Quantity Sold vs. Days Till Departure (DTD)"),
                            tableOutput('prevTbl'),br(),
                            h2("API calls"),
                            tableOutput('requests')
                          ),
                          column(1)
                        )
                        )
              # tabPanel("Manage", value="explorer", 
              #          column(1),
              #          column(10,RagGridOutput('manageTbl')),
              #          column(1)
              #          ),
              # tabPanel("Performance", value="simulator"),
              # tabPanel("Settings", value="simulator",
              #          #  uiOutput('settingsTable'),
              #          column(5,
              #                 fluidRow(
              #                   div(style="display: inline-block;vertical-align:top;",
              #                       selectInput("emailAccount", "Select Email:", 
              #                                   c("dave@ferriesusa.com", "operations@ferriesusa.com"))
              #                   ),
              #                   div(style="display: inline-block;vertical-align:top;",
              #                       br(),actionButton("saveSettings", "Save")
              #                   ),
              #                   div(style="display: inline-block;vertical-align:top;",
              #                       br(),actionButton("resetSettings", "Reset")
              #                   )
              #                 ),
              #                 h2("Email Settings"),
              #                 HTML(toggle)),
              #          column(5,br(style="line-height:85px;"),h2("Price Settings"))
              # )
           )),
              HTML("<div class='footer'>
                   <img src='http://mellopipelines.com/public/image/footer-hills.png'>
                   </img></div>")
)

server <- function(input,output,session){
  
  ####LOGIN
  {
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
  
  output$showpanel <- eventReactive(credentials()$user_auth, TRUE, ignoreInit = TRUE)
  
  outputOptions(output, "showpanel", suspendWhenHidden = FALSE)
  }
  
  observeEvent(input$leftDt,{
    if(!(input$forecastDate - 1 < Sys.Date())){
      updateDateInput(session, "forecastDate", value = input$forecastDate - 1)
    }
  })
  
  observeEvent(input$rightDt,{
    updateDateInput(session, "forecastDate", value = input$forecastDate + 1)
  })
   
  output$forecastTable <- renderTable({
    
    forDate <- input$forecastDate
    toDate <- as.Date(forDate) + 9
    
    #seats <- round(runif(1,100,170))
    #sold <- round(seats*runif(1,.5,1))
    
    simClient <- "intercity"
    
    maxCache <- qry(paste0("select MAX(`Cache.Time`) from ",simClient,"_demand"), NULL, "rds")
    
    demandData <- qry(paste0("select * from ",simClient,"_demand ", 
                             "where `Travel.Date` between CAST('",forDate,"' AS DATE) ",
                             "and CAST('",toDate,"' AS DATE) and `Cache.Time` = '",maxCache,"'"), NULL, "rds")
    
    #demandData %>% reshape2::dcast(service ~ Travel.Date)
      
    df <- as.data.frame(round(abs(replicate(10, runif(length(unique(demandData$Service)),.85,1.15)))* 100,2))
    names(df) <- seq(from=forDate, to=toDate, by="1 day")
    row.names(df) <- unique(demandData$Service)
    
    for(c in 1:ncol(df)){
      for(r in 1:nrow(df)){
        travel.date <- names(df)[c]
        service <- row.names(df)[r]
        
        dayData <- demandData[demandData$Travel.Date == travel.date & demandData$Service == service, ]
        
        actual <- dayData$Sold
        pred <- round(dayData$Predicted)
        
        day.el <- -2.5#el[as.numeric(as.Date(travel.date) - Sys.Date())]
        
        opt.price <- max(round((1+(((12/pred)-1)/(day.el)))*138,2),117.99)
        if(service == "ICZMZG"){opt.price <- 174.99}
        if(service == "ICZMZC"){opt.price <- 214.99;pred<-"-"}
        
        df[r,c] <-       paste0("$",opt.price,"<br><span style='color:#00cc00'>",actual,"</span>/",
                                "<span style='color:#009900'>",pred,"</span>")
        
      }
    }
    
    # df[] <- lapply(df, function(x){
    #   seats <- round(runif(1,100,170))
    #   sold <- round(seats*runif(1,.5,1))
    #   paste0("$",x,"<br><span style='color:#00cc00'>",sold,"</span>/",
    #          "<span style='color:#009900'>",seats,"</span>")
    #   })

    df }, rownames=T, sanitize.text.function = function(x) x)
  
  output$forecastDay <- renderHighchart({
    
    travel.date <- input$forecastDate
    service <- "ICZMZM"
    
    maxCache <- qry(paste0("select MAX(`Cache.Time`) from intercity_demand"), NULL, "rds")
    
    # df <- forecast_demand(as.Date(forecastDate)) %>%
    #   mutate(Type = if_else(Date < Sys.Date(), "Realized", "Forecast")) %>%
    #   bind_rows(.[.$Date == Sys.Date()-1,] %>% mutate(Type="Forecast")) %>%
    #   arrange(Date)
    
    df <- qry(paste0("
       select * from intercity_past 
       where `Travel.Date` ='",travel.date,"' 
       and Service = '",service,"'
       union 
       select * from intercity_new 
       where `Travel.Date` ='", travel.date,"' 
       and Service = '",service,"'"),NULL,"rds") %>%
      mutate(Purchase.Date = as.Date(Purchase.Date),
             Quantity = cumsum(Quantity)) %>%
      rename(Date = Purchase.Date) %>% mutate(Type="Realized")
    df <- df %>% 
      bind_rows(df %>% filter(row_number()==n()) %>% mutate(Type="Forecast")) %>%
      bind_rows(qry(paste0("select * from intercity_demand 
                   where `Travel.Date`= '",travel.date,"'
                   and Service = '",service,"' 
                   and `Cache.Time` = '",maxCache,"'"),NULL,"rds") %>%
                  rename(Quantity=Predicted, Date=Travel.Date) %>%
                  mutate(Type = "Forecast", Date=as.Date(Date))) %>%
      select(-Cache.Time, -Sold, -time) %>% as.data.frame()
    
    if(nrow(df) == 1){
      
      df <- df %>% mutate(Date = Sys.Date() + 1, Quantity = 0, Type="Realized") %>%
        bind_rows(
          df %>% mutate(Date = Sys.Date() + 1, Quantity = 0)
        ) %>%
        bind_rows(df)
      
    }
    
    df <- df %>% mutate(Quantity = round(Quantity))
    
    highchart() %>%
      hc_add_series(df[df$Type=="Realized",], type="line", hcaes(x=Date, y=Quantity),
                    name="Actual") %>%
      hc_add_series(df[df$Type=="Forecast",], type="line", hcaes(x=Date, y=Quantity), 
                    dashStyle="shortdot", name="Forecast") %>%
      hc_plotOptions(line = list(marker = list(enabled= F))) %>%
      hc_xAxis(type='datetime',labels=list(rotation=90, format="{value:%b-%d}")) %>%
      hc_title(text=HTML(paste0("<div><b>Forecasted Demand</b></div>"))) %>%
      hc_subtitle(text=HTML(paste0("<div>",travel.date,"</div>"))) %>%
      hc_tooltip(valueDecimals= 0) %>%
      hc_yAxis(
        plotLines = list(
          list(
          value= max(df$Quantity),
          color= '#d9534f',
          dashStyle= 'longdash',
          width= 2,
          label= list(text = "This is a plotBand")
        )
      ))
  })
  
  output$forecastBreakdown <- renderHighchart({
    
    travel.date <- as.character(input$forecastDate)
    
 #   hc <- create_dashboard(stats,travel.date)
    
    holidays <- stats$holidays
    school <- stats$school
    
    dow <- as.numeric(format(as.Date(travel.date),"%w"))
    travel.month <- as.numeric(format(as.Date(travel.date), "%m"))
    daydiff <- as.numeric(as.Date(travel.date) - Sys.Date())
    holiday <- if_else((holidays$public_holiday[holidays$date == travel.date] == "Yes" |
                          school$school_holiday[school$date == travel.date] == "Yes"), "Yes", "No")
    
    hc <- data.frame(
      Factor = c("Day.of.Week", "Travel.Month", "Lead.Time", "Holiday"),
      Effect = c(stats$day_of_week[[dow+1]],stats$month_dmd[[travel.month]],
                 stats$daydiff$cumsum[stats$daydiff$daydiff==daydiff] - 1,
                 if_else(holiday == "Yes", stats$holiday,stats$nonholiday)-1)) %>%
      mutate(Effect = Effect * 100, 
             Color = if_else(Effect < 1, "#d9534f", "#5cb85c")) %>%
      hchart(type="bar", hcaes(x=Factor, y=Effect, color=Color)) %>%
      hc_yAxis(labels = list(format='{value}%'),title=list(text="Relative to Avg. Demand")) %>%
      hc_xAxis(title=list(text="")) %>%
      hc_title(text=HTML(paste0("<div><b>Factor Effect</b></div>"))) %>%
      hc_subtitle(text=HTML(paste0("<div>",travel.date,"</div>"))) %>%
      hc_tooltip(valueDecimals= 2) %>%
      hc_plotOptions(
        series=list(
          dataLabels=list(
            enabled= T,
            #format='{point.y:.1f}%',
            formatter= JS("function() {
            if(this.y > 0){
              return '+' + Highcharts.numberFormat(this.y, 1) + '%';
            } else {
              return Highcharts.numberFormat(this.y, 1) + '%';
            }
        }"
            ))
        )
      )
    
    return(hc)
   
  })
  
  output$lineGraph <- renderHighchart({
    
    simClient <- "intercity"
    
    maxCache <- qry(paste0("select MAX(`Cache.Time`) from ",simClient,"_demand"), NULL, "rds")
    
    demandData <- qry(paste0("select * from ",simClient,"_demand ",
                             "WHERE `Cache.Time` = '",maxCache,"'"), NULL, "rds") %>%
                  mutate(Price =(1+(((12/Predicted)-1)/(-2.5)))*138,
             Price = if_else(Price < 117.99, 117.99, round(Price,2)),
             Travel.Date = as.Date(Travel.Date))
      
      demandData %>%
        hchart(type="spline", hcaes(y=Price,x=Travel.Date), name="Price", showInLegend=T) %>%
        hc_add_series(data=demandData, type="spline",hcaes(y=Predicted,x=Travel.Date),
                      name="Demand", yAxis=1, showInLegend=T) %>%
        hc_yAxis_multiples(
          list(title=list(text="Dynamic Price")),
          list(title=list(text="Predicted Demand"),
               showLastLabel = FALSE, opposite = TRUE)
        ) %>%
        hc_xAxis(title=list(text="Travel Date"),type='datetime',
                 labels = list(format = '{value:%m/%Y}'), tickInterval=28*24*3600*1000) %>%
        hc_title(text="Forecast Horizon")
    
    # tauchart(df) %>% 
    #   tau_line("day", "Quantity", "type") %>% 
    #   tau_guide_x(label="Date", tick_format="%Y-%m") %>%
    #   tau_guide_y(label="Quantity/Price") %>%
    #   tau_legend() %>%
    #   tau_tooltip()
    
  })
  
  output$dlTable <- renderTable({
    
    forDate <- input$dlRange[1]
    toDate <- input$dlRange[2]
    
    # start <- Sys.Date()
    # end <- Sys.Date() + 10
    
    # paste("Service", 1:5)
    # 
    # df <- as.data.frame(replicate(5, runif(as.numeric(end - start)+1,85,115))) %>%
    #   setNames(paste("Service", 1:5))
    # 
    # row.names(df) <- paste0("<b>",seq(start, end, "1 day"),"</b>")
    
    #return(df)
    
    simClient <- "intercity"
    
    maxCache <- qry(paste0("select MAX(`Cache.Time`) from ",simClient,"_demand"), NULL, "rds")
    
    demandData <- qry(paste0("select * from ",simClient,"_demand ", 
                             "where `Travel.Date` between CAST('",forDate,"' AS DATE) ",
                             "and CAST('",toDate,"' AS DATE) and `Cache.Time` = '",maxCache,"'"), NULL, "rds")
    
    reshape2::dcast(demandData, Travel.Date ~ Service, value.var = "Predicted") %>%
      mutate(ICZMZM = round((1+(((12/ICZMZM)-1)/(-2.5)))*138,2),
             ICZMZM = if_else(ICZMZM < 117.99, "$117.99", paste0("$", ICZMZM)),
             ICZMZC = "$174.99", ICZMZG = "$214.99",
             Travel.Date = paste("<b>",Travel.Date,"</b>")) %>% 
      select (Travel.Date, ICZMZM, ICZMZC, ICZMZG)
    
    
  }, rownames=F, sanitize.text.function = function(x) x)
  
  output$manageTbl <- renderRagGrid({
    
    forDate <- input$dlRange[1]
    toDate <- input$dlRange[2]
    
    simClient <- "intercity"
    service <- "ICZMZM"
    
    maxCache <- qry(paste0("select MAX(`Cache.Time`) from ",simClient,"_demand"), NULL, "rds")
    
    demandData <- qry(paste0("select * from ",simClient,"_demand ", 
                             "where `Travel.Date` between CAST('",forDate,"' AS DATE) ",
                             "and CAST('",toDate,"' AS DATE) and `Cache.Time` = '",maxCache,"' ",
                             "and Service = '",service,"'"), NULL, "rds")
    
    demandData <- demandData %>% select(Travel.Date, Sold, Predicted) %>% 
      mutate(Adjustment = 0, Adj.Pred = Adjustment + Predicted)
    
    colOpts = list(
      Predicted=list(field='a'),
      Adjustment=list(editable=TRUE,field='b'),
      Adj.Pred=list(valueGetter='data.a + data.b',aggFunc='sum'))
    aggrid(demandData, colOpts = colOpts)
  })
  
  output$settingsTable <- renderTable({
    
    
    toggle <- "<input type='checkbox' id='inventory_type' checked data-toggle='toggle' data-on='On' 
                                   data-off='Off' data-onstyle='success' data-offstyle='danger' data-width='120'>"
    
    data.frame(Setting = paste0("Setting ",1:10), Toggle = toggle)
    
  },sanitize.text.function = function(x) x)
  
  output$prevTbl <- renderTable({
    
    client_ip <- "34.221.111.126"
    
    input$simulate
    
    isolate({
      travel.date <- input$prevDate
      capacity <- input$capacity
      prevType <- input$prevType
    })
    
    results <- matrix(NA, nrow=30,ncol=16)
    reqs <- c()
    
    
    withProgress(message = 'Running Simulation', value = 0, {
      # Number of times we'll go through the loop
      n <- 15
    
    for(cum in 0:15){
      for(lt in 1:30){
        
    req <- GET(paste0("http://",client_ip,"/nnpred"), 
                       query = list("purchase.date" = travel.date-lt,
                                    "travel.date" = as.character(travel.date),
                                    "cumulative" = cum,
                                    "service" = as.character("ICZMZM"),
                                    "qty" = 0,
                                    "capacity" = 12))
    
    reqs <- c(reqs, req$url)
    
    res <- content(req)
    
    pred <- as.numeric(res[[prevType]][[1]])
      
    results[lt,cum+1] <- pred
    
      }
      
      incProgress(1/n, detail = paste("Computing ",prevType," for day ", cum," of 15."))
    }
    
    })
    
    
    results <- as.data.frame(results)
    names(results) <- paste0("QtySold=",0:15)
    row.names(results) <- paste0("DTD=",1:30)
    
    output$requests <- renderTable(reqs)
    
    return(results)

  }, rownames=T)
}

shinyApp(ui,server)