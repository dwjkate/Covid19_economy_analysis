# Package Loading

source(file = "../data/shiny_packages.R")

# Map Dependencies

source(file = "../data/map_dependencies.R")

# Small Business Open

source(file = "../data/small_business_open.R")

# Employment 

source(file = "../data/employment.R")

# Consumer Spending

source(file = "../data/consumer_spending.R")

vignette_link <- c("https://github.com/21Sp-STAT-413-613/final-project-covid-economic/blob/main/vignette/vignette.pdf")

# UI Elements

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0, 0, 0)"
  ,primaryFontColor = "rgb(0, 0, 0)"
  ,infoFontColor = "rgb(0, 0, 0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(252, 242, 216)"
  
  ### header
  ,logoBackColor = "rgb(20, 30, 48)"
  
  ,headerButtonBackColor = "rgb(255, 128, 0)"
  ,headerButtonIconColor = "rgb(200, 50, 0)"
  ,headerButtonBackColorHover = "rgb(200, 50, 0)"
  ,headerButtonIconColorHover = "rgb(255, 128, 0)"
  
  ,headerBackColor = "rgb(200, 50, 0)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(43, 88, 118)"
    ,colorMiddle = "rgb(78, 67, 118)"
    ,colorEnd = "rgb(78, 67, 118)"
    ,colorStartPos = 0
    ,colorMiddlePos = 80
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(43, 88, 118, 1)"
    ,colorMiddle = "rgba(78, 67, 118, 1)"
    ,colorEnd = "rgba(78, 67, 118, 1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 80
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(43, 88, 118, 1)"
    ,colorMiddle = "rgba(43, 88, 118, 1)"
    ,colorEnd = "rgba(78, 67, 118, 1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

header <- dashboardHeader(title = "Covid-19 Economic Analysis",
                          titleWidth = 300,
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Vignette",
                                         message = "Right click to open in a new tab",
                                         icon = icon("file_alt"),
                                         href = vignette_link, # this needs to be our vignette
                                       ) # messageItem 1
                                       
                          ) # dropdownMenu
                          ) # end header

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tracker", tabName = "tracker", icon = icon("atlas"),
             menuItem("United States", tabName = "trackerusa", icon = icon("map-marked-alt"))#,
            # menuItem("State Specific", tabName = "trackerstate", icon = icon("map-pin"))
             ),
    menuItem("Small Business Open", tabName = "smallbiz", icon = icon("arrow-circle-right"),
             menuItem("Explatory Data Analysis", tabName = "edasmallbiz", icon = icon("chart-pie")),
             menuItem("Statistical Analysis", tabName = "statsmallbiz", icon = icon("chart-bar"))),
    menuItem('Consumer Spending', tabName = 'consumer_spending', icon = icon("arrow-circle-right"),
             menuItem("Explatory Data Analysis", tabName = "edaspending", icon = icon("chart-pie")),
             menuItem("Statistical Analysis", tabName = "statspending", icon = icon("chart-bar"))),
    menuItem('Employment', tabName = 'employment', icon = icon("arrow-circle-right"),
             menuItem("Explatory Data Analysis", tabName = "edaemployment", icon = icon("chart-pie")),
             menuItem("Statistical Analysis", tabName = "statemployment", icon = icon("chart-bar")))
  ) # sidebarMenu
) # end sidebard


body <- dashboardBody(customTheme,
  tabItems(
    # First tab content
    tabItem(tabName = "trackerusa",
            fluidRow(
              leafletOutput("plot1"),
              box(title = "Covid-19 Economic Analysis", width = 12, 
                  solidHeader = TRUE, status = "danger",
                  "Covid-19 Economic Analysis App analyzes how COVID-19 positive cases impacted the U.S. economy, focusing on indicators such
as consumer spending, open small business, and employment. The aim of the app is to discover how the
number of positive COVID-19 cases influenced consumer spending across industries, as well as revealing how
COVID-19 positive cases impacted small businesses or changes in employment.")
            ) # fluidrow Tracker
    ),
    
    # Second tab content
    tabItem(tabName = "edasmallbiz",
            fluidRow(
              tabBox(width = "50%",
                     id = "tabboxsmallbiz1",
                     height = "250px",
                     # tabPanel("Data Overview", 
                     #           verbatimTextOutput("smallbizoverview")
                     # ),
                     tabPanel("Descriptive Statistics", 
                              dataTableOutput("smallbizdesc"),
                     ), # Tab Panel Desc Stat Small Biz
                     # tabPanel("Correlation",
                     #          plotOutput("smallbizcorr")
                     # ), # Tab Panel Correlation
                     tabPanel("Descriptive Plot",
                              plotOutput("smallbizdescplot"),
                              varSelectInput("smallbizvary", "Select Variable",
                                             data = small_business_open,
                                             selected = "All Revenue"),
                              selectInput("smallbizstate", "Select State",
                                          choices = state.abb, selected = "AL")
                     ) # Tab Panel Descriptive Plot
              ) # tabbox Descriptive Statistics
            ) # fluidrow Small Biz
    ), # tabItem 2
    
    tabItem(tabName = "statsmallbiz",
            fluidRow(
              tabBox(
                width = "50%",
                id = "tabboxsmallbiz2",
                height = "250px",
                # Tab Panel Descriptive Plot
                tabPanel("Density Plot",
                         plotOutput("smallbizdensityplot"),
                         varSelectInput("smallbizdensityvar", "Select Variable",
                                        data = small_business_open,
                                        selected = "All Revenue"),
                ), # Tab Panel Density Plot
                tabPanel("Correlation Matrix",
                         plotOutput("smallbizcormatrixopen"),
                         plotOutput("smallbizcormatrixrevenue")
                ), # Tab Panel Correlation Matrix
                tabPanel("Bivariate Analysis",
                         plotOutput("smallbizbivariate"),
                         varSelectInput("smallbizbivarx", "Select X-Variable",
                                        data = small_business_open,
                                        selected = "All Revenue"),
                         varSelectInput("smallbizbivary", "Select Y-Variable",
                                        selected = "All Open",
                                        data = small_business_open)
                ), # Tab Panel Time Series
                tabPanel("Time Series",
                         plotOutput("smallbiztimeseries"),
                         varSelectInput("smallbiztimeseriesy", "Select Y-Variable",
                                        selected = "All Revenue",
                                        data = small_business_open)
                )
            )
          )

    ),

    #3rd tab (consumer spending)
    tabItem(tabName = "edaspending",
            fluidRow(
              tabBox(width = "50%",
                     id = "tabboxspending",
                     height = "250px",
                     # tabPanel("Data Overview", 
                     #           textOutput("spendingoverview"),
                     #  ),
                     tabPanel("Descriptive Statistics", 
                              dataTableOutput("spendingdesc"),
                     ), # Tab Panel Desc Stat
                     # tabPanel("Correlation",
                     #          plotOutput("spendingcorr")
                     # ), # Tab Panel Correlation
                     tabPanel("Descriptive Plot",
                              plotOutput("spendingdescplot"),
                              varSelectInput("spendingvary", "Select Variable",
                                             data = consumer_spending,
                                             selected = "All Merchant Catergory"),
                              selectInput("spendingstate", "Select State",
                                          choices = state.abb, selected = "AL")
                     ) # Tab Panel Descriptive Plot
              ) # tabbox Descriptive Statistics
            ) # fluidrow Consumer spending
    ), #tab 3
    
    
    tabItem(tabName = "statspending",
            fluidRow(
              tabBox(
                width = "50%",
                id = "tabboxspending2",
                height = "250px",
                tabPanel("Density Plot",
                         plotOutput("spendingdensityplot"),
                         varSelectInput("spendingdensityvar", "Select Variable",
                                        data = consumer_spending,
                                        selected = "All Merchant Catergory"),
                ), # Tab Panel Density Plot
                tabPanel("Correlation Matrix",
                         plotOutput("spendingcormatrix")
                ), # Tab Panel Correlation Matrix
                tabPanel("Bivariate Analysis",
                         plotOutput("spendingbivariate"),
                         varSelectInput("spendingbivarx", "Select X-Variable",
                                        data = consumer_spending,
                                        selected = "All Merchant Catergory"),
                         varSelectInput("spendingbivary", "Select Y-Variable",
                                        selected = "Grocery",
                                        data = consumer_spending)
                ), # Tab Panel Time Series
                tabPanel("Time Series",
                         plotOutput("spendingtimeseries"),
                         varSelectInput("spendingtimeseriesy", "Select Y-Variable",
                                        selected = "All Merchant Catergory",
                                        data = consumer_spending)
                ) 
              )
            )
            
    ),
    
    
    #4th tab (employment)
    tabItem(tabName = "edaemployment",
            fluidRow(
              tabBox(width = "50%",
                     id = "tabboxemplyoment",
                     height = "250px",
                      # tabPanel("Data Overview", 
                      #         textOutput("emplyomentoverview"),
                      # ),
                     tabPanel("Descriptive Statistics", 
                              dataTableOutput("emplyomentdesc"),
                     ), # Tab Panel Desc Stat
                     # tabPanel("Correlation",
                     #          plotOutput("employmentcorr")
                     # ), # Tab Panel Histograms
                     tabPanel("Descriptive Plot",
                              plotOutput("employmentdescplot"),
                              varSelectInput("employmentvary", "Select Variable",
                                             data = employment,
                                             selected = "All Worker"),
                              selectInput("employmentstate", "Select State",
                                          choices = state.abb, selected = "AL")
                     ) # Tab Panel Descriptive Plot
              ) # tabbox Descriptive Statistics
            ) # fluidrow Consumer spending
    ), #tab 3
    
    
    tabItem(tabName = "statemployment",
            fluidRow(
              tabBox(
                width = "50%",
                id = "tabboxemployment2",
                height = "250px",
                tabPanel("Density Plot",
                         plotOutput("employmentdensityplot"),
                         varSelectInput("employmentdensityvar", "Select Variable",
                                        data = employment,
                                        selected = "All Worker"),
                ), # Tab Panel Density Plot
                tabPanel("Correlation Matrix",
                         plotOutput("employmentcormatrix")
                ), # Tab Panel Correlation Matrix
                tabPanel("Bivariate Analysis",
                         plotOutput("employmentbivariate"),
                         varSelectInput("employmentbivarx", "Select X-Variable",
                                        data = employment,
                                        selected = "All Worker"),
                         varSelectInput("employmentbivary", "Select Y-Variable",
                                        selected = "All Worker",
                                        data = employment)
                ), # Tab Panel Time Series
                tabPanel("Time Series",
                         plotOutput("employmenttimeseries"),
                         varSelectInput("employmenttimeseriesy", "Select Y-Variable",
                                        selected = "All Worker",
                                        data = employment)
                ) 
              )
            )
            
    )
    
    
  ) #tabItems
) # end dashboardBody

# UI Dashboard

ui <- dashboardPage(
                    header, sidebar, body
                    ) # end dashboardPage


# Server

server <- function(input, output, session) {
 
# Mapping Functions and Dependencies  
   
  time_series_plotter <- function(df, x1, x2, x3, x4, y1, ttl){
    df %>%
      ggplot(aes(x = {{ x1 }}, y = {{ y1 }}))+
      geom_line(aes(color = "New Covid Case"), size = 1.1)+
      geom_line(aes(y = {{ x2 }}, color = "All Revenue"), size = 1.1)+
      geom_line(aes(y = {{ x3 }}, color = "All Spending"), size = 1.1)+
      geom_line(aes(y = {{ x4 }}, color = "All Employment"), size = 1.1)+
      ggtitle({{ttl}})+
      ylab("Count")+
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            panel.background = element_rect(fill = "lightblue"))+
      scale_colour_viridis_d()
  }
  
  small_business_open %>%
    select(Date, statename, stateabbrev, `revenue_all`) %>%
    inner_join(consumer_spending %>%
                 select(Date, statename, stateabbrev, `spend_all`),
               by = c("Date", "statename", "stateabbrev")) %>%
    inner_join(employment %>%
                 select(Date, statename, stateabbrev, `emp`, `New Covid Case`),
               by = c("Date", "statename", "stateabbrev")) -> nested_combo
  
  nested_combo %>%
    mutate(`New Covid Case` = replace_na(`New Covid Case`, 0)) %>%
    group_by(statename) %>%
    mutate(`New Covid Case` = `New Covid Case`/max(`New Covid Case`, na.rm = TRUE)) %>%
    ungroup() %>%
    nest_by(statename) %>%
    mutate(plot = list(time_series_plotter(data, x1 = Date, 
                                           x2 = `revenue_all`, 
                                           x3 = `spend_all`,
                                           x4 = `emp`, 
                                           y1 = `New Covid Case`,
                                           ttl = `statename`))) %>%
    rename("NAME" = "statename") %>%
    select(NAME, plot) -> nested_combo_plots
  
  us_states@data %>%
    left_join(nested_combo_plots, by = "NAME") %>%
    left_join(geoID %>% select(statename, state_pop2019), by = c("NAME" = "statename")) -> us_states@data
  
  us_states@data %>%
    filter(!NAME == "Puerto Rico") -> us_states@data
  
  us_pal <- colorNumeric(palette = "YlOrRd", 
                         domain = us_states$state_pop2019)
  
  
  output$plot1 <- renderLeaflet({
    leaflet(us_states) %>%
      setView(-96, 37.8, 4) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~us_pal(us_states$state_pop2019),
        weight = 2,
        opacity = 4.5,
        color = "#D9849B",
        dashArray = "3",
        fillOpacity = 0.7,
        group = "popup",
        highlight = highlightOptions(
          weight = 3,
          color = "#59DBF1",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = us_states$NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = us_pal, values = ~us_states$state_pop2019, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addPopupGraphs(us_states$plot, group = "popup", width = 500)
  })
  

  
  # EDA Small Business Outputs
  
  observe({
    updateVarSelectInput(session, "smallbizvary", data = small_business_open %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "smallbizvarx", data = small_business_open %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "smallbizdensityvar", data = small_business_open %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "smallbizbivarx", data = small_business_open %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "smallbizbivary", data = small_business_open %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "smallbiztimeseriesy", data = avg_small_business_open_covid %>%
                           select(-c(Date, sum_case)))
  })

  
  output$smallbizdesc <- renderDataTable(
    summary_small_business %>%
      arrange(desc(Average)), filter = list(position = "bottom",
                                         clear = FALSE),
    rownames = FALSE
  )
  
  output$smallbizdescplot <- renderPlot({
    small_business_open %>%
      filter(stateabbrev == input$smallbizstate) %>%
      ggplot(aes(x = Date, y = !!input$smallbizvary))+
      geom_point()+
      theme_minimal()+
      geom_vline(aes(xintercept = ymd("2020-03-13"),
                     color = "1. March 13, National Emergency"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-03-27"),
                     color = "2. March 27, CARES Act Enabled"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-04-15"),
                     color = "3. April 15, 1st Stimulus Check"), size = 1)+ 
      geom_vline(aes(xintercept = ymd("2021-01-04"),
                     color = "4. January 4, 2nd Stimulus Check"), size = 1)+
      geom_vline(aes(xintercept = ymd("2021-03-17"),
                     color = "5. March 17, 3rd Stimulus Check"), size = 1)+
      theme_minimal()+
      ggtitle("State Outlook")+
      theme(legend.position = "bottom") +
      xlab("Date")
    
  })
  
  output$smallbizcorr <- renderPlot({
    small_business_cor <- cor(small_business_quant_only)
    ggcorrplot(small_business_cor)
  })
  
  output$smallbizdensityplot <- renderPlot({
    small_business_open %>%
      ggplot(aes(x = Date, y = !!input$smallbizdensityvar))+
      stat_density2d(aes(fill = stat(density)), geom = "raster", contour = FALSE)+
      scale_fill_viridis_c()+
      coord_cartesian(expand = FALSE)+
      geom_point(shape = ".", col = "white")+
      xlab("Date")+
      ggtitle("Heatmap Across the United States")
  })
  
  output$smallbizcormatrixopen <- renderPlot({
    cor(small_business_quant_only %>%
          select(starts_with("revenue"), `New Covid Case`, `New Covid Death`),
        use = "complete.obs", method = "pearson") -> small_business_cor
    
    ggcorrplot(small_business_cor)
  })
  
  output$smallbizcormatrixrevenue <- renderPlot({
    ggcorrplot(small_business_cor)
  })
  
  # Small Biz Bivariate Function
  
  small_var_x <- function(x){
    small_business_open[[input$smallbizbivarx]]
  }
  
  small_var_y <<- function(x){
    small_business_open[[input$smallbizbivary]]
  }
  
  small_gg_x <- function(x){
    !!input$smallbizbivarx
  }
  
  small_gg_y <- function(x){
    !!input$smallbizbivary
  }
  
  small_aes <- function(x){
    ggplot(small_business_open, aes(x = !!input$smallbizbivarx, y = !!input$smallbizbivary)) + theme_bw()
  }
  
  small_plotter <- function(...){
    if (!is.numeric(small_var_x()) & !is.numeric(small_var_y()))
    {small_aes() + geom_jitter()}
    else if (!is.numeric(small_var_x()) & is.numeric(small_var_y()))
    {small_aes() + geom_boxplot()}
    else if (is.numeric(small_var_x()) & is.numeric(small_var_y()))
    {small_aes() + geom_point(alpha = 0.5)}
  }
  
  output$smallbizbivariate <- renderPlot({
    small_plotter()
  })
  
  # Small Biz Time Series
    
  output$smallbiztimeseries <- renderPlot({
    
    avg_small_business_open_covid %>%
      select(Date, !!input$smallbiztimeseriesy, covid_percent_change) %>%
      gather(key="var", value = "value", -Date) -> avg_small_business_open_covid_new
   
    avg_small_business_open_covid_new %>%
      filter(Date > "2020-02-29") %>%
      ggplot(aes(x=Date, y=value)) +
      geom_line(aes(color=var)) +
      # geom_vline(aes(xintercept = ymd("2020-03-13"),
      #                color = "March 10, National Emergency"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-04-16"),
      #                color = "April 16, Gating Criteria"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-05-28"),
      #                color = "May 28, Deaths = 100k"), size = 1) +
      scale_color_manual(values = c("tomato2", "steelblue")) +
      xlab("Date") +
      ylab("Variable Change")+
      labs(color = "Variable")+
      theme_minimal() 
    
    
  })
  
  # Small Biz Open End

  
  # Consumer Spending Start
  observe({
    updateVarSelectInput(session, "spendingdensityvar", data = consumer_spending %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "spendingvarx", data = consumer_spending %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "spendingbivary", data = consumer_spending %>%
                           select(-c(Date:statefips)))
  })
  
  observe({
    updateVarSelectInput(session, "spendingtimeseriesy", data = avg_consumer_spending_covid %>%
                           select(-c(Date, sum_case)))
  })
  
  #EDA consumer spending
  
  output$spendingdesc <- renderDataTable(
    summary_consumer_spending %>%
      arrange(desc(Average)), filter = list(position = "bottom",
                                         clear = FALSE),
    rownames = FALSE
  )
  
  # output$spendingoverview <- renderText ({ paste("Testing")
  #   paste("testing2")})
  
  output$spendingdescplot <- renderPlot({
    consumer_spending %>%
      filter(stateabbrev == input$spendingstate) %>%
      ggplot(aes(x = Date, y = !!input$spendingvary))+
      geom_point()+
      theme_minimal()+
      geom_vline(aes(xintercept = ymd("2020-03-13"),
                     color = "1. March 13, National Emergency"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-03-27"),
                     color = "2. March 27, CARES Act Enabled"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-04-15"),
                     color = "3. April 15, 1st Stimulus Check"), size = 1)+ 
      geom_vline(aes(xintercept = ymd("2021-01-04"),
                     color = "4. January 4, 2nd Stimulus Check"), size = 1)+
      geom_vline(aes(xintercept = ymd("2021-03-17"),
                     color = "5. March 17, 3rd Stimulus Check"), size = 1)+
      theme_minimal()+
      ggtitle("State Outlook")+
      theme(legend.position = "bottom") +
      xlab("Date")
    
  })
  
  output$spendingcorr <- renderPlot({
    spending_cor <- cor(consumer_spending_quant_only)
    ggcorrplot(spending_cor)
  })
  # EDA Consumer Spending End
  
  # Consumer Spending Stat-Analysis Start
  
  # Consumer spending density plot
  output$spendingdensityplot <- renderPlot({
    consumer_spending %>%
      ggplot(aes(x = Date, y = !!input$spendingdensityvar))+
      stat_density2d(aes(fill = stat(density)), geom = "raster", contour = FALSE)+
      scale_fill_viridis_c()+
      coord_cartesian(expand = FALSE)+
      geom_point(shape = ".", col = "white")+
      xlab("Date")+
      ggtitle("Heatmap Across the United States")
  })
  

  # Consumer spending correlation matrix
  output$spendingcormatrix <- renderPlot({
    cor(consumer_spending_quant_only) -> consumer_spending_corr
    
    ggcorrplot(consumer_spending_corr)
  })
  
  # Consumer Spending Bivariate Function
  consumer_spending_var_x <- function(x){
    consumer_spending[[input$spendingbivarx]]
  }
  
  consumer_spending_var_y <<- function(x){
    consumer_spending[[input$spendingbivary]]
  }
  
  spending_gg_x <- function(x){
    !!input$spendingbivarx
  }
  
  spending_gg_y <- function(x){
    !!input$spendingbivary
  }
  
  spending_aes <- function(x){
    ggplot(consumer_spending, aes(x = !!input$spendingbivarx, y = !!input$spendingbivary)) + theme_bw()
  }
  
  spending_plotter <- function(...){
    if (!is.numeric(consumer_spending_var_x()) & !is.numeric(consumer_spending_var_y()))
    {spending_aes() + geom_jitter()}
    else if (!is.numeric(consumer_spending_var_x()) & is.numeric(consumer_spending_var_y()))
    {spending_aes() + geom_boxplot()}
    else if (is.numeric(consumer_spending_var_x()) & is.numeric(consumer_spending_var_y()))
    {spending_aes() + geom_point(alpha = 0.5)}
  }
  
  output$spendingbivariate <- renderPlot({
    spending_plotter()
  })
  
  # Consumer Spending Time Series
  
  output$spendingtimeseries <- renderPlot({
    
    avg_consumer_spending_covid %>%
      select(Date, !!input$spendingtimeseriesy, covid_percent_change) %>%
      gather(key="var", value = "value", -Date) -> avg_consumer_spending_covid_new
    
    avg_consumer_spending_covid_new %>%
      filter(Date > "2020-02-29") %>%
      ggplot(aes(x=Date, y=value)) +
      geom_line(aes(color=var)) +
      # geom_vline(aes(xintercept = ymd("2020-03-13"),
      #                color = "March 10, National Emergency"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-04-16"),
      #                color = "April 16, Gating Criteria"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-05-28"),
      #                color = "May 28, Deaths = 100k"), size = 1) +
      scale_color_manual(values = c("tomato2", "steelblue")) +
      xlab("Date") +
      ylab("Variable Change")+
      labs(color = "Variable")+
      theme_minimal() 
    
  })

  # Consumer Spending stat End
  
  # Employment Start
  
  observe({
    updateVarSelectInput(session, "employmenttimeseriesy", data = avg_employment_covid %>%
                           select(-c(Date, sum_case)))
  })
  #EDA employment

  
  output$emplyomentdesc <- renderDataTable(
    summary_employment %>%
      arrange(desc(Average)), filter = list(position = "bottom",
                                         clear = FALSE),
    rownames = FALSE
  )
  
  # output$emplyomentoverview <- renderText ({ paste("Testing") })
  
  output$employmentdescplot <- renderPlot({
    employment %>%
      filter(stateabbrev == input$employmentstate) %>%
      ggplot(aes(x = Date, y = !!input$employmentvary))+
      geom_point()+
      theme_minimal()+
      geom_vline(aes(xintercept = ymd("2020-03-13"),
                     color = "1. March 13, National Emergency"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-03-27"),
                     color = "2. March 27, CARES Act Enabled"),size = 1)+
      geom_vline(aes(xintercept = ymd("2020-04-15"),
                     color = "3. April 15, 1st Stimulus Check"), size = 1)+ 
      geom_vline(aes(xintercept = ymd("2021-01-04"),
                     color = "4. January 4, 2nd Stimulus Check"), size = 1)+
      geom_vline(aes(xintercept = ymd("2021-03-17"),
                     color = "5. March 17, 3rd Stimulus Check"), size = 1)+
      theme_minimal()+
      ggtitle("State Outlook")+
      theme(legend.position = "bottom") +
      xlab("Date")
  })
  
  output$employmentcorr <- renderPlot({
    employment_cor <- cor(employment_quant_only)
    ggcorrplot(employment_cor)
  })
  
  # Employment Stat-Analysis Start
  
  # Employment density plot
  output$employmentdensityplot <- renderPlot({
    employment %>%
      ggplot(aes(x = Date, y = !!input$employmentdensityvar))+
      stat_density2d(aes(fill = stat(density)), geom = "raster", contour = FALSE)+
      scale_fill_viridis_c()+
      coord_cartesian(expand = FALSE)+
      geom_point(shape = ".", col = "white")+
      xlab("Date")+
      ggtitle("Heatmap Across the United States")
  })
  
  
  # Employment correlation matrix
  output$employmentcormatrix <- renderPlot({
    cor(employment_quant_only) -> employment_corr
    
    ggcorrplot(employment_corr)
  })
  
  # Employment Bivariate Function
  employment_var_x <- function(x){
    employment[[input$employmentbivarx]]
  }
  
  employment_var_y <<- function(x){
    employment[[input$employmentbivary]]
  }
  
  employment_gg_x <- function(x){
    !!input$employmentbivarx
  }
  
  employment_gg_y <- function(x){
    !!input$employmentbivary
  }
  
  employment_aes <- function(x){
    ggplot(employment, aes(x = !!input$employmentbivarx, y = !!input$employmentbivary)) + theme_bw()
  }
  
  employment_plotter <- function(...){
    if (!is.numeric(employment_var_x()) & !is.numeric(employment_var_y()))
    {employment_aes() + geom_jitter()}
    else if (!is.numeric(employment_var_x()) & is.numeric(employment_var_y()))
    {employment_aes() + geom_boxplot()}
    else if (is.numeric(employment_var_x()) & is.numeric(employment_var_y()))
    {employment_aes() + geom_point(alpha = 0.5)}
  }
  
  output$employmentbivariate <- renderPlot({
    employment_plotter()
  })
  
  # Employment Time Series
  
  output$employmenttimeseries <- renderPlot({
    
    avg_employment_covid %>%
      select(Date, !!input$employmenttimeseriesy, covid_percent_change) %>%
      gather(key="var", value = "value", -Date) -> avg_employment_covid_new
    
    avg_employment_covid_new %>%
      filter(Date > "2020-02-29") %>%
      ggplot(aes(x=Date, y=value)) +
      geom_line(aes(color=var)) +
      # geom_vline(aes(xintercept = ymd("2020-03-13"),
      #                color = "March 10, National Emergency"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-04-16"),
      #                color = "April 16, Gating Criteria"),size = 1)+
      # geom_vline(aes(xintercept = ymd("2020-05-28"),
      #                color = "May 28, Deaths = 100k"), size = 1) +
      scale_color_manual(values = c("tomato2", "steelblue")) +
      xlab("Date") +
      ylab("Variable Change")+
      labs(color = "Variable") +
      theme_minimal()
    
  })
  
  # Employment stat End
  
  
  
  
  
  
  # Employment End
} # end of server

shinyApp(ui, server)
