# initialize packages
# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('shiny')
# install.packages('shinydashboard')
# install.packages('leaflet')
# install.packages('RColorBrewer')
# install.packages('plyr')
# install.packages('ggthemes')
# install.packages('treemap')
# install.packages('plotly')
# install.packages('rgeos')
# install.packages('rworldmap')
# install.packages('dashboardthemes')
# install.packages('dygraphs')
# install.packages('xts')

require(tidyverse)
require(plyr)
require(dplyr)
require(shiny)
require(shinydashboard)
require(dashboardthemes)
require(RColorBrewer)
require(treemap)
require(plotly)
require(rgeos)
require(rworldmap)
require(leaflet)
require(dygraphs)
require(xts)


options(warn=-1)

################################################################################
# read csv data
################################################################################
exped <- read_csv('./data/exped.csv')
peaks <- read_csv('./data/peaks.csv')
members <- read_csv('./data/members.csv')
coordinates <- read_csv('./data/co-ordinates.csv')

count(unique(exped$PEAKID))

################################################################################
# Popular peaks
################################################################################
popular.peaks <- members %>% 
  group_by(PEAKID) %>%
  tally(name='Frequency')

colnames(popular.peaks) <- c('PEAKID','Frequency')

success.table <- members %>%
  merge(peaks,by=c('PEAKID')) %>%
  select(PEAKID,MHIGHPT) %>%
  group_by(PEAKID,MHIGHPT) %>%
  filter(MHIGHPT==TRUE) %>%
  tally(name="SUCCESS") %>%
  select(PEAKID,SUCCESS)
  

popular.peaks <-popular.peaks %>% 
  merge(peaks,by = c('PEAKID')) %>%
  select('PEAKID','PKNAME','Frequency','HEIGHTM')

height_labels <- c('5250-5500','5500-5750','5750-6000','6000-6250','6250-6500',
                   '6500-6750','6750-7000','7000-7250','7250-7500','7500-7750',
                   '7750-8000','8000-8250','8250-8500','8500-8750','8750-9000')
popular.peaks <- popular.peaks %>% 
  mutate(HEIGHT.RANGE=cut(HEIGHTM, breaks = seq(5250,9000,250),dig.lab=10,labels=height_labels)) %>%
  select(PEAKID,PKNAME,HEIGHT.RANGE,HEIGHTM,Frequency) 
popular.peaks <- popular.peaks %>%
  merge(success.table, by=c('PEAKID')) %>%
  select("PEAKID","PKNAME","HEIGHT.RANGE","HEIGHTM","Frequency","SUCCESS")
colnames(popular.peaks) <- c("PEAKID","PEAK.NAME","HEIGHT.RANGE","HEIGHTM","VISIT.FREQUENCY","SUCCESS")

################################################################################
# Life Risk Analysis
################################################################################
exped_members <- merge(exped,members,by=c('EXPID','PEAKID'))
exped_members_peak <- merge(exped_members,peaks,by='PEAKID')
exped_members_peak <- exped_members_peak %>%
  select(c("EXPID","PEAKID","PKNAME","YEAR","SEASON","SEX","CALCAGE","HIGHPOINT","HEIGHTM","INJURYTYPE","DEATHTYPE"))

exped_members_peak$SEASON <- as.factor(exped_members_peak$SEASON)
exped_members_peak$SEASON <- revalue(exped_members_peak$SEASON, c('0' = "All", '1' = "Spring", '2' = 'Summer', '3' = 'Autumn',
       '4' = 'Winter') )

risk <- exped_members_peak
# members.risk <- filter(members.risk, MHIGHPT == FALSE)
risk$INJURYTYPE <- revalue(as.character(risk$INJURYTYPE), c('0' = "Unspecified" , '1' = "Acute Mountain Sickness",
                                                    '2' = "Exhaustion", '3' = "Exposure/Frostbite", '4' = "Fall",
                                                    '5' = "Crevasse", '6' = "Icefall collapse", '7' = "Avalanche",
                                                    '8' = "Falling Rock/Ice", '9' = "Disppearance", '10' = "Illness",
                                                    '11' = 'Other', '-1' = 'Died'))

risk$DEATHTYPE <- revalue(as.character(risk$DEATHTYPE), c('0' = "Unspecified" , '1' = "Acute Mountain Sickness",
                                                    '2' = "Exhaustion", '3' = "Exposure/Frostbite", '4' = "Fall",
                                                    '5' = "Crevasse", '6' = "Icefall collapse", '7' = "Avalanche",
                                                    '8' = "Falling Rock/Ice", '9' = "Disppearance", '10' = "Illness",
                                                    '11' = 'Other', '12'= 'Unknown'))

risk <- risk %>%
  filter(HIGHPOINT < HEIGHTM & INJURYTYPE != 0) %>%
  select(SEASON, INJURYTYPE, DEATHTYPE)

################################################################################
# Demographic Analysis
################################################################################
members.2 <-  filter(members, !is.na(PRIMARYCITIZEN))
# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
id <- rownames(df)

countries.details <- cbind(PRIMARYCITIZEN=id, df)


countries.details$PRIMARYCITIZEN[countries.details$PRIMARYCITIZEN=='United States of America'] <- 'USA' 
countries.details$PRIMARYCITIZEN[countries.details$PRIMARYCITIZEN=='United Kingdom'] <- 'UK'

sex.ratio <- members.2 %>%
  distinct(FNAME,LNAME,PRIMARYCITIZEN, SEX) %>%
  select(PRIMARYCITIZEN,SEX) %>%
  group_by(PRIMARYCITIZEN, SEX) %>%
  tally(name='Frequency') %>%
  spread(SEX,Frequency) %>%
  ungroup() %>%
  transmute(PRIMARYCITIZEN = PRIMARYCITIZEN, female = replace_na(F,0),
            male = replace_na(M,0), female_percent = female*100/(female+male),
            male_percent = male*100/(female+male), total = (female+male)) 

sex.ratio <- sex.ratio %>%
  merge(countries.details, on = PRIMARYCITIZEN)

age_labels <- c('0-10','10-20','20-30','30-40','40-50','50-60','60-70','70-80','80-90','90-100')
age.ratio <- members.2 %>% 
  distinct(FNAME,LNAME,PRIMARYCITIZEN, SEX, CALCAGE) %>%
  mutate(AGE.CATEGORY=cut(CALCAGE ,breaks = seq(0,100,10),dig.lab = 10,labels=age_labels)) %>%
  group_by(PRIMARYCITIZEN,AGE.CATEGORY) %>%
  filter(!is.na(AGE.CATEGORY)) %>%
  tally(name='count')

################################################################################
# Timeline
################################################################################ 
timeseries_df <- exped %>%
  group_by(BCDATE) %>%
  tally(name='Frequency') %>%
  na.omit()

countries_count <- members %>%
  distinct(FNAME,LNAME,PRIMARYCITIZEN) %>%
  group_by(PRIMARYCITIZEN) %>%
  tally(name='total')  %>%
  merge(countries.details, on = PRIMARYCITIZEN) 
  
countries_stats <- members %>%
  filter(PEAKID == 'EVER') %>%
  distinct(FNAME,LNAME,PRIMARYCITIZEN) %>%
  group_by(PRIMARYCITIZEN) %>%
  tally(name='ever')  %>%
  merge(countries_count, on = PRIMARYCITIZEN) 

################################################################################
# UI design
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Himalayan Expeditions", titleWidth = "275"),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Popular peaks", icon = icon("mountain"), tabName = "popularpeaks"),
      menuItem("Demographic Analysis", icon = icon("hiking"), tabName = "demographics"),
      menuItem("Summit trends", tabName = "timeseries", icon = icon("dashboard")),
      menuItem("Reasons for Abandoning Ascents", icon = icon("heart"), tabName = "treedisplay"),
      menuItem("Latest News & Data Sources", icon = icon("database"), tabName = "data")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tags$head(tags$style(HTML('.info-box {min-height: 45px;} 
                              .info-box-icon {height: 45px; line-height: 45px;} 
                              .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    
    tabItems(
      tabItem(tabName = "home",
        fluidPage(tags$head(
          # Note the wrapping of the string in HTML()
          tags$style(HTML("
          @import url('https://fonts.googleapis.com/css2?family=Rye&display=swap');
          @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
          @import url('https://fonts.googleapis.com/css2?family=Lato:ital,wght@1,100;1,300&display=swap');
          h1{
            font-family: 'Rye', sans-serif;
            font-size: 40px
          }
          h3 {
            font-family: 'Roboto', sans-serif;
            font-size: 18px
          }
          h4 {
            font-family: 'Lato', sans-serif;
          }
          h5 {
            font-family: 'Lato', sans-serif;
            font-size: 11px
          }
        "))
        ),
          h1("Mountaineering in the Nepal Himalayas",align = "center"),
          h5("Click on the markers to see the name of the peak", align='right'),
          leafletOutput("main", height = 325),
          h4("Geolocation of Himalayan peaks.", align = "center"),
          column(6,
                 fluidPage(
          p(style="text-align: justify;","The Himalayan Ranges is the world's youngest and highest mountain range in the South Eastern subcontinent 
          covering over 0.4% of the earth's surface. The Himalayas are one of the loftiest ranges in the
            world with jagged summits, huge glaciers, stunning vistas and abode 
            to the world's highest point, Mount Everest that reaches an elevation
            of 8850 m lying on the border between Tibet and Nepal.This natural grandeur is shared between Nepal, China, Tibet and India. 
            The Himalayas is an abode to 9 out of the 10 world's highest peaks including Mount Everest and K2. "),
          p(style="text-align: justify;","Mountaineering is one of the most 
          enthralling and adventurous sport of conquering great heights with different gradients and terrains. 
             It is also a dangerous sport which requires great amount of mental 
             and physical strength to ascent any peak. This sport has allured 
             people from across the globe to challenge themselves and experience 
             the thrill of climbing the various Himalayan peaks. The journey to the Himalayan peaks 
            is considered to be both inwards as well as outwards for an individual! "))),
          column(6, 
          p(style="text-align: justify;", "A non-profit organisation in the USA 
            - The Himalayan Database has maintained an archive of all the 
            expeditions covered over the Himalayas from 1905 to 2020. Author 
            Elizabeth Hawley has documented all the details regarding the expedition 
            groups, the members involved, summit information, the journey details, 
            peaks information, season etc. The main motivation 
            behind documenting all these is to learn from the previous expeditions
            and become more vigilant about the mortalities and morbidities during 
            those summits. Choosing the right peak from a plethora of options which
            are suitable for climbers belonging to different age groups, training 
            health conditions and which reference documents can guide you in preparing
            for a particular summit can be answered using all the data that has 
            been collected over the years to prepare yourself for a successful ascent." )),
          br(),
          br(),
        
        h4(style="text-align: center;","Narrative illustration of Himalayan Expeditions",br(), "by Roma Hambar"),
          
      )),
      
      tabItem(tabName = "timeseries",
              fluidPage(
                br(),
                fluidRow(
                  column(8,br(), br(),dygraphOutput("timechart", height = 500),
                         h5("Hover over any point on the graph to check number of summits started on that day. 
                 To change time scale, drag the bar above.", align="center")),
                  column(4, h1("How trending are the Himalayan Summits?",align = "center"),
                  p(style="text-align: justify;", "Himalayan Summits have become popular after Edmund Hillary of New Zealand and Sherpa Tenzing Norgay, 
                              of Nepal, became the first explorers to reach the pinnacle of Earth - Mount Everest. 
                              Very few had explored the Himalayas before this by climbing lower altitude peaks. We can see that a lot of months in the timeline
                              do not show any records for expeditions as it is important that the season and the climatic conditions in the Himalayas is 
                              taken into consideration while planning any expedition. Most of the expeditions are planned during the summit window months - May and September to avoid wild jet streams and 
                              harsh weather conditions leading to natural disasters and deteriorating health conditions. As you scroll down the time range, you will observe that very few expeditions 
                              have been recorded in those era suggesting the that such expeditions took time to pick up their popularity and people were hesistant to conquer such greater heights due to 
                              extremely unhealthy habitable conditions.
                              "),
                        h5(style="text-align: justify;", "Note: Some of the significant events that have happened in all the expeditions have been marked in the time series graph."))
                ),
                br(),
                br(),
                fluidRow(
                  column(5,h1("Himalayan Expeditions trending across the globe"),
                         sliderInput("no", "Number of observations:",min = 1, max = 20, value = 10, step = 1),
                         h5("Select number of top most countries participating in ascending the summits.",br(), 
                            "Hover over the bars to check number of climbers belonging to the particular country.",align='left'),
                         p(style="text-align: justify;", "Scaling the Nepal Himalayan Ranges is seen passionately in the Japenese people who are leading the race of sending the highest number of mountaineers - 3771 until the year 2020. Next in line is the USA with 3732 members. 
                           European nations such as France, Spain, Germany, Italy, Austria are also seen to be passionate about exploring the Himalayas. Most of the Sherpas in these expeditions come from Nepal which is also in the top 5 countries participating in these expeditions. Mount Everest, 
                           being the jewel in these ranges for being the highest point of the land mass, more than 50% of the total participating members from Nepal, China and India have attempted to climb this gigantic peak followed by other leading countries by number of members 
                           going on Himalayan expeditions till date. Most of the citizens from Nepal and India have been leading such expeditions."),
                         h5(style="text-align: justify;", "Note: Head over the Demographics Analysis page to study about the members belonging to the top countries listed.")
                         ),
                  column(7,plotlyOutput("countryBar", height = 600)))
              )
      ),
      
      tabItem(tabName = "popularpeaks",
              fluidPage(
              h1("Where can you go?",align = "center"),
              fluidRow(
                uiOutput("box1",width=2)
              ),
              column(7,fluidRow(
                sliderInput("n", "Number of observations:",min = 1, max = 30, value = 10, step = 1),
                h5("Use the slider to change observe the most explored peaks in the Himalayas","left"),
                plotlyOutput("brandBar", height = 450))),
              column(5,fluidRow(
                p(style="text-align: justify;", "A lot of mountaineers visit the Himalayas again to experience its 
                  loftiness or explore other mountain peaks. The most famous among all the mountain peaks is the Mount Everest - called as the tallest roof of the Earth with the probability of 55% of reaching the summit.
                  For most the first timers, it is recommended that they start with a low altitude mountain preferrably Ama Dablam which is at the height of 6814m and Tukuche at 6920m.
                  Ama Dablan has a higher success rate compared to Tukuche with 65%. Annapurna peak is considered to the world's deadliest summit due to its steep face and thus, the rate of survival there is extremely low - 28%.
                  Next popular peaks close to this segment are Manaslu and Cho Oyu with a higher success rate of 63% and 56% at an altitude of 8163 and 8188m respectively.
                  "),
                dataTableOutput("table"))
                )
      )),

      tabItem(tabName = "demographics",
              fluidPage(
              h1("Whom would you meet?",align = "center"),
              fluidRow(
              column(7,h5("Click on the markers to check the male female ratio and frequency of climbers from this country", align="right"),
                     leafletOutput("chart"),
                     h4("Proportion of expedition members belonging to various countries.", align="center")),
              column(5,br(), br(),p(style="text-align: justify;", "Mountaineering in the Himalayas has not just been restricted to countries surrounding it 
                but popular even in Japan, USA, UK and many European countries. It is one of the most common occupations to become a Sherpa in Nepal. 
                We can see that people from Japan have been most passionate about Himalayan mountaineering.
                More than 100 climbers from India(112) and Nepal(405) belonged to the age group of 10-20. Amongst all the nations, Botswana, Kosovo, Morrocco 
                and Guatemala have had the more females joing the expeditions compared to males. Most of the members belonged the age group between 
                20-50 after training themselves better. Himalayan Expeditions can be seen to be quite popular among the South East Asian counties and few
                countries in South America and seen as an emerging trend in the African countries.
                                    "), 
              h5(style="text-align: justify;", "Note: The number of males and females has been counted based on first exploration trip only. Here multiple attempts by single person have been ignored. 
                While finding number of people belonging to different age groups for a particular country, we have considered the fact that the age of person going on numerous expeditions will 
                varying depending in which age the person goes on a new exploration to the himalayas again.
                "))),
              fluidRow(uiOutput("box3", width = 2)),
              fluidRow(
            column(4,plotlyOutput('pie')),
            column(8,plotlyOutput('funnel')))
      )),
      
      tabItem(tabName = "treedisplay",
              fluidPage(
                h1("How risky can the quest be?",align = "center"),
                
                p(style="text-align: justify;", "Historical data helps in proactive planning of future ascents 
                in case of extreme weather conditions posing as the main reason behind most of the casualties.
                Different peak ascents take varying time durations to cover uphill and downhill ascents.
                Reaching the Mount Everest base takes about 20 days but reaching the Everest peak takes about 40 days from the base. 
                The main challenge posed at such high altitude is maintaining the oxygen and therefore climbers first get acclimatized 
                to the weather conditions before any ascent. It is very important that such expedditions are planned during the summit window period
                  between March and November i.e. in Summer, Spring and Autumn seasons. Summer season is found to be most favourable amongst the three.
                  Winters in these terrains are quite harsh with continuously strong jet streams leading to avalances and crevasses (long and huge cracks on the 
                  ice surface coated over the peaks)."),
                p(style="text-align: justify;","Recorded case show that the main reason for mountaineers dying during these summits is due to avalanches and falling from steep slopes.
                A lot of people suffer from Acute Mountain Sickness and hypothermia causing odema and and frostbite. The number of cases recrded during winters can be misleading 
                it should be noted that it is considered to be highly unsafe to travel during winter and so no expeditions are planned during this period. It is quite difficult to sustain in such harsh conditions and so it
                  is highly recommended that the mountaineer is both physically and mentally fit to overcome these fatal challenges. Below are the time periods for different seasons:"),
                tags$ol(
                  tags$li("Spring: From March to May"),
                  tags$li("Summer: From June to August"),
                  tags$li("Autumn: From September to November"),
                  tags$li("Winter: From December to February")
                ),
                div(style = "font-size: 18px;", radioButtons('season',label="Select Season",
                                                             choices=c('All','Spring','Summer','Autumn','Winter'),
                                                             inline = TRUE,selected = 'All' ), align = 'center'),
                column(6,plotlyOutput("treeinjury", height = 500), offset = 0, style='padding:0px;'),
                column(6,plotlyOutput("treedeath", height = 500), offset = 0, style='padding:0px;')
                # plotlyOutput("gauge")
      )),
      
      tabItem(tabName = "data",
              fluidPage(
                
                
                fluidRow(
                column(6,h1("Latest News:"),
  
                       img(src='mountain.jpg', align = "center",height="90%", width="90%"),
                br(),
                tags$head(tags$style(HTML("a {color: blue}"))),
                br(),
                p(tags$a(href="https://www.ndtv.com/india-news/iss-astronauts-share-spectacular-pictures-of-himalayas-turin-captured-from-space-2455339",
                         "NASA Astronauts Share Breathtaking Pictures of Himalayas, Italy's Turin (www.ndtv.com)")),
                p(tags$a(href="https://www.hindustantimes.com/india-news/how-covid-19-scaled-mt-everest-101621799223405.html",
                         "How Covid-19 scaled Mt Everest (www.hindustantimes.com)")),
                p(tags$a(href="https://www.theguardian.com/world/2021/feb/08/rescuers-search-for-missing-dead-glacier-dam-india-north",
                         "Rescuers search for 171 missing people after Indian glacier causes devastating flood (www.guardian.com)"))),
                column(6,
                h1("Data Sources"),
                br(),
                h3("Data for this study has been retrieved from:"),
                h4("1. Himalayan Expeditions Dataset"),
                tags$head(tags$style(HTML("a {color: blue}"))),
                p(tags$a(href="https://www.himalayandatabase.com/downloads.htm",
                         "Expeditions, Members and Peaks Dataset (www.himalayandatabase.com)")),
                p(tags$a(href="https://www.kaggle.com/tahminashoaib86/himalayan-database",
                         "CSV format of data (kaggle.com)")),
                br(),
                h4("2. Geo-coordinates of Few Himalayan Peaks in Nepal"),
                p(tags$a(href="https://www.latlong.net/category/mountains-154-1.html",
                         "Latitude Longitude Data of Himalayan Peaks in Nepal (www.latlong.net)")),
                p(tags$a(href="https://en.wikipedia.org/wiki/List_of_Himalayan_peaks_and_passes",
                         "Latitude Longitude Data of Himalayan Peaks in Nepal (wikipedia.org)")))
              ))
      )
    )
  )
)

################################################################################
# Server Logic
################################################################################
server <- function(input, output) {
  
  output$main <- renderLeaflet({
    leaflet(data = coordinates) %>% 
      addProviderTiles('Esri.NatGeoWorldMap') %>%
      addMarkers(~Longitude, ~Latitude, popup = ~Place_Name) 
  })

  
  ################################################################################
  # Popular peaks
  ################################################################################
  output$brandBar <- renderPlotly({
    if (req(input$n) < 1) {
      input$n == 1
    } else if (req(input$n) > 31) {
      input$n == 30
    }
   
   popular.peaks %>% 
     arrange(desc(VISIT.FREQUENCY)) %>%
     slice(1:input$n) %>%
     plot_ly(.) %>%
     add_trace(x = ~reorder(PEAKID,-VISIT.FREQUENCY), y = ~VISIT.FREQUENCY, color = ~HEIGHT.RANGE, type = 'bar', 
               hovertemplate = paste("Expeditions to %{x}:",
                                    "%{y}",
                                    "<extra></extra>")) %>%
     add_trace(x = ~reorder(PEAKID,-VISIT.FREQUENCY), y = ~SUCCESS, type = 'scatter', mode = 'dash',showlegend =FALSE , 
               hovertemplate = paste("Number of successful summits on %{x} is",
                                     "%{y} ",
                                     "<extra></extra>")) %>%
     config(displayModeBar = FALSE) %>%
     layout(xaxis=list(title="Peak IDs in the Himalayas"), 
            yaxis=list(title="Frequency of Summits"),
            plot_bgcolor='transparent', paper_bgcolor='transparent') %>% 
     layout(legend=list(title=list(text='<b> Height Range (m) </b>'),orientation = "h",   
                          xanchor = "center", 
                          x = 0, y = -0.2))
      
  })
  
  output$box1 <- renderUI({
    d <- event_data("plotly_click")
    if(!is.null(d)){
      data <- popular.peaks %>% 
        arrange(desc(VISIT.FREQUENCY)) %>%
        slice(1:input$n) %>%
        filter(PEAKID == d$x) 
    }
    else{
      data <- popular.peaks %>% 
        arrange(desc(VISIT.FREQUENCY)) %>%
        slice(1:input$n) %>%
        filter(PEAKID == 'EVER') 
    }
    tagList(
    infoBox(paste("Name"),paste(data$PEAK.NAME),color="purple",width = 3, icon=icon("mountain")),
    infoBox(paste("Altitude (m)"), paste(data$HEIGHTM),color="purple", width=3, icon=icon("mountain")),
    infoBox(paste("Frequency"), paste(data$VISIT.FREQUENCY),color="purple",width=3, icon=icon("users")),
    infoBox(paste("Success Rate"), paste(round(data$SUCCESS*100/data$VISIT.FREQUENCY,2),"%"),color = "purple",width=3))
    
  })
  
  
  output$table <- renderDataTable({
    validate(
      need(input$n > 1 , 'Choose between 2 - 50')
    )
    
    popular.peaks %>% 
      arrange(desc(VISIT.FREQUENCY)) %>%
      slice(1:input$n) %>%
      mutate(SUCCESSRATE = round(SUCCESS*100/VISIT.FREQUENCY,2)) %>%
      select(PEAKID,PEAK.NAME,HEIGHTM,SUCCESSRATE) %>%
      `colnames<-`(c("Peak ID", "Peak Name", "Altitude (m)", "Success rate"))
  }, 
  options = list(
    pageLength = 5
  ))
  
  ################################################################################
  # Life Risk Analysis
  ################################################################################
  # filter data based on user input
  df_injuries <- reactive({
    if (input$season == 'All'){
    d <- risk %>%
      select(INJURYTYPE) %>%
      group_by(INJURYTYPE) %>%
      tally(name='Frequency')  %>%
      filter(INJURYTYPE != 'Unspecified')
    }
    else{
      d <- risk %>%
        filter(SEASON == input$season) %>%
        select(INJURYTYPE) %>%
        group_by(INJURYTYPE) %>%
        tally(name='Frequency') %>%
        filter(INJURYTYPE != 'Unspecified')
    }
    d
  })

  df_deaths <- reactive({
    if (input$season == 'All'){
      d <- risk %>%
        select(DEATHTYPE) %>%
        group_by(DEATHTYPE) %>%
        tally(name='Frequency')  %>%
        filter(DEATHTYPE != 'Unspecified')
    }
    else{
      d <- risk %>%
        filter(SEASON == input$season) %>%
        select(DEATHTYPE) %>%
        group_by(DEATHTYPE) %>%
        tally(name='Frequency') %>%
        filter(DEATHTYPE != 'Unspecified') 
    }
    d
  })
  
  
    
  output$treeinjury <- renderPlotly({
      plot_ly(
        type='treemap',
        labels=df_injuries()$INJURYTYPE,
        parents=NA,
        values= df_injuries()$Frequency,
        textinfo="label+value",
        domain=list(column=0)
        ) %>%
      config(displayModeBar = FALSE) %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent',
             title = "Types of Injuries in season")
  })
    
  output$treedeath <- renderPlotly({
    plot_ly(
      type='treemap',
      labels=df_deaths()$DEATHTYPE,
      parents=NA,
      values= df_deaths()$Frequency,
      textinfo="label+value",
      domain=list(column=0)) %>%
      config(displayModeBar = FALSE) %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent', 
             title = "Reasons for Death in season")
    
  })
   # output$gauge <- renderPlotly({
   #   plot_ly(x =c('Injuries','Deaths') , y = ~c(sum(df_injuries()$Frequency,sum(df_deaths()$Frequency))), type = 'bar', 
   #               hovertemplate = paste("Expeditions to %{x}:",
   #                                     "%{y}",
   #                                     "<extra></extra>"))
   # })
  
  ################################################################################
  # Time Series Analysis
  ################################################################################
  output$timechart <- renderDygraph({
    
    timeseries_data <- xts(x = timeseries_df$Frequency, 
                           order.by = timeseries_df$BCDATE)["1950/2020"] 
    dygraph(timeseries_data, main = "Time Series Analysis of Himalayan Expeditions") %>%
      dyOptions(fillGraph=TRUE,axisTickSize = 10,drawGrid = FALSE, colors='#800080', 
                axisLineColor = "brown") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2020-04-01"))%>% 
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyLegend(show = "follow", width = 100) %>%
      dyAxis("y", label = "Summits organised") %>%
      dyAxis("x", label = "Year of Summit",drawGrid = FALSE )%>% 
      dySeries("V1", label = "Frequency")  %>%
      dyEvent("2019-11-02", "394/468 peaks have been ascended", labelLoc="top") %>%
      dyEvent("1953-05-29",
              HTML("First successful Everest Expedition by E. Hillary & Tenzing Norgay"),
              labelLoc='top') %>%
      dyEvent("1975-03-18", "First all womens summit to Everest led by Tabei Junko", labelLoc='top') %>%
      dyEvent("1980-08-22","First Solo Ascent to Monut Everest by Reinhold Messner", labelLoc='top') %>%
      dyEvent("2013-05-23","Nepalese Min Bahadur Sherchan completed summit at age of 80", labelLoc='top') %>%
      dyEvent("2015-01-01","No expeditions carried out for entire year due to avalanches", labelLoc='top') %>%
      dyEvent("2003-05-24","15 year old Nepalese girl Ming Kipa Sherpa climbed Everest", labelLoc='top') %>%
      dyEvent("2010-05-24","13 year old American boy Jordan Romero climbed Everest", labelLoc='top') %>%
      dyEvent("2015-04-25","Major Earthquake", labelLoc='top') %>%
      dyEvent("2019-05-25","Maximum number of members in one summit: 427", labelLoc='top') %>%
      dyEvent("2011-05-13","Apa Sherpa climbs the Everest for the 21st time at age 51") %>%
      dyEvent("2020-01-01", "Limited Expeditions due to COVID-19")
      
  })
  
  output$countryBar <- renderPlotly({
    if (req(input$no) < 1) {
      input$no == 1
    } else if (req(input$no) > 31) {
      input$no == 30
    }
    
    countries_stats %>% 
      arrange(desc(total)) %>%
      slice(1:input$no) %>%
      plot_ly(.) %>%
      add_trace(
        type='bar',
        y = ~reorder(PRIMARYCITIZEN,total),
        x = ~ever,
        textposition = "outside",
        hovertemplate = paste("Number of Everest Attempts from %{y}",
                              "%{x}",
                              "<extra></extra>"))  %>%
      add_trace(
        type='bar',
        y = ~reorder(PRIMARYCITIZEN,total),
        x = ~(total),
        # color = ~PRIMARYCITIZEN,
        textposition = "outside",
        hovertemplate = paste("Number of members from %{y}",
                              "%{x}",
                              "<extra></extra>"))  %>%
      
      config(displayModeBar = FALSE) %>%
      layout(yaxis=list(title="Countries"),
             xaxis=list(title="Frequency"),
             plot_bgcolor='transparent', paper_bgcolor='transparent',
             title="Top Countries by number of participants",
             showlegend=FALSE) 
  })
  
  
  ##############################################################################
  # Demographic Analysis
  ##############################################################################
  
  output$chart <- renderLeaflet({
    
    maxLong = max(countries_count$x)
    maxLat = max(countries_count$y)
    minLong = min(countries_count$x)
    minLat = min(countries_count$y)
    
    leaflet(data = countries_count,options=list(
      center = c(0, 0),
      zoom = 1,
      worldCopyJump = FALSE
    )) %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      # addTiles() %>%
      addCircleMarkers(~x, ~y,
                       layerId= ~PRIMARYCITIZEN,
                       radius= ~total*0.001,
                       opacity = 1, popup = ~PRIMARYCITIZEN) 
    
  })
  output$box3 <- renderUI({
    click<-input$chart_marker_click
    if(is.null(click)){
      name <- 'Japan'
      d <- sex.ratio %>%
        filter(PRIMARYCITIZEN == 'Japan') %>%
        select(male,female)
    }
    else{
      name <- click$id
      d <- sex.ratio %>%
        filter(PRIMARYCITIZEN == click$id) %>%
        select(male,female)
    }
    tagList(
      infoBox(paste("Country"),paste(name),color="purple",width = 4,icon= icon("map-pin")),
      infoBox(paste("Number of Mountaineers"), paste(d$male+d$female),color="purple",width=4, icon=icon("users")))
    
  })
  
  output$pie <- renderPlotly({
    click<-input$chart_marker_click
    if(!is.null(click))
    {
      # print(c(click$id,click$lat, click$lng))
      d <- sex.ratio %>%
        filter(PRIMARYCITIZEN == click$id) %>%
        select(male,female)
    }
    else{
      d <- sex.ratio %>%
        filter(PRIMARYCITIZEN == 'Japan') %>%
        select(male,female) 
    }
    
    plot_ly(labels =c("Male","Female"), values = c(d$male,d$female), 
            textinfo="label+value",
    marker = list(colors = c("deepskyblue", "lightsalmon"))) %>%
      add_pie(hole = 0.6) %>%
      config(displayModeBar = FALSE) %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent',
             showlegend=FALSE)
      
  })
  
  output$funnel <- renderPlotly({
    click<-input$chart_marker_click
    if(!is.null(click))
    {
      # print(c(click$id,click$lat, click$lng))
      d <- age.ratio %>%
        filter(PRIMARYCITIZEN == click$id) 
    }
    else{
      d <- age.ratio %>%
        filter(PRIMARYCITIZEN == 'Japan') 
    }
    
    plot_ly(d, y=~AGE.CATEGORY, x=~count, type='funnel',
            textinfo =  "y+x",
            hovertemplate = paste("Group %{y}: ",
                                  "%{x}",
                                  "<extra></extra>")) %>%
      config(displayModeBar = FALSE) %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent',
             yaxis=list(title="Age Categories", title = "Age groups"))

  
    })
  
}
################################################################################
# Run the application 
################################################################################
shinyApp(ui = ui, server = server)
