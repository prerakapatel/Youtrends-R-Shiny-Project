# Shiny
library(shiny)
library(shinydashboard)



################################################################
##################### R-Shiny App UI ###########################
################################################################

#Dashboard header carrying the title of the dashboard
#header <- dashboardHeader(
#  titleWidth='100%',
#  title = span(tags$img(src="Youtrends_icon_large.jpg", width = '100%')) #if % don't work - pass pixels like 350
#)
header <- dashboardHeader(title = "YouTrends")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(sidebarMenu(id = "tabs", width = '20%',
                                        menuItem("Home", tabName = "home",  icon = icon("home")),
                                        menuItem("Global Dashboard", tabName = "cc_dashboard", icon = icon("globe")),
                                        menuItem("Country Dashboard", tabName = "cn_dashboard", icon = icon("flag"),
                                                 menuSubItem("United States", tabName = "us_dashboard"),
                                                 menuSubItem("Great Britain", tabName = "gb_dashboard"),
                                                 menuSubItem("Canada", tabName = "ca_dashboard")),
                                        menuItem("Predict Popularity", tabName = "prediction", icon = icon("book"))
))

# Tab contents
tbs <- tabItems(
  # First tab content - home page
  tabItem(
    tabName = "home",
    #h2("Welcome to YOUTRENDS - your guide to popularity!"),
    fluidRow(column(width = 12,  imageOutput("home_yt_logo"))),
    fluidRow(column(width = 12,  imageOutput("home_yt_dash"))),
    fluidRow(column(width = 12,  imageOutput("home_text")))
  ), # end of first tab
  
  ######################################################
  # Second tab content - cross country
  tabItem(
    tabName = "cc_dashboard",
    h2("Welcome to Global Trending Videos Dashboard"),
    fluidRow(
      box(
        title = "Video Clip Appearances",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("cc1", height = "300px")
      )
      
      ,
      box(
        title = "Likes(%) as per Part of the day",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("cc2", height = "300px")
      )
    ) #End of Fluid Row 1
    ,
    fluidRow(
      box(
        width = 12,
        title = "Video Clips Appeared During the Different Time Intervals",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("cc3", height = "300px")
      )
    ) #End of fluid row 2
  ),  # end of second tab
  
  #######################################################
  # third tab - US
  tabItem(tabName = "us_dashboard",
          h2("Welcome to United States - Trending Videos Dashboard"),
          fluidRow(
            box(
              title = "Views",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us1", height = "300px")
            ),
            box(
              title = "Likes & Dislikes",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us2", height = "300px")
            )
          ) #End of Fluid Row 1
          ,
          fluidRow(
            box(
              title = "Comments",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us3", height = "300px")
            ),
            box(
              title = "Days to Trend",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us4", height = "300px")
            )
          ) #End of Fluid Row 2
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Category",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us5", height = "300px")
            )
          ) #End of fluid row 3
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Weekday",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us6", height = "300px")
            )
          ) #End of fluid row 4
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Month",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us7", height = "300px")
            )
          ) #End of fluid row 5
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Hour",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us8", height = "300px")
            )
          ) #End of fluid row 6
          ,
          fluidRow(
            box(
              width = 12,
              title = "L / D Ratio",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("us9", height = "300px")
            )
          ) #End of fluid row 7
  ),
  # third tab - GB
  tabItem(tabName = "gb_dashboard",
          h2("Welcome to Great Britain - Trending Videos Dashboard"),
          fluidRow(
            box(
              title = "Views",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb1", height = "300px")
            ),
            box(
              title = "Likes & Dislikes",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb2", height = "300px")
            )
          ) #End of Fluid Row 1
          ,
          fluidRow(
            box(
              title = "Comments",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb3", height = "300px")
            ),
            box(
              title = "Days to Trend",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb4", height = "300px")
            )
          ) #End of Fluid Row 2
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Category",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb5", height = "300px")
            )
          ) #End of fluid row 3
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Weekday",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb6", height = "300px")
            )
          ) #End of fluid row 4
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Month",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb7", height = "300px")
            )
          ) #End of fluid row 5
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Hour",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb8", height = "300px")
            )
          ) #End of fluid row 6
          ,
          fluidRow(
            box(
              width = 12,
              title = "L / D Ratio",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("gb9", height = "300px")
            )
          ) #End of fluid row 7
  ),
  # third tab - CA
  tabItem(tabName = "ca_dashboard",
          h2("Welcome to Canada - Trending Videos Dashboard"),
          fluidRow(
            box(
              title = "Views",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca1", height = "300px")
            ),
            box(
              title = "Likes & Dislikes",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca2", height = "300px")
            )
          ) #End of Fluid Row 1
          ,
          fluidRow(
            box(
              title = "Comments",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca3", height = "300px")
            ),
            box(
              title = "Days to Trend",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca4", height = "300px")
            )
          ) #End of Fluid Row 2
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Category",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca5", height = "300px")
            )
          ) #End of fluid row 3
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Weekday",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca6", height = "300px")
            )
          ) #End of fluid row 4
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Month",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca7", height = "300px")
            )
          ) #End of fluid row 5
          ,
          fluidRow(
            box(
              width = 12,
              title = "Videos and Views as per Hour",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca8", height = "300px")
            )
          ) #End of fluid row 6
          ,
          fluidRow(
            box(
              width = 12,
              title = "L / D Ratio",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("ca9", height = "300px")
            )
          ) #End of fluid row 7
  ), 
  
  ################################################################
  # Fourth tab content
  tabItem(tabName = "prediction",
          h2("Let's see how your video will perform!"),
          fluidRow(
            box(
              selectInput("cn_country", "Pick a Country",
                          c("United States" = "us",
                            "Great Britain" = "gb",
                            "Canada" = "ca" )),
              radioButtons(
                "category_id",
                "What category does your video fall under?",
                choices = c(
                  "Film & Animation" = "1",
                  "Music" = "10",
                  "Pets & Animals" = "15",
                  "Sports" = "17",
                  "Travel & Events" = "19",
                  "Autos & Vehicles" = "2",
                  "Gaming" = "20",
                  "People & Blogs" = "22",
                  "Comedy" = "34",
                  "Entertainment" = "24",
                  "News & Politics" = "25",
                  "Howto & Style" = "26",
                  "Education" = "27",
                  "Science & Technology" = "28",
                  "Shows" = "43")),
              radioButtons(
                "month",
                "In which month are you uploading your video?",
                choices = c(
                  "January" = "January",
                  "February" = "February",
                  "March" = "March",
                  "April" = "April",
                  "May" = "May",
                  "June" = "June",
                  "July" = "July",
                  "August" = "August",
                  "September" = "September",
                  "October" = "October",
                  "November" = "November",
                  "December" = "December")),
              radioButtons(
                "weekday",
                "On what day are you uploading your video?",
                choices = c(
                  "Monday" = "Monday",
                  "Tuesday" = "Tuesday",
                  "Wednesday" = "Wednesday",
                  "Thursday" = "Thursday",
                  "Friday" = "Friday",
                  "Saturday" = "Saturday",
                  "Sunday" = "Sunday")),
              radioButtons(
                "dayparting_name",
                "When are you uploading your video?",
                choices = c(
                  "12am - 6am" = "12am - 6am",
                  "6am - 12pm" = "6am - 12pm",
                  "12pm - 6pm" = "12pm - 6pm",
                  "6pm - 12am" = "6pm - 12am")),
              numericInput("desc_row", "How long is your description?", value=200, min=2, max=4998),
              numericInput("num_tags", "How many #tags are there?", value=15, min=1, max=74),
              numericInput("title_length", "How long is your title?", value=25, min=3, max=100),
              numericInput("title_upper", "How many letters in title are uppercase?", value=5, min=0, max=81)
            ),
            imageOutput("popularityimage")
          ))
)

# combine the two tabitems to make the body
body <- dashboardBody(tbs)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Youtrends', header = header, sidebar, body, skin = 'red')



################################################################