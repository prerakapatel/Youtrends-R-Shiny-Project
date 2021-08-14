##########################################################################
# load the required packages
##########################################################################
setwd("~/R material/Project/data")

# Data manipulation
library(data.table)
library(dplyr)
library(DT)
library(readr)
library(rjson)
library(janitor)
library(assertr)
library(tidyverse)
library(lubridate)
library(glue)
library(scales)
library(caret)
library(magrittr)

# Time manipulation
library(lubridate)

# Visualization
library(ggplot2)
library(ggcorrplot)
library(plotrix)
library(corrplot)
library(ggdendro)
library(ggrepel)
library(RColorBrewer)
library(plotly)
library(htmlwidgets)

# Extra
library(tidyr)
library(scales)
library(R.utils)
library(repr)
library(gbm)
library(pROC)

# load H2O package
library(h2o)
h2o.init(nthreads=3, max_mem_size="4g")

# Shiny
library(shiny)
library("shinydashboard")
library("dashboard")
require(shinydashboard)
library(shinydashboardPlus)

################################################################################
# Loading Datasets 
################################################################################
#load visual data
USvideos_vis <- read_csv("USvideos_vis.csv")
CAvideos_vis <- read_csv("CAvideos_vis.csv")
GBvideos_vis <- read_csv("GBvideos_vis.csv")

# add additional column "Country" to each data set
CAvideos_vis$country <- c("Canada")
GBvideos_vis$country <- c("UK")
USvideos_vis$country <- c("USA")

# data manuplation
USvideos_vis$category_id <- factor(USvideos_vis$category_id, order=FALSE)
USvideos_vis$likes <- as.numeric(USvideos_vis$likes)
USvideos_vis$dislikes <- as.numeric(USvideos_vis$dislikes)
USvideos_vis$comment_count <- as.numeric(USvideos_vis$comment_count)

CAvideos_vis$category_id <- factor(CAvideos_vis$category_id, order=FALSE)
CAvideos_vis$likes <- as.numeric(CAvideos_vis$likes)
CAvideos_vis$dislikes <- as.numeric(CAvideos_vis$dislikes)
CAvideos_vis$comment_count <- as.numeric(CAvideos_vis$comment_count)

GBvideos_vis$category_id <- factor(GBvideos_vis$category_id, order=FALSE)
GBvideos_vis$likes <- as.numeric(GBvideos_vis$likes)
GBvideos_vis$dislikes <- as.numeric(GBvideos_vis$dislikes)
GBvideos_vis$comment_count <- as.numeric(GBvideos_vis$comment_count)

# cross-country dataset ########################################################
ALLvideos_vis <- as.data.table(rbind(USvideos_vis,CAvideos_vis,GBvideos_vis))
df_all <- ALLvideos_vis

matdf1 = matrix(ncol = 9, nrow = 1)
df1 <- data.frame(matdf1)
colnames(df1) <- c("category_id", "month", "dayparting_name", "weekday",  "desc_row", "num_tags", "title_upper",
                   "title_length", "popularity")

################################################################################
# Visuals 
################################################################################
# Compare the countries based on the video clip appearances. These are not "views" counts.
compare_countries <- df_all %>%
  select("country") %>%
  group_by(country) %>%
  summarize(`TotalCount` = n())
compare_countries$country <- factor(compare_countries$country,
                                    levels = compare_countries$country[order(compare_countries$TotalCount)])

# Percentages of likes based on time of day 
df_all$dayparting_name <- factor(df_all$dayparting_name, levels=c("12am - 6am","6am - 12pm", "12pm - 6pm", "6pm - 12am"))
levels(df_all$dayparting_name)
df_timeOfDay <- df_all %>%
  select("dayparting_name", "likes")
df_timeOfDay <- df_timeOfDay %>%
  group_by(dayparting_name, likes) %>%
  summarize(`sum` = n())
result <- aggregate(df_timeOfDay$sum, by=list(time_slot=df_timeOfDay$dayparting_name), FUN=sum)
group <- result$time_slot
value <- result$x

# How Many Times Have Video Clips Appeared During the Different Time Intervals Across the Countries
df_compareCountriesWithTimeofDay <- df_all %>%
  select("dayparting_name", "country")
df_compareCountriesWithTimeofDay <- df_compareCountriesWithTimeofDay %>%
  group_by(dayparting_name, country) %>%
  summarize(`Number` = n())
# Define the number of colors you want
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)


################################################################################
# Loading trained Models 
################################################################################
uploaded_model_us <- h2o.upload_model('USA_Model')
uploaded_model_gb <- h2o.upload_model('GB_Model')
uploaded_model_ca <- h2o.upload_model('Canada_Model')


############################################################################
# CROSS COUNTRY PLOTS
############################################################################
# plot for No. of Video Clip Appearances
cc1 <- ggplot(compare_countries, aes(x=country, y=TotalCount, fill=country)) +
  ggtitle(label = "No. of Video Clip Appearances", subtitle = "By Country") +
  geom_bar(position = 'dodge', stat="identity")+theme_minimal() +
  geom_label(aes(label=TotalCount), size = 6) +
  ylab("Number of Appearances") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
  ) +
  scale_fill_manual(values = c( "Canada" = "#e07571",
                                "UK" = "#c4302b",
                                "USA" = "#d8504b"
  ))

# Pie plot for Likes as per time of day
#cols <- c("#eca9a7", "#e07571",  "#d8504b",  "#c4302b")
#percentlabels<- round(100*value/sum(value), 1)
#pielabels<- paste(percentlabels, "%", sep="")
#pie(value, main="Likes by Time of Day", col=cols, labels=pielabels, cex=0.75)
#legend("bottomleft", title = "Time of Day : Local Hour Intervals" ,legend = group, cex=0.7, fill=cols, bty='o')
# also saved as cc2.png

# Plot for How Many Times Have Video Clips Appeared During the Different Time Intervals
cc3_1 <- ggplot(df_compareCountriesWithTimeofDay, aes(x=country, y=Number, fill = dayparting_name)) +
  ggtitle(label = "No. of times video vlips appeared during the different time intervals", subtitle = "Showing Different Time Intervals") +
  geom_bar(aes(color = dayparting_name, fill = dayparting_name, y = Number, x = reorder(country, -Number)), position = position_dodge(0.8), stat="identity", width = 0.7)+theme_minimal() +
  geom_label(aes(country,Number,label=Number, group=dayparting_name), position=position_dodge(width=0.8), size=6) +
  ylab("Number of Appearances") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.8, 'cm')
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(values = c( 
    "12am - 6am" = "#eca9a7",
    "6am - 12pm" = "#e07571",
    "12pm - 6pm" = "#d8504b",
    "6pm - 12am" = "#c4302b"
  ))+
  scale_fill_manual(values = c( 
    "12am - 6am" = "#eca9a7",
    "6am - 12pm" = "#e07571",
    "12pm - 6pm" = "#d8504b",
    "6pm - 12am" = "#c4302b"
  ))
# override legend
cc3 <- cc3_1 + guides(
  fill = guide_legend(
    override.aes = aes(label = "")
  )
)


############################################################################
# COUNTRY-WISE PLOTS
############################################################################

# US Dashboard ##############################################################

#####VIEW COUNTS#####
us1 <- USvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of views", subtitle = "United States") +
  geom_density(aes(x=views, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma) +
  labs(x = "View Count")

######LIKE AND DISLIKE COUNTS#####
us2 <- USvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of likes and dislikes", subtitle = "United States") +
  #geom_histogram(aes(x=views, y=stat(density)),binwidth=0.1) +
  geom_density(aes(x=likes, y=stat(density)),fill="#F7000B", color="red",alpha=0.7) +
  geom_density(aes(x=dislikes, y=stat(density)),fill="black", color="black",alpha=0.7) +
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Like and Dislike Count")

#####COMMENT COUNTS#####
us3 <- USvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of comments", subtitle = "United States") +
  geom_density(aes(x=comment_count, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####DAYS TO TREND COUNTS#####
us4 <- USvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of days to trend", subtitle = "United States") +
  #geom_histogram(aes(x = days_to_trend), fill="#F7000B", alpha=0.7)+
  geom_density(aes(x=days_to_trend, y=stat(density)),fill="#F7000B",color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####CATEGORY COUNT#####
us5 <- USvideos_vis %>%
  mutate(category_id=as.character(category_id)) %>%
  group_by(category_id) %>%
  mutate(num=n()) %>%
  ungroup() %>% 
  ggplot() +
  ggtitle(label = "Videos and views as per category", subtitle = "United States") +
  geom_col(aes(x=category_id, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= category_id, y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_discrete(labels = c("Film & Animation", "Music", "Pets & Animals", "Sports", "Travel & Events", "Autos & Vehicles", "Gaming", "People & Blogs", "Comedy", "Entertainment", "News & Politics", "Howto & Style", "Education","Science & Technology", "Shows", "Nonprofit"))+
  scale_y_continuous(labels = comma)+
  coord_flip()

#####weekday#####
us6 <- USvideos_vis %>%
  group_by(weekday) %>%
  mutate(num=n()) %>%
  ggplot(aes(x=weekday)) +
  ggtitle(label = "Videos and views as per Weekday", subtitle = "United States") +
  geom_col(aes(y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  coord_flip()

#####MONTH#####
us7 <- USvideos_vis %>%
  #mutate(time_month=as.factor(time_month)) %>%
  group_by(month) %>%
  arrange(month) %>% 
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per month of publish", subtitle = "United States") +
  geom_col(aes(x=month, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= month, y= views),size=0.2,alpha=0.5, width=0.3) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  coord_flip()

##### HOUR - PUBLISH#####
us8 <- USvideos_vis %>%
  #mutate(time_hour=as.factor(time_hour)) %>%
  group_by(hour) %>%
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per hour of the day", subtitle = "United States") +
  geom_col(aes(x=hour, y=num),fill="#F7000B", alpha=0.7) + 
  geom_jitter(aes(x= hour, y= views),size=0.2,alpha=0.5, width=0.3) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  theme_minimal() +
  theme(panel.grid = element_blank())+
  coord_flip()

#####LIKES/DISLIKES RATE#####
us9 <- USvideos_vis %>%
  ggplot()+
  ggtitle(label = "Likes / Dislike Ratio", subtitle = "United States") +
  geom_jitter(aes(x=LD_ratio, y= views),size=0.2, alpha=0.5, width = 0.3, color ='red')+
  scale_y_continuous(labels = comma)+
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# GB Dashboard ################################################################

#####VIEW COUNTS#####
gb1 <- GBvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of views", subtitle = "Great Britain") +
  geom_density(aes(x=views, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma) +
  labs(x = "View Count")

######LIKE AND DISLIKE COUNTS#####
gb2 <- GBvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of likes and dislikes", subtitle = "Great Britain") +
  #geom_histogram(aes(x=views, y=stat(density)),binwidth=0.1) +
  geom_density(aes(x=likes, y=stat(density)),fill="#F7000B", color="red",alpha=0.7) +
  geom_density(aes(x=dislikes, y=stat(density)),fill="black", color="black",alpha=0.7) +
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Like and Dislike Count")

#####COMMENT COUNTS#####
gb3 <- GBvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of comments", subtitle = "Great Britain") +
  geom_density(aes(x=comment_count, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####DAYS TO TREND COUNTS#####
gb4 <- GBvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of days to trend", subtitle = "Great Britain") +
  #geom_histogram(aes(x = days_to_trend), fill="#F7000B", alpha=0.7)+
  geom_density(aes(x=days_to_trend, y=stat(density)),fill="#F7000B",color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####CATEGORY COUNT#####
gb5 <- GBvideos_vis %>%
  mutate(category_id=as.character(category_id)) %>%
  group_by(category_id) %>%
  mutate(num=n()) %>%
  ungroup() %>% 
  ggplot() +
  ggtitle(label = "Videos and views as per category", subtitle = "Great Britain") +
  geom_col(aes(x=category_id, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= category_id, y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_discrete(labels = c("Film & Animation", "Music", "Pets & Animals", "Sports", "Travel & Events", "Autos & Vehicles", "Gaming", "People & Blogs", "Comedy", "Entertainment", "News & Politics", "Howto & Style", "Education","Science & Technology", "Shows", "Nonprofit"))+
  scale_y_continuous(labels = comma)+
  coord_flip()

#####weekday#####
gb6 <- GBvideos_vis %>%
  group_by(weekday) %>%
  mutate(num=n()) %>%
  ggplot(aes(x=weekday)) +
  ggtitle(label = "Videos and views as per Weekday", subtitle = "Great Britain") +
  geom_col(aes(y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  coord_flip()

#####MONTH#####
gb7 <- GBvideos_vis %>%
  #mutate(time_month=as.factor(time_month)) %>%
  group_by(month) %>%
  arrange(month) %>% 
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per month of publish", subtitle = "Great Britain") +
  geom_col(aes(x=month, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= month, y= views),size=0.2,alpha=0.5, width=0.3) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  coord_flip()

##### HOUR - PUBLISH#####
gb8 <- GBvideos_vis %>%
  #mutate(time_hour=as.factor(time_hour)) %>%
  group_by(hour) %>%
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per hour of the day", subtitle = "Great Britain") +
  geom_col(aes(x=hour, y=num),fill="#F7000B", alpha=0.7) + 
  geom_jitter(aes(x= hour, y= views),size=0.2,alpha=0.5, width=0.3) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  theme_minimal() +
  theme(panel.grid = element_blank())+
  coord_flip()

#####LIKES/DISLIKES RATE#####
gb9 <- GBvideos_vis %>%
  ggplot()+
  ggtitle(label = "Likes / Dislike Ratio", subtitle = "Great Britain") +
  geom_jitter(aes(x=LD_ratio, y= views),size=0.2, alpha=0.5, width = 0.3, color ="red")+
  scale_y_continuous(labels = comma)+
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# CA Dashboard ###############################################################
#####VIEW COUNTS#####
ca1 <- CAvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of views", subtitle = "Canada") +
  geom_density(aes(x=views, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma) +
  labs(x = "View Count")

######LIKE AND DISLIKE COUNTS#####
ca2 <- CAvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of likes and dislikes", subtitle = "Canada") +
  #geom_histogram(aes(x=views, y=stat(density)),binwidth=0.1) +
  geom_density(aes(x=likes, y=stat(density)),fill="#F7000B", color="red",alpha=0.7) +
  geom_density(aes(x=dislikes, y=stat(density)),fill="black", color="black",alpha=0.7) +
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Like and Dislike Count")

#####COMMENT COUNTS#####
ca3 <- CAvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of comments", subtitle = "Canada") +
  geom_density(aes(x=comment_count, y=stat(density)),fill="#F7000B", color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####DAYS TO TREND COUNTS#####
ca4 <- CAvideos_vis %>%
  ggplot()+
  ggtitle(label = "No. of days to trend", subtitle = "Canada") +
  #geom_histogram(aes(x = days_to_trend), fill="#F7000B", alpha=0.7)+
  geom_density(aes(x=days_to_trend, y=stat(density)),fill="#F7000B",color="red", alpha=0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_log10(labels = comma)

#####CATEGORY COUNT#####
ca5 <- CAvideos_vis %>%
  mutate(category_id=as.character(category_id)) %>%
  group_by(category_id) %>%
  mutate(num=n()) %>%
  ungroup() %>% 
  ggplot() +
  ggtitle(label = "Videos and views as per category", subtitle = "Canada") +
  geom_col(aes(x=category_id, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= category_id, y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_discrete(labels = c("Film & Animation", "Music", "Pets & Animals", "Sports", "Travel & Events", "Autos & Vehicles", "Gaming", "People & Blogs", "Comedy", "Entertainment", "News & Politics", "Howto & Style", "Education","Science & Technology", "Shows", "Nonprofit"))+
  scale_y_continuous(labels = comma)+
  coord_flip()

#####weekday#####
ca6 <- CAvideos_vis %>%
  group_by(weekday) %>%
  mutate(num=n()) %>%
  ggplot(aes(x=weekday)) +
  ggtitle(label = "Videos and views as per Weekday", subtitle = "Canada") +
  geom_col(aes(y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(y= views),size=0.2,alpha=0.5, width=0.3,color="black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  coord_flip()

#####MONTH#####
ca7 <- CAvideos_vis %>%
  #mutate(time_month=as.factor(time_month)) %>%
  group_by(month) %>%
  arrange(month) %>% 
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per month of publish", subtitle = "Canada") +
  geom_col(aes(x=month, y=num),fill="#F7000B", alpha=0.7) +
  geom_jitter(aes(x= month, y= views),size=0.2,alpha=0.5, width=0.3) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  coord_flip()

##### HOUR - PUBLISH#####
ca8 <- CAvideos_vis %>%
  #mutate(time_hour=as.factor(time_hour)) %>%
  group_by(hour) %>%
  mutate(num=n()) %>%
  ggplot() +
  ggtitle(label = "Videos and views as per hour of the day", subtitle = "Canada") +
  geom_col(aes(x=hour, y=num),fill="#F7000B", alpha=0.7) + 
  geom_jitter(aes(x= hour, y= views),size=0.2,alpha=0.5, width=0.3) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  theme_minimal() +
  theme(panel.grid = element_blank())+
  coord_flip()

#####LIKES/DISLIKES RATE#####
ca9 <- CAvideos_vis %>%
  ggplot()+
  ggtitle(label = "Likes / Dislike Ratio", subtitle = "Canada") +
  geom_jitter(aes(x=LD_ratio, y= views),size=0.2, alpha=0.5, width = 0.3, color ="red")+
  scale_y_continuous(labels = comma)+
  scale_x_log10(labels = comma) +
  theme_minimal() +
  theme(panel.grid = element_blank())


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
    h2("Welcome to YOUTRENDS - your guide to popularity!"),
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
ui <- dashboardPage(title = 'Youtrends', header, sidebar, body, skin = 'red')


################################################################
##################### R-Shiny App SERVER #######################
################################################################

# create the server functions for the dashboard
server <- function(input, output) {
  
  ############ Title image##############
  #  output$home_yt_dash <- renderImage({
  #      list(src = "Youtrends_icon_large.jpg")
  #    }, deleteFile = FALSE)
  
  ############ Home page images##############
  output$home_yt_dash <- renderImage({
    list(src = "home_yt_dash.gif", width = 1000, height = 500)
  }, deleteFile = FALSE)
  output$home_text <- renderImage({
    list(src = "home_text.png", width = 1000, height = 400)
  }, deleteFile = FALSE)
  
  ############ Global Dashboard images##############
  output$cc1 <- renderPlot({cc1})
  output$cc2 <- renderPlot({
    cols <- c("#eca9a7", "#e07571",  "#d8504b",  "#c4302b")
    percentlabels<- round(100*value/sum(value), 1)
    pielabels<- paste(percentlabels, "%", sep="")
    pie(value, main="Likes by Time of Day", col=cols, labels=pielabels, cex=0.75)
    legend("bottomleft", title = "Time of Day : Local Hour Intervals" ,legend = group, cex=0.7, fill=cols, bty='o')
  })
  output$cc3 <- renderPlot({cc3})
  
  ############ US Dashboard images##############
  output$us1 <- renderPlot({us1})
  output$us2 <- renderPlot({us2})
  output$us3 <- renderPlot({us3})
  output$us4 <- renderPlot({us4})
  output$us5 <- renderPlot({us5})
  output$us6 <- renderPlot({us6})
  output$us7 <- renderPlot({us7})
  output$us8 <- renderPlot({us8})
  output$us9 <- renderPlot({us9})
  
  ############ GB Dashboard images##############
  output$gb1 <- renderPlot({gb1})
  output$gb2 <- renderPlot({gb2})
  output$gb3 <- renderPlot({gb3})
  output$gb4 <- renderPlot({gb4})
  output$gb5 <- renderPlot({gb5})
  output$gb6 <- renderPlot({gb6})
  output$gb7 <- renderPlot({gb7})
  output$gb8 <- renderPlot({gb8})
  output$gb9 <- renderPlot({gb9}) 
  
  ############ CA Dashboard images##############
  output$ca1 <- renderPlot({ca1})
  output$ca2 <- renderPlot({ca2})
  output$ca3 <- renderPlot({ca3})
  output$ca4 <- renderPlot({ca4})
  output$ca5 <- renderPlot({ca5})
  output$ca6 <- renderPlot({ca6})
  output$ca7 <- renderPlot({ca7})
  output$ca8 <- renderPlot({ca8})
  output$ca9 <- renderPlot({ca9})
  
  ############ Popularity Prediction ##############
  
  output$popularityimage <- renderImage({
    
    if (input$cn_country == 'us') {model_df <- uploaded_model_us}
    else if (input$cn_country == 'gb') {model_df <- uploaded_model_gb}
    else if (input$cn_country == 'ca') {model_df <- uploaded_model_ca}
    
    #df1['cn_country'] <- as.factor(input$cn_country)
    df1['category_id'] <- as.factor(input$category_id)
    df1['month'] <- as.factor(input$month)
    df1['weekday'] <- as.factor(input$weekday)
    df1['dayparting_name'] <- as.factor(input$dayparting_name)
    df1['desc_row'] <- as.numeric(input$desc_row)
    df1['num_tags'] <- as.numeric(input$num_tags)
    df1['title_length'] <- as.numeric(input$title_length)
    df1['title_upper'] <- as.numeric(input$title_upper)
    
    df1 <- as.h2o(df1)
    
    pred <- h2o.predict(model_df, df1)
    result <- pred[1,1]
    
    if (result == "Hyper Popular")
    {return(list(src = "hyper_popular.JPG", width = 400, height = 400))}
    else if (result == "Highly Popular")
    {return(list(src = "highly_popular.JPG", width = 400, height = 400))}
    else if (result == "Very Popular")
    {return(list(src = "very_popular.JPG",  width = 400, height = 400))}
    else if (result == "Popular")
    {return(list(src = "popular.JPG",  width = 400, height = 400))}
  },deleteFile=FALSE)
  
  # shutdown your cluster
  #h2o.shutdown()
  #Y
}

######################################################
# Knitting ui and server
#######################################################

shinyApp(ui, server)
#runApp()

#####################################################
# Deploy APP
####################################################
library(rsconnect)

rsconnect::setAccountInfo(name='prerakpatel',
                          token='4719CA490E2BF53C94D004A67F351518',
                          secret='zvXl15pnqOQDikN3HQ5rTyGIgk/KFuzjZTOTnLqN')

#rsconnect::deployApp("~/R material/Project/data")

tmp.enc <- options()$encoding
options(encoding = "UTF-8")
deployApp()
Y
options(encoding = tmp.enc)

# https://prerakpatel.shinyapps.io/data/


