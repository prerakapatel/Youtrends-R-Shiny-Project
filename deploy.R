
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
library(shinydashboard)


######################################################
# Knitting ui and server
#######################################################

shinyApp(ui, server)
#runApp()

####################################################
library(rsconnect)

rsconnect::setAccountInfo(name='prerakpatel',
                          token='4719CA490E2BF53C94D004A67F351518',
                          secret='zvXl15pnqOQDikN3HQ5rTyGIgk/KFuzjZTOTnLqN')

#rsconnect::configureApp("data", size="xlarge")
#gc()

rsconnect::deployApp("~/R material/Project/data")

tmp.enc <- options()$encoding
options(encoding = "UTF-8")
deployApp()
Y
options(encoding = tmp.enc)

# https://prerakpatel.shinyapps.io/data/
# https://prerakpatel.shinyapps.io/Youtrends/
