

#===============================================================================
#PRE-BUILDING FEATURES
#===============================================================================


#==========================================
#PACKAGES
#==========================================


library(shiny)
library(sf)
library(tidyverse)
library(readxl)
library(numbers)
library(stringr)
library(ggpubr)
library(maps)
library(plotly)
library(DT)
library(rlang)
library(shinythemes)

#==========================================
#FUNCTIONS
#==========================================

#Transform dates - make them readable and easy to index

transform_dates<- function(dataset){ #Accepts the data set as an argument
  months<- c(1:(ncol(dataset)-1)) #Get the total number of months
  years<-div((months-1),12) + 2015    # Tricky: the DS starts from 2015. So, we take the
  # result of floor division of the month's number 
  # and add the base year of 2015 to each element of the vector
  
  months[months>12] <- (months[months>12] %% 12) #Now we just need the month's number (remainder of division by 12)
  months[months==0] <- 12  #Change zeros to 12 
  
  #And then concatenate the string/date name:
  dates<- str_pad(string=paste(as.character(months), 
                               "01", as.character(years), 
                               sep="-"), #Gives the date in the format M-DD-YYYY
                  width=10, 
                  side="left",
                  pad="0") #Pad it. The output: MM-DD-YYYY
  colnames(dataset)[2:ncol(dataset)]<- dates #Change the colnames in the dataset 
  
  return(dataset) #Return the dataset
}

#==========================================
#UI ELEMENTS
#==========================================

rosstat_elements<- c("#1	Индекс промышленного производства по субъектам Российской Федерации (в % к соответствующему месяцу предыдущего года)",															
  "#2	Индекс промышленного производства по субъектам Российской Федерации (в % к соответствующему периоду предыдущего года)",															
  "#3	Индекс промышленного производства по субъектам Российской Федерации (в % к предыдущему месяцу)", 															
  "#4	Индекс производства по виду экономической деятельности «Добыча полезных ископаемых» по субъектам Российской Федерации (в % к соответствующему месяцу предыдущего года)",															
  "#5	Индекс производства по виду экономической деятельности «Добыча полезных ископаемых» по субъектам Российской Федерации (в % к соответствующему периоду предыдущего года)",															
  "#6	Индекс производства по виду экономической деятельности «Добыча полезных ископаемых» по субъектам Российской Федерации (в % к предыдущему месяцу)",				
  "#7	Индекс производства по виду экономической деятельности «Обрабатывающие производства» по субъектам Российской Федерации (в % к соответствующему месяцу предыдущего года)",											
  "#8	Индекс производства по виду экономической деятельности «Обрабатывающие производства» по субъектам Российской Федерации (в % к соответствующему периоду предыдущего года)",															
  "#9	Индекс производства по виду экономической деятельности «Обрабатывающие производства» по субъектам Российской Федерации (в % к предыдущему месяцу)",
  "#10	Индекс производства по виду экономической деятельности «Обеспечение электрической энергией, газом и паром; кондиционирование воздуха» по субъектам Российской Федерации (в % к соответствующему месяцу предыдущего года)",															
  "#11	Индекс производства по виду экономической деятельности «Обеспечение электрической энергией, газом и паром; кондиционирование воздуха» по субъектам Российской Федерации (в % к соответствующему периоду предыдущего года)",															
  "#12	Индекс производства по виду экономической деятельности «Обеспечение электрической энергией, газом и паром; кондиционирование воздуха» по субъектам Российской Федерации (в % к предыдущему месяцу)",			
  "#13	Индекс производства по виду экономической деятельности «Водоснабжение; водоотведение, организация сбора и утилизации отходов, деятельность по ликвидации загрязнений» по субъектам Российской Федерации (в % к соответствующему месяцу предыдущего года)",													
  "#14	Индекс производства по виду экономической деятельности «Водоснабжение; водоотведение, организация сбоzра и утилизации отходов, деятельность по ликвидации загрязнений» по субъектам Российской Федерации (в % к соответствующему периоду предыдущего года)",															
  "#15	Индекс производства по виду экономической деятельности «Водоснабжение; водоотведение, организация сбора и утилизации отходов, деятельность по ликвидации загрязнений» по субъектам Российской Федерации (в % к предыдущему месяцу)"										
  )


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),

    # Application title
    titlePanel("Экономическая география эпохи СВО"),
    p("Данное приложение разработано с целью визуализировать официальные данные по основным экономическим показателям на территориях, подконтрольных Российской Федерации. Используемые данные были взяты с сайта", tags$a(href="https://rosstat.gov.ru/enterprise_industrial", "Федеральной службы государственной статистики РФ.")),
    p("Дата последнего обновления данных: 12 августа 2023 года"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="DataType",
                        label = "Выберите статистику для визуализации",
                        choices=rosstat_elements,
                        selected=rosstat_elements[1],
                        multiple=FALSE),
            numericInput(inputId="Year",
                         label="Выберите год для визуализации статистики",
                         value=2023,
                         min=2015,
                         max=2023,
                         step=1),
            numericInput(inputId="Month",
                         label="... и месяц года",
                         min=1,
                         value=1,
                         max=12,
                         step=1),
            actionButton(inputId="Update",
                         label="Обновить результаты")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("Map_Russia"),
           textOutput("caption"),
           br(),
           plotlyOutput("Best_Worst"),
           h6("Данные за последние 12 месяцев"),
           DTOutput("Index_Data")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #========================================================================
  # Clean the data for the map (including Crimea)
  #========================================================================
  
   
  Russia_shape<- st_read("gadm41_RUS_1.shp") #Russia's metadata
  Russia_shape<- Russia_shape %>%
    arrange(NL_NAME_1)
  
  #Don't touch anything here - the process of data cleaning is very sensitive to iterations
  
  Russia_shape["NL_NAME_1"]$NL_NAME_1[1]<- "Еврейская авт.область"
  Russia_shape["NL_NAME_1"]$NL_NAME_1[2]<- "г. Москва"
  Russia_shape["NL_NAME_1"]$NL_NAME_1[44]<- "Республика Ингушетия"
  Russia_shape<- Russia_shape %>%
    arrange(NL_NAME_1)
  
  Ukraine_shape<- st_read("gadm41_UKR_1.shp") #Ukraine's metadata
  
  
  
  #Russia_shape["Number"]<- as.factor(1:nrow(Russia_shape["NAME_1"])) || sample line
  #Russian_regions<- Russia_shape["NL_NAME_1"]$NL_NAME_1
  
  
  #Check where the files differ
  colnames(Ukraine_shape)
  colnames(Russia_shape)
  
  #Correct the Ukrainian file 
  
  Ukraine_shape["GID_0"]<- "RUS"
  Ukraine_shape["COUNTRY"]<-"Russia"
  
  #Attach Crimea
  
  Russia_shape<-rbind(Russia_shape, Ukraine_shape[5,])
  
  # Again, don't touch anything here
  
  Russia_shape["NL_NAME_1"]$NL_NAME_1[nrow(Russia_shape)]<- "Республика Крым"
  Russia_shape<- Russia_shape %>%
    arrange(NL_NAME_1)
  Russia_shape["NL_NAME_1"]$NL_NAME_1[60]<- "Чеченская Республика"
  Russia_shape<- Russia_shape %>%
    arrange(NL_NAME_1)
  
  

  #========================================================================
  # Get the right sheet from the dataset
  #========================================================================
  
  Sheet<- eventReactive(input$Update, {
    if (input$DataType== rosstat_elements[1]){
      x="1"
    } else if (input$DataType== rosstat_elements[2]) {
      x="2"
    } else if (input$DataType== rosstat_elements[3]){
      x="3"
    } else if (input$DataType== rosstat_elements[4]){
      x="4"
    } else if (input$DataType== rosstat_elements[5]){
      x="5"
    } else if (input$DataType== rosstat_elements[6]){
      x="6"
    } else if (input$DataType== rosstat_elements[7]){
      x="7"
    } else if (input$DataType== rosstat_elements[8]){
      x="8"
    } else if (input$DataType== rosstat_elements[9]){
      x="9"
    } else if (input$DataType== rosstat_elements[10]){
      x="10"
    } else if (input$DataType== rosstat_elements[11]){
      x="11"
    } else if (input$DataType== rosstat_elements[12]){
      x="12"
    } else if (input$DataType== rosstat_elements[13]){
      x="13"
    } else if (input$DataType== rosstat_elements[14]){
      x="14"
    } else if (input$DataType== rosstat_elements[15]){
      x="15"
    }
    
    
    return(x)  
  })

  industrial_production<- eventReactive(input$Update, {
    
    sheet_no<-Sheet()
    
    if (sheet_no %in% c("1", "2", "3", "4", "5", "6")) {
    industrial_production<-read_excel("Production_Indices.xlsx", sheet=as.character(sheet_no), skip=4)
    
    colnames(industrial_production)[1]<-"Регион"
    
    industrial_production <- (head(industrial_production, -2) %>%
      dplyr::mutate(Регион=dplyr::case_when((str_detect(`Регион`, pattern="Ненецкий авт") & ! str_detect(`Регион`, pattern="Ямало")) ~ "Ненецкий АО",
                              str_detect(`Регион`, pattern="Ханты") ~ "Ханты-Мансийский АО",
                              str_detect(`Регион`, pattern="Санкт-Петербург") ~ "Санкт-Петербург",
                              str_detect(`Регион`, pattern="Москва") ~ "г. Москва",
                              str_detect(`Регион`, pattern="Чукотский") ~ "Чукотский АО",
                              str_detect(`Регион`, pattern="Ямало") ~ "Ямало-Ненецкий АО",
                              TRUE~`Регион`)) %>%
      dplyr::filter(!str_detect(`Регион`, pattern="округ"),
             !str_detect(`Регион`, pattern="Российская"),
             !str_detect(`Регион`, pattern="г. Севастополь")) %>%
      dplyr::arrange(`Регион`))
    }
    
    else {
      industrial_production<-read_excel("Production_Indices.xlsx", sheet=as.character(sheet_no), skip=4)
      
      colnames(industrial_production)[1]<-"Регион"
      
      industrial_production <- (head(industrial_production, -1) %>%
                                  dplyr::mutate(Регион=dplyr::case_when((str_detect(`Регион`, pattern="Ненецкий авт") & ! str_detect(`Регион`, pattern="Ямало"))~ "Ненецкий АО",
                                                                        str_detect(`Регион`, pattern="Ханты") ~ "Ханты-Мансийский АО",
                                                                        str_detect(`Регион`, pattern="Санкт-Петербург") ~ "Санкт-Петербург",
                                                                        str_detect(`Регион`, pattern="Москва") ~ "г. Москва",
                                                                        str_detect(`Регион`, pattern="Чукотский") ~ "Чукотский АО",
                                                                        str_detect(`Регион`, pattern="Ямало") ~ "Ямало-Ненецкий АО",
                                                                        TRUE~`Регион`)) %>%
                                  dplyr::filter(!str_detect(`Регион`, pattern="округ"),
                                                !str_detect(`Регион`, pattern="Российская"),
                                                !str_detect(`Регион`, pattern="г. Севастополь")) %>%
                                  dplyr::arrange(`Регион`))
      
    }
    
    
    industrial_production<- transform_dates(industrial_production)
    
    #In case the values don't come as numbers
    
    industrial_production[, 2:ncol(industrial_production)] <- apply(industrial_production[, 2:ncol(industrial_production)], MARGIN=2, FUN=as.numeric)
    
    
    return(industrial_production)
  
    })
  
  
  date_column<- eventReactive(input$Update, {
    year_string<- as.character(input$Year)
    month_string<- as.character(input$Month)
    
    date_string<-str_pad(string=paste(month_string, 
                               "01", 
                               year_string, 
                               sep="-"), #Gives the date in the format M-DD-YYYY
                  width=10, 
                  side="left",
                  pad="0") #Pad it. The output: MM-DD-YYYY
    
    return(date_string)
    
  })

  map_data_vector<- eventReactive(input$Update, {
    
    industrial_production<- industrial_production()
    date_column<-date_column()
    data_vector2<-(industrial_production[, date_column]-100)
    
    # Get a subset of NAs, keep the index. But be careful: the dataset might not have NAs!
    
    if(sum(is.na(data_vector2[,1])!=0)){  #If there are NAs....
      NAs<-subset(data_vector2, is.na(date_column))
      
      #Get a subset of negative values, keep the index
      
      negatives<- case_when(data_vector2>=0 ~ NA, TRUE ~ data_vector2) %>% na.omit()
      
      #Get a subset of positive values, keep the index 
      
      positives<- case_when(data_vector2<0 ~ NA, TRUE ~ data_vector2)  %>% na.omit()
      
      # Assign the "tier" within each category. 3 per category 
      negatives$tier<- cut(negatives[, 1], breaks=quantile(negatives[, 1], probs=seq(0,1, 1/3)), labels=c("Сильная просадка", "Средняя просадка", "Слабая просадка"))
      negatives$index<-rownames(negatives) #this will make sure the index doesn't get lost
      positives$tier<-cut(positives[, 1], breaks=quantile(positives[, 1], probs=seq(0,1, 1/3)), labels=c("Слабый рост", "Средний рост", "Сильный рост"))
      positives$index<-rownames(positives)
      NAs$tier<-NA #This is needed for concatenation at a later stage
      
      #Concatenate the tables 
      
      values_recreated<-rbind(positives, negatives, NAs) 
    } else { #And if there aren't...
      
      negatives<- case_when(data_vector2>=0 ~ NA, TRUE ~ data_vector2) %>% na.omit()
      positives<- case_when(data_vector2<0 ~ NA, TRUE ~ data_vector2)  %>% na.omit()
      
      # Assign the "tier" within each category. 3 per category 
      negatives$tier<- cut(negatives[, 1], breaks=quantile(negatives[, 1], probs=seq(0,1, 1/3)), labels=c("Сильная просадка", "Средняя просадка", "Слабая просадка"))
      negatives$index<-rownames(negatives) #this will make sure the index doesn't get lost
      positives$tier<-cut(positives[, 1], breaks=quantile(positives[, 1], probs=seq(0,1, 1/3)), labels=c("Слабый рост", "Средний рост", "Сильный рост"))
      positives$index<-rownames(positives)
      values_recreated<-rbind(positives, negatives) 
      
    }
    #Arrange by the index - this way you recreate the original dataset but now with tiers
    values_recreated<- values_recreated %>%
      mutate(index=as.numeric(index)) %>%
      arrange(index)
    # Check whether the values are the same! Should be a straight line 
    
    #values_recreated %>%
    #  mutate(originals=data_vector2[,1]) %>%
    #  ggplot(aes(x=`05-01-2023`, y=originals)) + geom_point()
    
    
    #Now you need to relevel the factor:
    
    values_recreated<- values_recreated %>%
      mutate(tier=factor(tier, levels=c("Сильная просадка", "Средняя просадка", "Слабая просадка",
                                        "Слабый рост", "Средний рост", "Сильный рост")))
    
    #Check the order of values 
    
    return(values_recreated$tier)
    
  })  
  
  map_nominal_values<- eventReactive(input$Update, {
    industrial_production<- industrial_production()
    date_column<-date_column()
    data_vector2<-(industrial_production[, date_column]-100)
    
    return(round(data_vector2, 2))
  })
  
  
  negatives<- eventReactive(input$Update,{
    industrial_production<- industrial_production()
    date_column<-date_column()
    data_vector2<-(industrial_production[, date_column]-100)
    negatives<- case_when(data_vector2>=0 ~ NA, TRUE ~ data_vector2) %>% na.omit()
    
    negative_bins<-paste(as.character(round(quantile(negatives[, 1], probs=seq(0,1, 1/3)), 0)), "%", sep="")[1:3]
    return(negative_bins)
  })
  
  positives<- eventReactive(input$Update,{
    industrial_production<- industrial_production()
    date_column<-date_column()
    data_vector2<-(industrial_production[, date_column]-100)
    positives<- case_when(data_vector2<0 ~ NA, TRUE ~ data_vector2)  %>% na.omit()
    
    positive_bins<-paste(as.character(round(quantile(positives[, 1], probs=seq(0,1, 1/3)), 0)), "%", sep="")[2:4]
    return(positive_bins)
  })
  
  
  plot_title<- eventReactive(input$Update, {
    
    if (input$DataType== rosstat_elements[1]){
      x="Индекс промышленного производства по субъектам РФ" 
      y= "Изменения приведены в процентном изменении по отношению к соответствующему месяцу предыдущего года."
    } else if (input$DataType== rosstat_elements[2]) {
      x="Индекс промышленного производства по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему периоду предыдущего года."
    } else if (input$DataType== rosstat_elements[3]){
      x="Индекс промышленного производства по субъектам РФ" 
      y="(Изменения приведены в процентном изменении по отношению к предыдущему месяцу"
    } else if (input$DataType== rosstat_elements[4]){
      x="Добыча полезных ископаемых по субъектам РФ" 
      y = "Изменения приведены в процентном изменении по отношению к соответствующему месяцу предыдущего года."
    } else if (input$DataType== rosstat_elements[5]){
      x="Добыча полезных ископаемых по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему периоду предыдущего года."
    } else if (input$DataType== rosstat_elements[6]){
      x="Добыча полезных ископаемых по субъектам РФ" 
      y="(Изменения приведены в процентном изменении по отношению к предыдущему месяцу)."
    } else if (input$DataType== rosstat_elements[7]){
      x="Обрабатывающие производства по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему месяцу предыдущего года."
    } else if (input$DataType== rosstat_elements[8]){
      x="Обрабатывающие производства по субъектам РФ" 
      y="(в % к соответствующему периоду предыдущего года."
    } else if (input$DataType== rosstat_elements[9]){
      x="Обрабатывающие производства по субъектам РФ" 
      y ="Изменения приведены в процентном изменении по отношению к предыдущему месяцу."
    } else if (input$DataType== rosstat_elements[10]){
      x="Обеспечение электрической энергией, газом и паром; кондиционирование воздуха по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему месяцу предыдущего года."
    } else if (input$DataType== rosstat_elements[11]){
      x="Обеспечение электрической энергией, газом и паром; кондиционирование воздуха по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему периоду предыдущего года."
    } else if (input$DataType== rosstat_elements[12]){
      x="Обеспечение электрической энергией, газом и паром; кондиционирование воздуха по субъектам РФ"
      y="Изменения приведены в процентном изменении по отношению к предыдущему месяцу."
    } else if (input$DataType== rosstat_elements[13]){
      x="Водоснабжение; водоотведение, организация сбора и утилизации отходов, деятельность по ликвидации загрязнений по субъектам РФ"
      y="Изменения приведены в процентном изменении по отношению к соответствующему месяцу предыдущего года."
    } else if (input$DataType== rosstat_elements[14]){
      x="Водоснабжение; водоотведение, организация сбора и утилизации отходов, деятельность по ликвидации загрязнений» по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к соответствующему периоду предыдущего года."
    } else if (input$DataType== rosstat_elements[15]){
      x="«Водоснабжение; водоотведение, организация сбора и утилизации отходов, деятельность по ликвидации загрязнений по субъектам РФ" 
      y="Изменения приведены в процентном изменении по отношению к предыдущему месяцу."
    }
    
    if (input$Month==1) {
      z<-"январь"
      } else if (input$Month==2) {
        z<-"февраль" 
      } else if (input$Month==3) {
        z<-"март"
      } else if (input$Month==4) {
        z<-"апрель" 
        } else if (input$Month==5) {
          z<-"май"
        } else if (input$Month==6) {
          z<-"июнь"
        } else if (input$Month==7) {
          z<-"июль" 
        } else if (input$Month==8) {
          z<-"август"
        } else if (input$Month==9) {
          z<-"сентябрь"
        } else if (input$Month==10) {
          z<-"октябрь"
        } else if (input$Month==11) {
          z<-"ноябрь"
        } else if (input$Month==12) {
          z<-"декабрь"
        }
        
    
    z<- paste("Данные за", z, as.character(input$Year), "года.", sep=" ")
    
    return(c(x, y, z))  
  })
  
  Render_Table<- eventReactive(input$Update, {
    x<-industrial_production()
    
    sheet_no<-Sheet()
    
    industrial_production<-read_excel("Production_Indices.xlsx", sheet=as.character(sheet_no), skip=4)
      
    colnames(industrial_production)[1]<-"Регион"
    
    industrial_production <- industrial_production %>%
      filter(str_detect(`Регион`, pattern="Российская Федерация"))
    
    colnames(industrial_production)<- colnames(x)
    
    x<- rbind(industrial_production, x)
    
    
    
    return(x)  
    
    
  })
  
  
  Plot_BestWorst<- eventReactive(input$Update, {
    sheet_no<- Sheet()
    date_column<-date_column()
    industrial_production<-Render_Table()
    
    best<- industrial_production %>%
      dplyr::filter(!str_detect(Регион, pattern="Российская Федерация")) %>%
      arrange_at(ncol(.), desc) %>%
      dplyr:: filter(row_number() %in% c(1:3)) %>%
      pivot_longer(-c("Регион"), names_to="Дата", values_to="Изменение") %>%
      mutate(Дата=as.Date(`Дата`, format="%m-%d-%Y"),
             Изменение=Изменение-100)
    
    best[, "Группа"]<- rep("Лучшие показатели", nrow(best))
    
    worst<- industrial_production %>%
      filter(!str_detect(Регион, pattern="Российская Федерация")) %>%
      arrange_at(ncol(.)) %>%
      filter(row_number() %in% c(1:3)) %>%
      pivot_longer(-c("Регион"), names_to="Дата", values_to="Изменение") %>%
      mutate(Дата=as.Date(`Дата`, format="%m-%d-%Y"),
             Изменение=Изменение-100)
    
    worst[, "Группа"]<- rep("Худшие показатели", nrow(worst))  
    
    
    ds_russia <- read_excel("Production_Indices.xlsx", sheet=sheet_no, skip=4)
    
    colnames(ds_russia)[1]<-"Регион"
    
    ds_russia<- ds_russia %>%
      dplyr::mutate(Регион=dplyr::case_when((str_detect(`Регион`, pattern="Ненецкий авт") & ! str_detect(`Регион`, pattern="Ямало")) ~ "Ненецкий АО",
                                            str_detect(`Регион`, pattern="Ханты") ~ "Ханты-Мансийский АО",
                                            str_detect(`Регион`, pattern="Санкт-Петербург") ~ "Санкт-Петербург",
                                            str_detect(`Регион`, pattern="Москва") ~ "г. Москва",
                                            str_detect(`Регион`, pattern="Чукотский") ~ "Чукотский АО",
                                            str_detect(`Регион`, pattern="Ямало") ~ "Ямало-Ненецкий АО",
                                            TRUE~`Регион`)) %>%
      dplyr::filter(!str_detect(`Регион`, pattern="округ"),
                    !str_detect(`Регион`, pattern="г. Севастополь")) %>%
      dplyr::arrange(`Регион`)
    
    ds_russia<- transform_dates(ds_russia)
    
    ds_russia[, 2:ncol(ds_russia)] <- apply(ds_russia[, 2:ncol(ds_russia)], MARGIN=2, FUN=as.numeric)
    
    
    Russia<-ds_russia  %>%
      filter(str_detect(Регион, pattern="Российская")) %>%
      pivot_longer(-c("Регион"), names_to="Дата", values_to="Изменение") %>%
      mutate(Дата=as.Date(`Дата`, format="%m-%d-%Y"),
             Изменение=(Изменение-100))
    
    
    Russia[, "Группа"]<- rep("Среднее значение по России", nrow(Russia))
    
    out<-rbind(worst, best, Russia)
    out$Группа<-as.factor(out$Группа)
    
    
    
    return(out)
      
  })
  
  output$Map_Russia<- renderPlotly({
    Shape<-Russia_shape
    Shape$Регион<-Shape$NL_NAME_1
    nominals<- map_nominal_values()
    meta_data<- c()
    
    for (i in c(1:nrow(Shape))){
      meta_data[i]<- paste("Регион -", Shape$NL_NAME_1[i], "; Изменение показателя: ", as.character(nominals[i, 1]))
    }
    economic_data<- map_data_vector()
    Shape$Категория<- economic_data
    Shape$Информация<-meta_data
    negative_bins<-negatives()
    positive_bins<-positives()
    titles<-plot_title()
    
    graph<-ggplot(Shape) +
      geom_sf(aes(fill=Категория, group=Информация)) +
      coord_sf(crs = st_crs(2477)) + # list of EPSG systems: https://epsg.io/?q=Russian Federation kind:PROJCRS
      scale_fill_brewer(palette="RdYlGn") +
      ggtitle(titles[1],
              subtitle= titles[2]) +
      theme(panel.grid.major = element_line(color = gray(.5),
                                            linetype = "dashed",
                                            linewidth = 0.1),
            panel.background = element_rect(fill = "aliceblue"),
            legend.title=element_blank()) 
    
    
    
    return(print(graph))
    
  })
  
  output$caption<- renderText({
    
    negative_bins<-negatives()
    positive_bins<-positives()
    titles<- plot_title()
    
  print(paste(titles[3], titles[2],
    "Категории основаны на относительных показателях региона в выбранный период.
    
    На данном графике, деление на категории построено по следующему принципу:
    
    Сильная просадка - падение от", negative_bins[1], "до", negative_bins[2],
    "; Средняя просадка -  падение от", negative_bins[2], "до", negative_bins[3],
    "; Слабая просадка - падение от", negative_bins[3], "до 0;", 
    "  
    ",
    "
    Слабый рост - рост от 0", "до", positive_bins[1],
    "; Средний рост- рост от", positive_bins[1], "до", positive_bins[2],
    "; Сильный рост - рост от", positive_bins[2], "до", positive_bins[3],
    sep=" "))
  }) 

  
  output$Best_Worst<- renderPlotly({
    
    out <- Plot_BestWorst()
    graph<- ggplot(data=subset(out, !(Регион %in% "Российская Федерация")), 
             aes(x=Дата, y=Изменение)) + 
      geom_line(aes(colour=Группа, linetype=Регион)) +
      scale_color_brewer(palette="Set2") +
      geom_line(data=subset(out, (Группа %in% "Среднее значение по России")),
                colour="black", 
                linewidth=1, 
                alpha=0.7, 
                linetype="twodash", 
                aes(group=Группа)) +
      labs(title= "Сравнение долгосрочной динамики 3 лучших и 3 худших регионов за выбранный месяц",
           x="",
           y="Индекс") + theme_classic()
    
    print(graph)
  })
  
  output$Index_Data<- DT:: renderDataTable({
   x<-Render_Table()
   reg<- x[, "Регион"]
   x<- x[, c((ncol(x)-12):ncol(x))] #Return values only for the last year
   x<-cbind(reg, x)
   print(x)
    
  })   
  
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
