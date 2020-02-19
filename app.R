rsconnect::setAccountInfo(name="nikolaipuchko", token="A4148E55E9A24FA1570B6D8D53C43714", secret="oHnwZGnc2NUMJrWzMyJ0075g6xpPeg6cI/ztrKQO")
library(shinythemes)
library(DT)
#library(CarletonStats)
ui <- navbarPage(title = "17 вариант",
                 theme = shinytheme("flatly"),
                 tabPanel("About data",
                          sidebarLayout(
                              sidebarPanel(
                                  inputPanel(
                                      fileInput(inputId = "file_in",
                                                "Выберите .xlsx файл // выбор произвольного датасета временно недоступен",
                                                accept = c(".xlsx")
                                                )
                                  )
                                  ),
                              mainPanel(
                                  DT::dataTableOutput("data_out")
                              ),
                              position = "right"
                          )
                      ),
                 tabPanel("Histogram", 
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput(inputId = "bins",
                                              label = "Количество интервалов:",
                                              min = 5,
                                              max = 25,
                                              value = 8,
                                              animate = TRUE),
                                  
                                  
                                  sliderInput(inputId = "high",
                                              label = "Максимум часов:",
                                              min = 105,
                                              max = 110,
                                              step = 1,
                                              value = 110,
                                              animate = TRUE),
                                  
                                  
                                  sliderInput(inputId = "low",
                                              label = "Минимум часов:",
                                              min = 100,
                                              max = 104,
                                              step = 1,
                                              value = 100,
                                              animate = TRUE),
                                  
                                  
                                  radioButtons(inputId = "labels",
                                               label = "Подпись частот",
                                               c("Добавить" = TRUE,
                                                 "Не подписывать" = FALSE))
                              ),
                              mainPanel(
                                  plotOutput(outputId = "histPlot")
                              )
                          )
                 ),
                 
                 tabPanel("Distribution",
                          sidebarLayout(
                              sidebarPanel(
                                  checkboxGroupInput(inputId = "distribution_type",
                                                     label = "Добавить случайное распределение", 
                                                     c(
                                                       "Нормальное" = "norm",
                                                       "Равномерное" = "uniform"
                                                       )),
                              ),
                              mainPanel(
                                  plotOutput(outputId = "distPlot")
                              )
                          )
                          ),
                 
                 

                 
                 
                 tabPanel("Summary",
                          verticalLayout(
                              verbatimTextOutput("summary"),
                              verbatimTextOutput(outputId = "stplot"),
                              plotOutput(outputId = "quart")
                              )
                          )
                 
)


server <- function(input, output) {
    library(readxl)
    matrix_data <- as.matrix(read_excel("data1.xlsx", col_names = FALSE))
    data1 <- as.numeric(matrix_data)
    
   # data_input <- reactive({
    #    inFile <- input$file_in
     #   if (is.null(inFile)) { return(NULL) }    
      #  dataFile <- as.matrix(read_excel(inFile$datapath, col_names = FALSE))
       # return(dataFile)
    #})
    
    #data1 <- as.numeric(data_input)
    
    #data2 <- reactiveFileReader(1000, session, input$file_in, read_excel)

    #data <- reactive({ 
    #    fin <- input$file_in
    #    matrix_data <- as.matrix(read_excel(path = fin$datapath, col_names = FALSE))
    #    data1 <- as.numeric(matrix_data)
    #})

    prepare_data <- function(x){
        x <- na.omit(x)
        x <- x[ x > 0]
        return(x)}
    
    output$data_out = DT::renderDataTable({
        matrix_data
    })
    
    
    output$histPlot <- renderPlot({
        d <- prepare_data(data1)
        d <- d[ d > input$low ]
        d <- d[ d < input$high ]
        bins <- seq(min(d), max(d), length.out = input$bins + 1)
        bool_labels <- input$labels == TRUE
        hist(d, breaks = bins, freq = TRUE, labels = bool_labels,  col = "steelblue", border = "white", xlab = "Время, затраченное на аудиторскую проверку,  часы", ylab = "Частота, количество компаний", main = "Гистограмма")
    })
    
    output$distPlot <- renderPlot({
        d <- prepare_data(data1)
        sko = sqrt(var(d))
        average = mean(d)
        size = length(d)
        plot(d, col = "steelblue")
        if ("norm" %in% input$distribution_type) {
            points(rnorm(100, mean = average, sd = sko), col = "green")
        }
        if ("uniform" %in% input$distribution_type) {
            points(runif(100, min = min(d), max = max(d)), col = "red")
        }
    })
    
    
    output$stplot <- renderPrint({
        d <- prepare_data(data1)
        print(stem(d))
    })
    
    output$summary <- renderPrint({
        d <- prepare_data(data1)
        print(summary(d))
        print(quantile(d))
        print(IQR(d))
    })
    
    output$quart <- renderPlot({
        d <- prepare_data(data1)
        qqnorm(d, col = "steelblue")
    })
    
}


rsconnect::showLogs()
shinyApp(ui=ui, server=server)


