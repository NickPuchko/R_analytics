#grot <- as.numeric(as.matrix(read_excel("grot.xlsx", col_names = FALSE)))
#pogrebok <- as.numeric(as.matrix(read_excel("pogrebok.xlsx", col_names = FALSE)))
#primorie <- as.numeric(as.matrix(read_excel("primorie.xlsx", col_names = FALSE)))
#fm1 <- as.numeric(as.matrix(read_excel("fm1.xlsx", col_names = FALSE)))
#rsconnect::configureApp("Abrau_Analytics_initial", size = "small")
rsconnect::setAccountInfo(name="nikolaipuchko", token="A4148E55E9A24FA1570B6D8D53C43714", secret="oHnwZGnc2NUMJrWzMyJ0075g6xpPeg6cI/ztrKQO")
library(shinythemes)
ui <- fluidPage(theme = shinytheme("yeti"),
  titlePanel("Интервальный анализ продаж"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Количество интервалов:",
                  min = 5,
                  max = 100,
                  value = 20,
                  animate = TRUE)
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)


server <- function(input, output) {
  output$distPlot <- renderPlot({
    prepare_data <- function(x, high){
      x <- na.omit(x)
      x <- x[ x > 0]
      x <- x[ x < high]
      return(x)
    }
    library(readxl)
    gpp <- as.numeric(as.matrix(read_excel("GPP.xlsx", col_names = FALSE)))
    d <- prepare_data(gpp, 5000)
    bins <- seq(min(d), max(d), length.out = input$bins + 1)
    hist(d, breaks = bins, col = "steelblue", border = "white", xlab = "Размер чека, ₽", ylab = "Частота", main = "Гистограмма распределения чеков")
  })
}


rsconnect::showLogs()
shinyApp(ui=ui, server=server)

#Пример для подгонки параметров
#d <- prepare_data(gpp, 2000)
#hist(d, breaks = 50, col = "steelblue", xlab = "Размер чека, ₽", ylab = "Частота", main = "Гистограмма распределения чеков")

#Сравнение распределение для 4 магазинов
#d1 <- prepare_data(fm1, 2000)
#d2 <- prepare_data(pogrebok, 2000)
#d3 <- prepare_data(primorie, 2000)
#d4 <- prepare_data(grot, 2000)
#plot(d1, col = "red")
#points(d2, col = "orange")
#points(d3, col = "blue")
#points(d4, col = "green")

#Гистограммы для отдельных магаизнов
#hist(d1, breaks = "sturges", col = "blue")
#hist(d2, breaks = "sturges", col = "blue")
#hist(d3, breaks = "sturges", col = "blue")
#hist(d4, breaks = "sturges", col = "blue")