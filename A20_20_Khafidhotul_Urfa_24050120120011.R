library(shiny)
library(shinythemes)

ui<-fluidPage(theme=shinytheme("cerulean"),
  titlePanel(title="Anuitas Matematika Finansial"),
  h6("Anuitas merupakan suatu rangkaian pembayaran/ penerimaan sejumlah uang. Umumnya sama besar dengan periode waktu yang sama untuk setiap pembayaran. Anuitas secara umum dibagi menjadi 3 bagian yaitu anuitas biasa, anuitas dimuka, dan anuitas ditunda. Sementara itu, untuk anuitas bertumbuh dan anuitas Variabel itu sebenarnya ada, akan tetapi jarang digunakan.",style="color:black"),
  navbarPage("CALCULATOR",
  navbarMenu("ANUITAS",
#Anuitas Biasa
  tabPanel("Anuitas Biasa",
           h3("ANUITAS BIASA"),
           sidebarLayout(
             sidebarPanel(
      textInput("a","Angsuran(A)",value = "0"),
      textInput("i", "Suku Bunga(i)",value="0.5"),
      textInput("n","Periode(n)",value="1"),
      actionButton("hitung","Kerjakan")),
    mainPanel(
      tabsetPanel(
        tabPanel("Present Value",
                 tags$h5("Nilai Present Value:"),
                 verbatimTextOutput("PVAB"),
                 tabsetPanel(
                   tabPanel("Grafik",
                            tags$h5("Grafik Present Value Anuitas Biasa",style="text-align:Center"),  
                            plotPVAB<-plotOutput("plotpv_ab")),
                    tabPanel("Tabel",
                             tags$h5("Tabel perhitungan Anuitas Biasa",style="text-align:Center"),
                             tablePVAB<-tableOutput("tabab")))),
      tabPanel("Future Value",
               tags$h5("Nilai Future Value:"),
               verbatimTextOutput("FVAB"),
               tabsetPanel(
                 tabPanel("Grafik",
                          tags$h5("Grafik Future Value Anuitas Biasa",style="text-align:Center"), 
                          plotFVAB<-plotOutput("plotfv_ab"))
    )))))),
#Anuitas Dimuka
  tabPanel("Anuitas Dimuka",
           h3("ANUITAS DIMUKA"),
           sidebarLayout(
             sidebarPanel(
               textInput("satu","Angsuran(A)",value = "0"),
               textInput("dua", "Suku Bunga(i)",value="0.5"),
               textInput("tiga","Periode(n)",value="1"),
               actionButton("hitungad","Kerjakan")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Present Value",
                          tags$h5("Nilai Present Value:"),
                          verbatimTextOutput("PVAD"),
                          tabsetPanel(
                   tabPanel("Grafik",
                            tags$h5("Grafik Present Value Anuitas Dimuka",style="text-align:Center"), 
                            plotPVAD<-plotOutput("plotpv_ad")))),
                 
                   tabPanel("Future Value",
                            tags$h5("Nilai Future Value:"),
                            verbatimTextOutput("FVAD"),
                            tabsetPanel(
                              tabPanel("Grafik",
                                       tags$h5("Grafik Present Value Anuitas Dimuka",style="text-align:Center"), 
                                       plotFVAD<-plotOutput("plotfv_ad"))
           )))))),
#Anuitas Ditunda
tabPanel("Anuitas Ditunda",
         h3("ANUITAS DITUNDA"),
         sidebarLayout(
           sidebarPanel(
             textInput("empat","Angsuran(A)",value = "0"),
             textInput("lima", "Suku Bunga(i)",value="0.5"),
             textInput("enam","Periode(n)",value="1"),
             textInput("tujuh","Periode Penundaan(m)",value="0"),
             actionButton("hitungat","Kerjakan")),
           mainPanel(
             tabsetPanel(
             tabPanel("Present Value",
             tags$h5("Nilai Present Value:"),
             verbatimTextOutput("PVAt"),
             tabsetPanel(
               tabPanel("Grafik",
                        tags$h5("Grafik Present Value Anuitas Ditunda",style="text-align:Center"),
             plotPVAT<-plotOutput("plotpv_at")))),
                      
             tabPanel("Future Value",
                     tags$h5("Nilai Future Value:"),
                      verbatimTextOutput("FVAt"),
                     tabsetPanel(
                       tabPanel("Grafik",
             tags$h5("Grafik Present Value Anuitas Ditunda",style="text-align:Center"), 
             plotFVAT<-plotOutput("plotfv_at"))
         )))))),

#Anuitas bertumbuh nilai present value saja karena pada anuitas bertumbuh tidak dapat mencari nilai FV
tabPanel("Anuitas Bertumbuh",
         h3("ANUITAS BERTUMBUH (i > g)"),
         sidebarLayout(
           sidebarPanel(
             textInput("o","Angsuran(A)",value = "0"),
             textInput("p", "Suku Bunga(i)",value="0.5"),
             textInput("r","Periode(n)",value="1"),
             textInput("s","Tingkat Pertumbuhan(g)",value="0"),
             actionButton("hitungtu","Kerjakan")),
           mainPanel(
             tags$h5("Nilai Present Value:"),
             verbatimTextOutput("PVtum"),
             br(),
             tags$h5("Grafik Present Value Anuitas Bertumbuh",style="text-align:Center"), 
             plotPVtum<-plotOutput("plotpv_tum"))
         )),
#Anuitas Variabel
tabPanel("Anuitas Variabel",
         h3("ANUITAS VARIABEL"),
         sidebarLayout(
           sidebarPanel(
             textInput("b","Angsuran(A)",value = "0"),
             textInput("c", "Suku Bunga(i)",value="0.5"),
             textInput("d","Periode(n)",value="1"),
             textInput("e","Nilai Pertumbuhan dari Angsuran (Q)",value="0"),
             actionButton("hitungva","Kerjakan")),
           mainPanel(
             tags$h5("Nilai Present Value:"),
             verbatimTextOutput("PVva"),
             br(),
             tags$h5("Grafik Present Value Anuitas Variabel", Style="text-align:center"),
             plotPVva<-plotOutput("plotpv_va"))
         )),
  )
))
server<- function(input, output, session) {
#Anuitas biasa
  observeEvent(input$hitung,{
    A <- as.numeric(input$a)
    i <- as.numeric(input$i)
    n <- as.numeric(input$n)
    PV_ab<- A*(1-(1+i)^(-n))/i
    FV_ab<-A*(((1+i)^n)-1)/i
    output$PVAB <- renderPrint(PV_ab)
    output$FVAB <- renderPrint(FV_ab)})
  output$plotpv_ab<-renderPlot({input$hitung
    A <- as.numeric(input$a)
    i <- as.numeric(input$i)
    n <- as.numeric(input$n)
    x<-1:n
    PV_Biasa=A*(1-(1+i)^(-x))/i
    plot(PV_Biasa)})
  output$tabab<-renderTable(bordered = TRUE, rownames = TRUE,{input$tabab
    A <- as.numeric(input$a)
    i <- as.numeric(input$i)
    n <- as.numeric(input$n)
    x<-n:1
    Angsuran_Biasa<-rep(A,n)
    p<-rep(i,n)
    PV_Biasa=A*(1-(1+i)^(-x))/i
    Bunga_Biasa<-(p*PV_Biasa)
    data.frame(Angsuran_Biasa,Bunga_Biasa,PV_Biasa)})
  output$plotfv_ab<-renderPlot({input$hitung
    A <- as.numeric(input$a)
    i <- as.numeric(input$i)
    n <- as.numeric(input$n)
    x<-1:n
    FV_Biasa=A*(((1+i)^x)-1)/i
    plot(FV_Biasa)})
  
 
#Anuitas dimuka
  observeEvent(input$hitungad,{
    A <- as.numeric(input$satu)
    i <- as.numeric(input$dua)
    n <- as.numeric(input$tiga)
    PV_ad<- A*(((1-(1+i)^(-n+1))/i)+1)
    FV_ad<-((((1+i)^n)-1)/i)*(A*(1+i))
    output$PVAD <- renderPrint(PV_ad)
    output$FVAD <- renderPrint(FV_ad)})
  output$plotpv_ad<-renderPlot({input$hitungad
    A <- as.numeric(input$satu)
    i <- as.numeric(input$dua)
    n <- as.numeric(input$tiga)
    v<-0:(n-1)
    PV_Dimuka=A*(((1-(1+i)^(-v+1))/i)+1)
    plot(PV_Dimuka)})

  output$plotfv_ad<-renderPlot({input$hitungad
    A <- as.numeric(input$satu)
    i <- as.numeric(input$dua)
    n <- as.numeric(input$tiga)
    v<-0:(n-1)
    FV_Dimuka=((((1+i)^v)-1)/i)*(A*(1+i))
    plot(FV_Dimuka)})
  
#Anuitas ditunda
  observeEvent(input$hitungat,{
    A <- as.numeric(input$empat)
    i <- as.numeric(input$lima)
    n <- as.numeric(input$enam)
    m <- as.numeric(input$tujuh)
    PV_at<- (((1-((1+i)^(-n)))/i)*A)/((1+i)^(m-1))
    FV_at<-A*(((1+i)^n)-1)/i
    output$PVAt <- renderPrint(PV_at)
    output$FVAt <- renderPrint(FV_at)})
  output$plotpv_at<-renderPlot({input$hitungat
    A <- as.numeric(input$empat)
    i <- as.numeric(input$lima)
    n <- as.numeric(input$enam)
    m <- as.numeric(input$tujuh)
    z<-1:n
    PV_Ditunda=(((1-((1+i)^(-z)))/i)*A)/((1+i)^(m-1))
    plot(PV_Ditunda)})
  
  output$plotfv_at<-renderPlot({input$hitungat
    A <- as.numeric(input$empat)
    i <- as.numeric(input$lima)
    n <- as.numeric(input$enam)
    m <- as.numeric(input$tujuh)
    z<-1:n
    FV_Ditunda=A*(((1+i)^z)-1)/i
    plot(FV_Ditunda)})
  
#Anuitas Bertumbuh
  observeEvent(input$hitungtu,{
    A <- as.numeric(input$o)
    i <- as.numeric(input$p)
    n <- as.numeric(input$r)
    g <- as.numeric(input$s)
    PV_t<- A*((1-(((1+g)/(1+i))^n))/(i-g))
    output$PVtum <- renderPrint(PV_t)})
  output$plotpv_tum<-renderPlot({input$hitungantu
    A <- as.numeric(input$o)
    i <- as.numeric(input$p)
    n <- as.numeric(input$r)
    g <- as.numeric(input$s)
    y<-1:n
    PV_Bertumbuh=A*((1-(((1+g)/(1+i))^y))/(i-g))
    plot(PV_Bertumbuh)})
  
#Anuitas Variabel
  observeEvent(input$hitungva,{
    A <- as.numeric(input$b)
    i <- as.numeric(input$c)
    n <- as.numeric(input$d)
    Q <- as.numeric(input$e)
    PV_V<- (A*((1-((1+i)^(-n)))/i))+(Q*(((1-((1+i)^(-n)))/i)-(n*((1+i)^(-n))))/i)
    output$PVva <- renderPrint(PV_V)})
  output$plotpv_va<-renderPlot({input$hitunganva
    A <- as.numeric(input$b)
    i <- as.numeric(input$c)
    n <- as.numeric(input$d)
    Q <- as.numeric(input$e)
    u<- 1:n
    PV_variabel=(A*((1-((1+i)^(-u)))/i))+(Q*(((1-((1+i)^(-u)))/i)-(u*((1+i)^(-u))))/i)
    plot(PV_variabel)})
  
}
shinyApp(ui=ui, server=server)