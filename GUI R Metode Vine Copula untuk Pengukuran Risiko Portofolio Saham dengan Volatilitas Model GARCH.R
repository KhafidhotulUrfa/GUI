library(shiny)
library(shinydashboard)
library(quantmod)
library(forecast)
library(psych)
library(tseries)
library(lmtest)
library(rugarch)
library(FinTS)
library(Kendall)
library(fitdistrplus)
library(CDVineCopulaConditional)
library(VineCopula)
library(copula)
library(CDVine)
library(quadprog)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = h3("Pengukuran Risiko Portofolio Saham Model ARIMA-GARCH Vine Copula", style = "font-family:'arial',cursive;color: bold;text-align:center"), titleWidth = 1500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input", icon = icon("database")),
      menuItem("ARIMA", tabName = "ARIMA", icon = icon("chart-line")),
      menuItem("GARCH", tabName = "ARIMA-GARCH", icon = icon("chart-line")),
      menuItem("Vine Copula", tabName = "Vine-Copula", icon = icon("sitemap")),
      menuItem("VaR-CVaR", tabName = "VaR-CVaR", icon = icon("square-poll-vertical"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input",
              box(title="Input Data",status = "primary", solidHeader = TRUE,
                  textInput("symbol1", "Stock Symbol 1", ""),
                  textInput("symbol2", "Stock Symbol 2", ""),
                  textInput("symbol3", "Stock Symbol 3", ""),
                  dateInput("start_date", "Start Date", as.Date("2019-12-01")),
                  dateInput("end_date", "End Date", as.Date("2023-12-30")),
                  actionButton("submit", "Submit")),
              tabsetPanel(
                tabPanel("Grafik",
                         tags$h5("Grafik Saham", style = "text-align:center"),
                         plotOutput("plotsaham1"),
                         plotOutput("plotsaham2"),
                         plotOutput("plotsaham3")),
                tabPanel("Summary",
                         tags$h5("Summary Saham", style = "text-align:center"),
                         verbatimTextOutput("SummarySaham1"),
                         verbatimTextOutput("SummarySaham2"),
                         verbatimTextOutput("SummarySaham3")),
                tabPanel("Stasioneritas",
                         tags$h5("Stasioneritas Saham", style = "text-align:center"),
                         tabsetPanel(
                           tabPanel("Stasioner dalam Varian",
                                    verbatimTextOutput("Stasioner1"),
                                    verbatimTextOutput("Stasioner2"),
                                    verbatimTextOutput("Stasioner3")),
                           tabPanel("Stasioner dalam Mean",
                                    verbatimTextOutput("Stas1"),
                                    verbatimTextOutput("Stas2"),
                                    verbatimTextOutput("Stas3"))
                         )
                )
              )
      ),
      tabItem(tabName = "ARIMA",
              tabsetPanel(
                tabPanel("Model ARIMA",
                         box(title = "ARIMA Saham 1", status = "primary", solidHeader = TRUE,
                             verbatimTextOutput("ARIMA_1")),
                         box(title = "ARIMA Saham 2", status = "primary", solidHeader = TRUE,
                             verbatimTextOutput("ARIMA_2")),
                         box(title = "ARIMA Saham 3", status = "primary", solidHeader = TRUE,
                             verbatimTextOutput("ARIMA_3"))),
                tabPanel("Uji Asumsi",
                         tabsetPanel(
                           tabPanel("Uji Autokorelasi",
                                    verbatimTextOutput("au1"),
                                    verbatimTextOutput("au2"),
                                    verbatimTextOutput("au3")),
                           tabPanel("Uji Heteroskedastisitas",
                                    verbatimTextOutput("het1"),
                                    verbatimTextOutput("het2"),
                                    verbatimTextOutput("het3"))
                         )
                )
              )
              
      ),
      tabItem(tabName = "ARIMA-GARCH",
              tabsetPanel(
                tabPanel("Model GARCH",
                         box(title = "ARIMA Saham 1", 
                             numericInput("p", "p", value = 0, min = 0),
                             numericInput("q", "q", value = 0, min = 0),
                             numericInput("d", "d", value = 0, min = 0),
                             verbatimTextOutput("GARCH_1")),
                         box(title = "ARIMA Saham 2", 
                             numericInput("a", "p", value = 0, min = 0),
                             numericInput("b", "q", value = 0, min = 0),
                             numericInput("k", "d", value = 0, min = 0),
                             verbatimTextOutput("GARCH_2")),
                         box(title = "ARIMA Saham 3", 
                             numericInput("e", "p", value = 0, min = 0),
                             numericInput("f", "q", value = 0, min = 0),
                             numericInput("g", "d", value = 0, min = 0),
                             verbatimTextOutput("GARCH_3")),
                         actionButton("submit1", "Submit")),
                tabPanel("Pengujian Distribusi",
                         tags$h5("AIC Distribusi residual", style = "text-align:center"),
                         verbatimTextOutput("dis"))
              )
      ),
      tabItem(tabName = "Vine-Copula",
              tabsetPanel(
                tabPanel("Copula",
                         tags$h5("AIC Vine Copula", style = "text-align:center"),
                         verbatimTextOutput("Cop6")),
                tabPanel("Copula Terbaik",
                         box(title = "Vine copula terbaik", 
                             textInput("copu", "copula terbaik", ""),
                             textInput("type", "type", ""), 
                             actionButton("submit2", "Submit")),
                         tabsetPanel(
                           tabPanel("Pamater",
                                    verbatimTextOutput("parameter")
                           ),
                           tabPanel("Grafik Vine Copula",
                                    tags$h5("Plot Tree Vine Copula", style = "text-align:center"),
                                    plotOutput("plottree1"),
                                    plotOutput("plottree2"))
                         )
                )
              )
      ),
      tabItem(tabName = "VaR-CVaR",
              box(title = "VaR dan CVaR", status = "primary", solidHeader = TRUE,
                  numericInput("confi_level", "Tingkat Kepercayaan (%)", value = 0.95, min = 0.90, max = 0.99),
                  radioButtons("method", "Pilih Metode Pembobotan:",
                               choices = list("SEM-Variance" = "sem_variance",
                                              "MVEP" = "mvep"),
                               selected = "sem_variance"),
                  actionButton("calculate", "Calculate")),
              
              tabsetPanel(
                tabPanel("Bobot",
                         tags$h5("Pembobotan Saham", style = "text-align:center"),
                         verbatimTextOutput("bot")),
                tabPanel("Risiko",
                         tags$h5("VaR dan CVaR", style = "text-align:center"),
                         verbatimTextOutput("Var_CvaR")),
                tabPanel("Validitas",
                         tags$h5("Validitas VaR dan CVaR", style = "text-align:center"),
                         verbatimTextOutput("Val_VarCvar"))
              )
      )
    )
  )
)

server <- function(input, output) {
  LJung.Box<- function(x, lag=15)
  {
    out1=acf(x,lag.max = lag, plot=F, na.action  =na.pass)
    acfout=out1$acf
    out2=pacf(x,lag.max = lag,plot=F,na.action = na.pass)
    pacfout=out2$pacf
    pacfout[1]=1
    pacfout=c(pacfout,out2$acf)
    temp1=numeric(lag+1)
    temp2=numeric(lag+1)
    for (i in 1 :lag)
    {
      temp1[i+1]=Box.test(x,lag=i,type="Ljung")$statistic
      temp2[i+1]=Box.test(x,lag=i,type="Ljung")$p.value
    }
    result=cbind(ACF=acfout, PACF=pacfout,"Q-Stats"=temp1, "P-Value"=temp2)
    rownames(result)=0:lag
    print(result)
  }
  observeEvent(input$submit, {
    Stock_Symbol_1 <- as.character(input$symbol1)
    Stock_Symbol_2 <- as.character(input$symbol2)
    Stock_Symbol_3 <- as.character(input$symbol3)
    start_date <- as.Date(input$start_date)
    end_date <- as.Date(input$end_date)
    
    # Fetching stock data
    stock_data_1 <- tryCatch({
      getSymbols(Stock_Symbol_1, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    stock_data_2 <- tryCatch({
      getSymbols(Stock_Symbol_2, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    stock_data_3 <- tryCatch({
      getSymbols(Stock_Symbol_3, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    # Ensure stock data exists
    if (!is.null(stock_data_1)) {
      returns_1 <- diff(log(Cl(stock_data_1)))
      returns_1 <- na.omit(returns_1)
    } else {
      print("Failed to fetch data for Stock Symbol 1")
    }
    
    if (!is.null(stock_data_2)) {
      returns_2 <- diff(log(Cl(stock_data_2)))
      returns_2 <- na.omit(returns_2)
    } else {
      print("Failed to fetch data for Stock Symbol 2")
    }
    
    if (!is.null(stock_data_3)) {
      returns_3 <- diff(log(Cl(stock_data_3)))
      returns_3 <- na.omit(returns_3)
    } else {
      print("Failed to fetch data for Stock Symbol 3")
    }
    
    # ARIMA modeling
    if (exists("returns_1")) {
      lambda <- round(BoxCox.lambda(returns_1))
      initial_lambda <- lambda
      transformed <- FALSE
      
      while (lambda != 1) {
        transformed <- TRUE
        returns_1 <- if(lambda == 0) log(returns_1) else (returns_1^lambda - 1) / lambda
        lambda <- round(BoxCox.lambda(returns_1))
      }
      
      output$Stasioner1 <- renderPrint({
        if (transformed) {
          cat(Stock_Symbol_1, "Initial BoxCox Lambda:", initial_lambda, "\n")
          cat("Transformed BoxCox Lambda:", lambda)
        } else {
          cat(Stock_Symbol_1, "BoxCox Lambda:", lambda)
        }
      })
      returns_1.ts<-ts(returns_1)
      adf_result <- adf.test(returns_1.ts)
      p_value <- adf_result$p.value
      diff_count <- 0
      
      while (p_value > 0.05) {
        returns_1.ts <- diff(returns_1.ts)
        diff_count <- diff_count + 1
        p_value <- adf.test(returns_1.ts)$p.value
      }
      
      output$Stas1 <- renderPrint({
        cat(Stock_Symbol_1, "ADF Test Result:\n")
        if (diff_count > 0) {
          cat("dilakukan differencing :", diff_count, "\n")
        } else {
          cat("Data sudah stasioner dengan p-value <", p_value, "\n")
        }
        print(adf_result)
      })
      autoarima_1 <- auto.arima(returns_1, stationary = TRUE, trace = TRUE)
      sig_1 <- coeftest(autoarima_1)
      output$ARIMA_1 <- renderPrint({
        cat(Stock_Symbol_1, "ARIMA Model :\n")
        print(sig_1)
      })
    }
    if (exists("returns_2")) {
      lambda <- round(BoxCox.lambda(returns_2))
      initial_lambda <- lambda
      transformed <- FALSE
      
      while (lambda != 1) {
        transformed <- TRUE
        returns_2 <- if(lambda == 0) log(returns_2) else (returns_2^lambda - 1) / lambda
        lambda <- round(BoxCox.lambda(returns_2))
      }
      
      output$Stasioner2 <- renderPrint({
        if (transformed) {
          cat(Stock_Symbol_2, "Initial BoxCox Lambda:", initial_lambda, "\n")
          cat("Transformed BoxCox Lambda:", lambda)
        } else {
          cat(Stock_Symbol_2, "BoxCox Lambda:", lambda)
        }
      })
      returns_2.ts<-ts(returns_2)
      adf_result <- adf.test(returns_2.ts)
      p_value <- adf_result$p.value
      diff_count <- 0
      
      while (p_value > 0.05) {
        returns_2.ts <- diff(returns_2.ts)
        diff_count <- diff_count + 1
        p_value <- adf.test(returns_2.ts)$p.value
      }
      
      output$Stas2 <- renderPrint({
        cat(Stock_Symbol_2, "ADF Test Result:\n")
        if (diff_count > 0) {
          cat("dilakukan differencing :", diff_count, "\n")
        } else {
          cat("Data sudah stasioner dengan p-value <", p_value, "\n")
        }
        print(adf_result)
      })
      autoarima_2 <- auto.arima(returns_2, stationary = TRUE, trace = TRUE)
      sig_2 <- coeftest(autoarima_2)
      output$ARIMA_2 <- renderPrint({
        cat(Stock_Symbol_2, "ARIMA Model :\n")
        print(sig_2)
      })
    }
    if (exists("returns_3")) {
      lambda <- round(BoxCox.lambda(returns_3))
      initial_lambda <- lambda
      transformed <- FALSE
      
      while (lambda != 1) {
        transformed <- TRUE
        returns_3 <- if(lambda == 0) log(returns_3) else (returns_3^lambda - 1) / lambda
        lambda <- round(BoxCox.lambda(returns_3))
      }
      
      output$Stasioner3 <- renderPrint({
        if (transformed) {
          cat(Stock_Symbol_3, "Initial BoxCox Lambda:", initial_lambda, "\n")
          cat("Transformed BoxCox Lambda:", lambda)
        } else {
          cat(Stock_Symbol_3, "BoxCox Lambda:", lambda)
        }
      })
      returns_3.ts<-ts(returns_3)
      adf_result <- adf.test(returns_3.ts)
      p_value <- adf_result$p.value
      diff_count <- 0
      
      while (p_value > 0.05) {
        returns_3.ts <- diff(returns_3.ts)
        diff_count <- diff_count + 1
        p_value <- adf.test(returns_3.ts)$p.value
      }
      
      output$Stas3 <- renderPrint({
        cat(Stock_Symbol_3, "ADF Test Result:\n")
        if (diff_count > 0) {
          cat("dilakukan differencing :", diff_count, "\n")
        } else {
          cat("Data sudah stasioner dengan p-value <", p_value, "\n")
        }
        print(adf_result)
      })
      autoarima_3 <- auto.arima(returns_3, stationary = TRUE, trace = TRUE)
      sig_3 <- coeftest(autoarima_3)
      output$ARIMA_3 <- renderPrint({
        cat(Stock_Symbol_3, "ARIMA Model :\n")
        print(sig_3)})
    }
    
    # Membuat residu dari model ARIMA
    if (exists("autoarima_1")) {
      residuals_1 <- residuals(autoarima_1)
      output$het1 <- renderPrint({
        het<-ArchTest(residuals_1, lag = 36)
        cat(Stock_Symbol_1,"Uji Heteroskedastisitas: \n")
        print(het)})
      output$au1<-renderPrint({
        cat(Stock_Symbol_1,"Uji Autokorelasi : \n")
        LJ1<-LJung.Box(residuals_1)
        print(LJ1)})
    }
    if (exists("autoarima_2")) {
      residuals_2 <- residuals(autoarima_2)
      output$het2 <- renderPrint({
        het<-ArchTest(residuals_2, lag = 36)
        cat(Stock_Symbol_2,"Uji Heteroskedastisitas: \n")
        print(het)})
      output$au2<-renderPrint({
        cat(Stock_Symbol_2,"Uji Autokorelasi : \n")
        print(LJung.Box(residuals_2))})
    }
    if (exists("autoarima_3")) {
      residuals_3 <- residuals(autoarima_3)
      output$het3 <- renderPrint({
        het<- ArchTest(residuals_3, lag = 36)
        cat(Stock_Symbol_3,"Uji Heteroskedastisitas: \n")
        print(het)})
      output$au3<-renderPrint({
        cat(Stock_Symbol_3,"Uji Autokorelasi : \n")
        print(LJung.Box(residuals_3))})
    }
    
    # Output stock summary
    summary_stats <- function(data) {
      summary <- list(
        jumlah_data = length(data),
        rata_rata = mean(data),
        standar_deviasi = sd(data),
        nilai_min = min(data),
        nilai_max = max(data),
        Skewness=skew(data),
        Kurtosis=kurtosi(data)
      )
      return(summary)
    }
    
    output$SummarySaham1 <- renderPrint({
      cat("Summary Return Saham", Stock_Symbol_1,"\n")
      print(summary_stats(returns_1))})
    output$SummarySaham2 <- renderPrint({
      cat("Summary Return Saham", Stock_Symbol_2,"\n")
      print(summary_stats(returns_2))
    })
    output$SummarySaham3 <- renderPrint({
      cat("Summary Return Saham", Stock_Symbol_3,"\n")
      print(summary_stats(returns_3))
    })
    
    # Plotting stock data
    output$plotsaham1 <- renderPlot({
      if (!is.null(stock_data_1)) {
        plot(returns_1, main = Stock_Symbol_1)
      }
    })
    output$plotsaham2 <- renderPlot({
      if (!is.null(stock_data_2)) {
        plot(returns_2, main = Stock_Symbol_2)
      }
    })
    output$plotsaham3 <- renderPlot({
      if (!is.null(stock_data_3)) {
        plot(returns_3, main = Stock_Symbol_3)
      }
    })
    
    # GARCH modeling
    observeEvent(input$submit1, {
      fit_best_distribution <- function(data) {
        fit_norm <- fitdist(data = as.vector(data), "norm")
        fit_t <- fitdist(data = as.vector(data), "t", start = list(df = 2))
        fit_unif <- fitdist(data = as.vector(data), "unif")
        fit_cauchy <- fitdist(data = as.vector(data), "cauchy")
        fit_logis <- fitdist(data = as.vector(data), "logis")
        
        aic_values <- c(
          norm = fit_norm$aic,
          t = fit_t$aic,
          unif = fit_unif$aic,
          logis = fit_logis$aic,
          cauchy = fit_cauchy$aic
        )
        
        best_model <- names(which.min(aic_values))
        best_fit <- switch(best_model,
                           norm = fit_norm,
                           t = fit_t,
                           unif = fit_unif,
                           logis = fit_logis,
                           cauchy = fit_cauchy)
        
        return(list(model = best_model, fit = best_fit))
      }
      if (exists("returns_1")) {
        spec_1 <- ugarchspec(mean.model = list(armaOrder = c(input$p, input$q), include.mean = FALSE, arfima = F), variance.model = list(model = "sGARCH",garchOrder = c(1, 1)))
        garchfit_1 <- ugarchfit(spec = spec_1, data = returns_1, cond.dist = "MLE")
        output$GARCH_1 <- renderPrint({
          CO<-coeftest(garchfit_1)
          cat(Stock_Symbol_1, "GARCH Model :\n")
          print(CO)})
        Resid_1 <- residuals(garchfit_1)
        best_fit_1 <- fit_best_distribution(Resid_1)
        data_1 <- as.matrix(Resid_1[, 1], ncol = d, mode = "any")
       
      }
      
      if (exists("returns_2")) {
        spec_2 <- ugarchspec(mean.model = list(armaOrder = c(input$a, input$b), include.mean = FALSE, arfima = F), variance.model = list(garchOrder = c(1, 1)))
        garchfit_2 <- ugarchfit(spec = spec_2, data = returns_2, cond.dist = "MLE")
        output$GARCH_2 <- renderPrint({
          CO<-coeftest(garchfit_2)
          cat(Stock_Symbol_2, "GARCH Model :\n")
          print(CO)
        })
        Resid_2 <- residuals(garchfit_2)
        best_fit_2 <- fit_best_distribution(Resid_2)
        data_2 <- as.matrix(Resid_2[, 1], ncol = d, mode = "any")
      }
      
      if (exists("returns_3")) {
        spec_3 <- ugarchspec(mean.model = list(armaOrder = c(input$e, input$f), include.mean = FALSE, arfima = F), variance.model = list(garchOrder = c(1, 1)))
        garchfit_3 <- ugarchfit(spec = spec_3, data = returns_3, cond.dist = "MLE")
        output$GARCH_3 <- renderPrint({
          CO<-coeftest(garchfit_3)
          cat(Stock_Symbol_3, "GARCH Model :\n")
          print(CO)})
        Resid_3 <- residuals(garchfit_3)
        best_fit_3 <- fit_best_distribution(Resid_3)
        data_3 <- as.matrix(Resid_3[, 1], ncol = d, mode = "any")
        output$dis <- renderPrint({
          cat(Stock_Symbol_1, "Distribusi Model :\n")
          print(best_fit_1)
          cat("\n",Stock_Symbol_2, "Distribusi Model :\n")
          print(best_fit_2)
          cat("\n",Stock_Symbol_3, "Distribusi Model :\n")
          print(best_fit_3)
          })
      }
      
      residual_data <- data.frame(da1 = data_1, da2 = data_2, da3 = data_3)
      copula_data <- apply(residual_data[, 1:3], 2, rank) / (nrow(residual_data) + 1)
      d <- dim(copula_data)[2]
      fam1 <- rep(1, d * (d - 1) / 2)
      fam2 <- rep(2, d * (d - 1) / 2)
      fam3 <- rep(3, d * (d - 1) / 2)
      fam4 <- rep(4, d * (d - 1) / 2)
      fam5 <- rep(5, d * (d - 1) / 2)
      
      # sequential estimation 
      cvgaus <- CDVineSeqEst(copula_data, fam1, type = "CVine", method = "mle")
      cvt <- CDVineSeqEst(copula_data, fam2, type = "CVine", method = "mle")
      cvclay <- CDVineSeqEst(copula_data, fam3, type = "CVine", method = "mle")
      cvgum <- CDVineSeqEst(copula_data, fam4, type = "CVine", method = "mle")
      cvfrank <- CDVineSeqEst(copula_data, fam5, type = "CVine", method = "mle")
      
      Dvgaus <- CDVineSeqEst(copula_data, fam1, type = "DVine", method = "mle")
      Dvt <- CDVineSeqEst(copula_data, fam2, type = "DVine", method = "mle")
      Dvclay <- CDVineSeqEst(copula_data, fam3, type = "DVine", method = "mle")
      Dvgum <- CDVineSeqEst(copula_data, fam4, type = "DVine", method = "mle")
      Dvfrank <- CDVineSeqEst(copula_data, fam5, type = "DVine", method = "mle")
      
      # Estimasi C-Vine
      AICgaus <- CDVineAIC(copula_data, fam1, par = cvgaus$par, type = "CVine")
      AICt <- CDVineAIC(copula_data, fam2, par = cvt$par, par2 = cvt$par2, type = "CVine")
      AICclay <- CDVineAIC(copula_data, fam3, par = cvclay$par, type = "CVine")
      AICgum <- CDVineAIC(copula_data, fam4, par = cvgum$par, type = "CVine")
      AICfrank <- CDVineAIC(copula_data, fam5, par = cvfrank$par, type = "CVine")
      
      # Estimasi D-vine
      AICDgaus <- CDVineAIC(copula_data, fam1, par = Dvgaus$par, type = "DVine")
      AICDt <- CDVineAIC(copula_data, fam2, par = Dvt$par, par2 = Dvt$par2, type = "DVine")
      AICDclay <- CDVineAIC(copula_data, fam3, par = Dvclay$par, type = "DVine")
      AICDgum <- CDVineAIC(copula_data, fam4, par = Dvgum$par, type = "DVine")
      AICDfrank <- CDVineAIC(copula_data, fam5, par = Dvfrank$par, type = "DVine")
      AIC <- c("Cvine Gaussian" = AICgaus$AIC, "CVine t" = AICt$AIC, "Cvine Clayton" = AICclay$AIC, 
               "CVine Gumbel" = AICgum$AIC, "CVine Frank" = AICfrank$AIC,
               "Dvine Gaussian" = AICDgaus$AIC, "DVine t" = AICDt$AIC, 
               "Dvine Clayton" = AICDclay$AIC, "DVine Gumbel" = AICDgum$AIC, 
               "DVine Frank" = AICDfrank$AIC)
      AIC_df <- data.frame(Model = names(AIC), AIC = as.numeric(AIC), stringsAsFactors = FALSE)
      
      # Cari nilai AIC terkecil dan model terkait
      AICter <- min(AIC_df$AIC)
      aic <- AIC_df$Model[which.min(AIC_df$AIC)]
      
      # Cetak hasilnya menggunakan output$Cop6 di Shiny
      output$Cop6 <- renderPrint({
        cat("Nilai AIC Model Vine adalah:\n")
        print(AIC_df)
        cat("\nModel dengan nilai AIC terkecil adalah", aic, "dengan nilai AIC sebesar", AICter, "\n")
      })
      
      observeEvent(input$submit2, {
        t<-as.character(input$copu)
        type=as.character(input$type)
        if (type == "CVine") {
          if (t == "t") {
            par1 <- cvt$par
            par2 <- cvt$par2
            fam <- fam2
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(cvt)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- ellipCopula(t, param = par1[3], df = par2[3], dim = 3, dispstr = "ex")
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "normal") {
            par1 <- cvgaus$par
            par2<-cvgaus$par2
            fam <- fam1
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(cvgaus)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- ellipCopula(t, param = par1[3], df = par2[3], dim = 3, dispstr = "ex")
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "clayton") {
            par1 <-cvclay$par
            par2<-cvclay$par2
            fam <- fam3
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(cvclay)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "gumbel") {
            par1 <-cvgum$par
            par2<-cvgum$par2
            fam<-fam4
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(cvgum)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "frank") {
            par1 <- cvfrank$par
            par2<-cvfrank$par2
            fam=fam5
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(cvfrank)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          }
        } else if (type == "DVine") {
          if (t == "t") {
            par1 <- Dvt$par
            par2 <- Dvt$par2
            fam <- fam2
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(Dvt)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- ellipCopula(t, param = par1[3], df = par2[3], dim = 3, dispstr = "ex")
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "normal") {
            par1 <- Dvgaus$par
            par2<-Dvgaus$par2
            fam <- fam1
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(Dvgaus)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- ellipCopula(t, param = par1[3], df = par2[3], dim = 3, dispstr = "ex")
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "clayton") {
            par1 <-Dvclay$par
            par2<-Dvclay$par2
            fam <- fam3
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(Dvclay)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "gumbel") {
            par1 <-Dvgum$par
            par2<-Dvgum$par2
            fam<-fam4
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(Dvgum)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          } else if (t == "frank") {
            par1 <- Dvfrank$par
            par2<-Dvfrank$par2
            fam=fam5
            output$parameter<-renderPrint({
              cat("Parmeter Model",aic, "adalah \n")
              print(Dvfrank)
            })
            output$plottree1<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=1,edge.labels="par"))
            output$plottree2<- renderPlot(
              CDVineTreePlot(copula_data,fam,par1,par2,type=type,method="mle",tree=2,edge.labels="par"))
            cop <- archmCopula(t, param = par1[3], dim = 3)
            MVD <- mvdc(cop, margins = c("logis", "logis", "logis"),
                        paramMargins = list(list(location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2]),
                                            list(location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2]),
                                            list(location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])))
          }
        }
        
        
        observeEvent(input$calculate,{
          repeat {
            # Step 1: Generate new simulated returns
            n=length(returns_1)
            R2 = rMvdc(n, MVD)
            colnames(R2) <- c("x1", "x2", "x3")
            x1 = sort(subset(R2, select = "x1"))
            sim1_1 <- qlogis(runif(x1, 0, 1), location = best_fit_1$fit$estimate[1], scale = best_fit_1$fit$estimate[2])
            x2 = sort(subset(R2, select = "x2"))
            sim1_2 <- qlogis(runif(x2, 0, 1), location = best_fit_2$fit$estimate[1], scale = best_fit_2$fit$estimate[2])
            x3 = sort(subset(R2, select = "x3"))
            sim1_3 <- qlogis(runif(x3, 0, 1), location = best_fit_3$fit$estimate[1], scale = best_fit_3$fit$estimate[2])  
            
            # Step 2: Calculate portfolio weights
            symbol <- "^JKSE"
            getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
            str(JKSE)
            JKSE
            
            returnsJKSE <- diff(log(Cl(JKSE)))
            returnsJKSE <- na.omit(returnsJKSE)
            returnsJKSE
            
            returns <- cbind(sim1_1, sim1_2, sim1_3)
            benchmark_returns <- returnsJKSE
            x = matrix(c(sim1_1, sim1_2, sim1_3), ncol = 3)
            
            if (input$method == "sem_variance") {
              mean_semivariance_portfolio <- function(returns, benchmark) {
                # Ensure that the returns and benchmark have the same number of rows
                if (nrow(returns) != length(benchmark)) {
                  stop("The number of rows in returns and the length of benchmark must be the same.")
                }
                excess_returns <- sweep(returns, 1, benchmark)
                semi_var <- apply(excess_returns, 2, function(x) {
                  neg_returns <- x[x < 0]
                  if (length(neg_returns) > 1) {
                    return(sum(neg_returns^2) / (length(neg_returns) - 1))
                  } else {
                    return(0)
                  }
                })
                mean_returns <- colMeans(returns)
                Dmat <- 2 * diag(semi_var)
                dvec <- rep(0, ncol(returns))
                Amat <- cbind(rep(1, ncol(returns)), diag(ncol(returns)))
                bvec <- c(1, rep(0, ncol(returns)))
                meq <- 1
                # Solve the quadratic programming problem
                solution <- solve.QP(Dmat, dvec, Amat, bvec, meq)
                weights <- solution$solution
                return(weights)
              }
              
              bobot <- mean_semivariance_portfolio(returns, benchmark_returns)
              output$bot <- renderPrint({
                cat("Bobot untuk Portofolio saham berdasarkan Semivariance adalah:\n")
                print(bobot)
              })
            }
            else if(input$method=="mvep"){
              MVEP <- function(x) {
                x <- as.matrix(x)
                n.var <- dim(x)[2]
                sigma <- var(x)
                invers.sigma <- solve(sigma)
                i <- matrix(1, nrow = n.var, ncol = 1)
                it <- t(i)
                w <- as.vector(invers.sigma %*% i) / (as.vector(it %*% invers.sigma %*% i))
                return(w)}
              bobot = MVEP(x)
              output$bot <- renderPrint({
                cat("Bobot untuk Portofolio saham adalah \n")
                print(bobot)
              })
            }
            
            # Step 3: Calculate VaR and CVaR
            tingkat_kepercayaan = as.numeric(input$confi_level)
            alpha = 1 - tingkat_kepercayaan
            
            VaR_CVaR <- function(data_porto, w1, w2, w3, returns1, returns2, returns3, alpha) {
              data = as.matrix(data_porto)
              alpha = alpha
              tingkat_kepercayaan = 1 - alpha
              h = 1000
              
              VaR = rep(0, h)
              data_CVaR = rep(0, h)
              CVaR = rep(0, h)
              for (i in 1:h) {
                return_porto = (w1 * returns1) + (w2 * returns2) + (w3 * returns3)
                VaR[i] = quantile(return_porto, alpha)
                data_CVaR[i] = return_porto[return_porto < VaR[i]]
                CVaR[i] = mean(data_CVaR[i])
              }
              VaR = mean(VaR)
              CVaR = mean(CVaR)
              return(list(VaR = VaR, CVaR = CVaR))
            }
            
            return_porto = (bobot[1] * sim1_1) + (bobot[2] * sim1_2) + (bobot[3] * sim1_3)
            result <- VaR_CVaR(x, bobot[1], bobot[2], bobot[3], sim1_1, sim1_2, sim1_3, alpha)
            
            # Step 4: Backtesting
            data=as.matrix(return_porto)
            alpha=0.05
            k=250
            c=length(return_porto)
            window_size=c-k
            calc_VaR <- function(returns, alpha) {
              quantile(returns, probs = alpha)
            }
            # Membagi data menjadi jendela uji dan jendela prediksi
            VaR_values <- rep(NA, length(return_porto) - window_size)
            CVaR_values <- rep(NA, length(return_porto) - window_size)
            for (i in 1:(length(return_porto) - window_size)) {
              window_returns <- return_porto[i:(i + window_size - 1)]
              VaR_values[i] <- calc_VaR(window_returns,alpha)
              CVaR_values[i]=window_returns[window_returns<VaR_values[i]]
              CVaR[i]=mean(CVaR_values[i])
            }
            actual_returns <- return_porto[(window_size + 1):length(return_porto)]
            
            backtesting <- function(a, b, p) {
              c <- length(a)
              return_matrix <- matrix(a, nrow = c, ncol = 1)
              var_matrix <- matrix(b, nrow = c, ncol = 1)
              backtest <- matrix(nrow = c, ncol = 3)
              
              for (t in 1:c) {
                backtest[t, 1] <- return_matrix[t, 1]
                backtest[t, 2] <- var_matrix[t, 1]
                if (backtest[t, 1] <= backtest[t, 2]) {
                  backtest[t, 3] <- 1
                } else {
                  backtest[t, 3] <- 0
                }
              }
              
              v1 <- sum(backtest[, 3])
              lr <- -2 * log((1 - p)^(c - v1) * p^(v1)) + 2 * log((1 - (v1 / c))^(c - v1) * (v1 / c)^(v1))
              cr <- qchisq((1 - p), 1)
              
              # Calculate p-value using chi-square distribution
              p_value <- pchisq(lr, df = 1, lower.tail = FALSE)
              
              # Return results
              return(list(
                n=c,
                Jumlah_Kesalahan = v1,
                statistic = lr,
                critical_value = cr,
                alpha = alpha,
                p_value = p_value,
                hasil = ifelse(lr <= cr, "H0 diterima", "H0 ditolak")
              ))
            }
            
            backtesting_Var <- backtesting(matrix(actual_returns), VaR_values, alpha)
            backtesting_Cvar <- backtesting(matrix(actual_returns), CVaR_values, alpha)
            
            # Step 5: Check backtesting results
            if (backtesting_Var$hasil == "H0 diterima" && backtesting_Cvar$hasil == "H0 diterima") {
              output$Var_CvaR <- renderPrint({
                cat("Nilai VaR dan CVaR pada tingkat kepercayaan", alpha, "adalah \n")
                print(result)
              })
              
              output$Val_VarCvar <- renderPrint({
                cat("Hasil Backtesting untuk VaR adalah \n")
                print(backtesting_Var)
                cat("\nHasil Backtesting untuk CVaR adalah \n")
                print(backtesting_Cvar)
                cat("\n")
              })
              break
            }
          }
        })
        
      })
    })
  })
}
shinyApp(ui = ui, server = server)

