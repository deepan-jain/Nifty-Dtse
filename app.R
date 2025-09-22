# ==============================================================================
#
# Nifty 50 Stock Analysis Shiny App
#
# Description:
# This application fetches and displays historical stock price data for
# Nifty 50 companies and compares their performance against the Nifty 50 index.
# Users can select a company and a date range to view the corresponding
# stock chart, summary statistics, a data table, and a performance comparison chart.
#
# Author: Gemini
# Date: September 22, 2025
#
# ==============================================================================


# 1. PACKAGE MANAGEMENT
# ------------------------------------------------------------------------------
# List of required packages
required_packages <- c("shiny", "quantmod", "DT")

# Check if packages are installed and install them if they are not
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(shiny)
library(quantmod)
library(DT)


# 2. NIFTY 50 STOCK SYMBOLS
# ------------------------------------------------------------------------------
# A named vector of Nifty 50 companies and their corresponding ticker symbols
# for Yahoo Finance. The ".NS" suffix is required for Indian stocks.
nifty50_tickers <- c(
  "Adani Enterprises" = "ADANIENT.NS",
  "Adani Ports & SEZ" = "ADANIPORTS.NS",
  "Apollo Hospitals Enterprise" = "APOLLOHOSP.NS",
  "Asian Paints" = "ASIANPAINT.NS",
  "Axis Bank" = "AXISBANK.NS",
  "Bajaj Auto" = "BAJAJ-AUTO.NS",
  "Bajaj Finance" = "BAJFINANCE.NS",
  "Bajaj Finserv" = "BAJFINSV.NS",
  "Bharti Airtel" = "BHARTIARTL.NS",
  "Bharat Petroleum Corp" = "BPCL.NS",
  "Britannia Industries" = "BRITANNIA.NS",
  "Cipla" = "CIPLA.NS",
  "Coal India" = "COALINDIA.NS",
  "Divi's Laboratories" = "DIVISLAB.NS",
  "Dr. Reddy's Laboratories" = "DRREDDY.NS",
  "Eicher Motors" = "EICHERMOT.NS",
  "Grasim Industries" = "GRASIM.NS",
  "HCL Technologies" = "HCLTECH.NS",
  "HDFC Bank" = "HDFCBANK.NS",
  "HDFC Life Insurance" = "HDFCLIFE.NS",
  "Hero MotoCorp" = "HEROMOTOCO.NS",
  "Hindalco Industries" = "HINDALCO.NS",
  "Hindustan Unilever" = "HINDUNILVR.NS",
  "ICICI Bank" = "ICICIBANK.NS",
  "ITC" = "ITC.NS",
  "IndusInd Bank" = "INDUSINDBK.NS",
  "Infosys" = "INFY.NS",
  "JSW Steel" = "JSWSTEEL.NS",
  "Kotak Mahindra Bank" = "KOTAKBANK.NS",
  "Larsen & Toubro" = "LT.NS",
  "Mahindra & Mahindra" = "M&M.NS",
  "Maruti Suzuki India" = "MARUTI.NS",
  "NTPC" = "NTPC.NS",
  "Nestle India" = "NESTLEIND.NS",
  "Oil & Natural Gas Corp" = "ONGC.NS",
  "Power Grid Corp of India" = "POWERGRID.NS",
  "Reliance Industries" = "RELIANCE.NS",
  "SBI Life Insurance" = "SBILIFE.NS",
  "State Bank of India" = "SBIN.NS",
  "Sun Pharmaceutical Industries" = "SUNPHARMA.NS",
  "Tata Consultancy Services" = "TCS.NS",
  "Tata Consumer Products" = "TATACONSUM.NS",
  "Tata Motors" = "TATAMOTORS.NS",
  "Tata Steel" = "TATASTEEL.NS",
  "Tech Mahindra" = "TECHM.NS",
  "Titan Company" = "TITAN.NS",
  "UltraTech Cement" = "ULTRACEMCO.NS",
  "UPL" = "UPL.NS",
  "Wipro" = "WIPRO.NS"
)


# 3. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  # Application Title
  titlePanel("Nifty 50 Stock Price Dashboard"),
  
  # Sidebar Layout
  sidebarLayout(
    # Sidebar with user input controls
    sidebarPanel(
      h4("User Inputs"),
      
      # Dropdown for selecting a company
      selectInput("company", 
                  "Select Company:", 
                  choices = nifty50_tickers,
                  selected = "RELIANCE.NS"),
      
      # Date range selector
      dateRangeInput("dates", 
                     "Select Date Range:",
                     start = Sys.Date() - 365, # Default start date is one year ago
                     end = Sys.Date()),        # Default end date is today
                     
      # Help text for context
      helpText("Select a company and a date range. Data is fetched from Yahoo Finance."),
      
      # Action button to trigger data fetching
      actionButton("getData", "Get Stock Data", class = "btn-primary")
    ),
    
    # Main panel for displaying output
    mainPanel(
      # Using a tabset for organized output
      tabsetPanel(
        type = "tabs",
        tabPanel("Historical Chart", 
                 h3(textOutput("chartTitle")),
                 plotOutput("stockPlot", height = "500px")
        ),
        tabPanel("Performance vs. Nifty 50",
                 h3(textOutput("perfTitle")),
                 plotOutput("perfPlot", height = "500px")
        ),
        tabPanel("Stock vs Nifty",
                 h3(textOutput("stockVsNiftyTitle")),
                 plotOutput("stockVsNiftyPlot", height = "500px")
        ),
        tabPanel("Summary Statistics", 
                 h3(textOutput("summaryTitle")),
                 verbatimTextOutput("summaryStats")
        ),
        tabPanel("Data Table", 
                 h3(textOutput("tableTitle")),
                 DT::dataTableOutput("stockTable")
        )
      )
    )
  )
)


# 4. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive expression to fetch company data when the action button is clicked
  stock_data <- eventReactive(input$getData, {
    withProgress({
      setProgress(message = "Fetching company data...", value = 0.5)
      tryCatch({
        data <- getSymbols(
          input$company, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE
        )
        setProgress(1)
        return(data)
      }, error = function(e) {
        showNotification(paste("Error fetching company data:", e$message), type = "error")
        setProgress(1)
        return(NULL)
      })
    })
  }, ignoreNULL = FALSE)

  # Reactive expression to fetch Nifty 50 index data
  nifty_index_data <- eventReactive(input$getData, {
    withProgress({
      setProgress(message = "Fetching Nifty 50 data...", value = 0.5)
      tryCatch({
        data <- getSymbols(
          "^NSEI", src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE
        )
        setProgress(1)
        return(data)
      }, error = function(e) {
        showNotification(paste("Error fetching Nifty 50 data:", e$message), type = "error")
        setProgress(1)
        return(NULL)
      })
    })
  }, ignoreNULL = FALSE)

  # Generate dynamic titles based on user selection
  company_name <- eventReactive(input$getData, {
      names(nifty50_tickers)[nifty50_tickers == input$company]
  }, ignoreNULL = FALSE)

  output$chartTitle <- renderText({ paste("Candlestick Chart for", company_name()) })
  output$summaryTitle <- renderText({ paste("Summary for", company_name()) })
  output$tableTitle <- renderText({ paste("Historical Data for", company_name()) })
  output$perfTitle <- renderText({ paste("Performance of", company_name(), "vs. Nifty 50 Index") })
  output$stockVsNiftyTitle <- renderText({ paste("Price Movement:", company_name(), "vs. Nifty 50") })

  # Render the stock plot
  output$stockPlot <- renderPlot({
    data <- stock_data()
    if (!is.null(data)) {
      chartSeries(data,
                  theme = chartTheme("white"), type = "candlesticks", name = company_name(),
                  TA = "addVo();addBBands();addRSI(n=14);addMACD()")
    }
  })

  # Render the performance comparison plot
  output$perfPlot <- renderPlot({
    company_data <- stock_data()
    index_data <- nifty_index_data()

    if (!is.null(company_data) && !is.null(index_data)) {
      # Use Adjusted Close for accurate performance comparison (accounts for dividends/splits)
      company_returns <- dailyReturn(Ad(company_data))
      index_returns <- dailyReturn(Ad(index_data))

      # Calculate the cumulative growth of 1 unit (e.g., ₹1)
      company_growth <- cumprod(1 + company_returns)
      index_growth <- cumprod(1 + index_returns)
      
      # Merge the two series for plotting
      comparison_data <- merge(company_growth, index_growth)
      colnames(comparison_data) <- c(company_name(), "Nifty 50")

      # Plot the performance comparison
      plot.xts(comparison_data, 
               main = "Growth of ₹1 Investment", 
               ylab = "Cumulative Returns",
               col = c("blue", "red"),
               legend.loc = "topleft")
    }
  })

  # Render the dual-axis price plot
  output$stockVsNiftyPlot <- renderPlot({
    company_data <- stock_data()
    index_data <- nifty_index_data()

    if (!is.null(company_data) && !is.null(index_data)) {
      stock_price <- Ad(company_data)
      nifty_price <- Ad(index_data)

      # Set plot margins to accommodate the second y-axis
      par(mar = c(5, 4, 4, 4) + 0.1)
      
      # Plot Nifty data on the left axis
      plot(index(nifty_price), nifty_price, type = 'l', col = 'red', 
           main = "Stock Price vs. Nifty Index", 
           xlab = "Date", ylab = "Nifty 50 Index Price")
           
      # Allow a new plot on top of the old one
      par(new = TRUE)
      
      # Plot stock data without axes or labels
      plot(index(stock_price), stock_price, type = 'l', col = 'blue', 
           xaxt = "n", yaxt = "n", xlab = "", ylab = "")
           
      # Add the right-side axis
      axis(4)
      mtext("Stock Price (₹)", side = 4, line = 3)
      
      # Add a legend
      legend("topleft", 
             legend = c("Nifty 50 (Left Axis)", paste(company_name(), "(Right Axis)")), 
             col = c("red", "blue"), 
             lty = 1, cex = 0.8)
    }
  })
  
  # Render the summary statistics
  output$summaryStats <- renderPrint({
    data <- stock_data()
    if (!is.null(data)) {
      cat("----- Summary -----\n")
      print(summary(data))
      cat("\n----- Latest Data -----\n")
      print(last(data))
    } else {
      "No data to display. Please click 'Get Stock Data'."
    }
  })
  
  # Render the data table
  output$stockTable <- DT::renderDataTable({
    data <- stock_data()
    if (!is.null(data)) {
      df <- data.frame(Date = index(data), coredata(data))
      colnames(df) <- gsub(paste0(input$company, "."), "", colnames(df))
      
      DT::datatable(df,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
}


# 5. RUN THE APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


# ==============================================================================
#
# --- README.md CONTENT ---
#
# You can copy the content below this line and paste it into a `README.md`
# file for your GitHub repository.
#
# ==============================================================================

# # Nifty 50 Stock Analysis Dashboard
# 
# An interactive R Shiny application for visualizing and analyzing the performance of companies listed on India's Nifty 50 index. This tool fetches real-time and historical data from Yahoo Finance and provides multiple ways to compare a stock's performance against the Nifty 50 benchmark.
# 
# ## Features
# 
# - **Interactive Company Selection:** Choose any of the Nifty 50 companies from a dropdown menu.
# - **Custom Date Range:** Select any historical date range for analysis.
# - **Candlestick Charts:** View detailed historical price movements with Open, High, Low, Close (OHLC) data, along with technical indicators like Volume, Bollinger Bands, RSI, and MACD.
# - **Performance Comparison:** Analyze the cumulative returns of a stock against the Nifty 50 index on a normalized percentage scale ("Growth of ₹1").
# - **Dual-Axis Price Chart:** Directly compare the absolute price trends of a stock (right axis) and the Nifty 50 index (left axis) on the same chart.
# - **Summary Statistics:** Get a quick statistical overview of the selected stock's performance.
# - **Data Table:** View and search the raw historical data in an interactive table.
# 
# ## How to Run
# 
# 1.  **Prerequisites:** You need to have R and RStudio installed on your system.
# 2.  **Download:** Clone or download the `app.R` file from this repository.
# 3.  **Run in RStudio:**
#     -   Open the `app.R` file in RStudio.
#     -   The script will automatically check for and install the required packages (`shiny`, `quantmod`, `DT`).
#     -   Click the "Run App" button that appears at the top of the editor.
# 
# ## Packages Used
# 
# This application is built using the following R packages:
# 
# -   `shiny`: For the web application framework.
# -   `quantmod`: For fetching financial data and creating charts.
# -   `DT`: For rendering interactive data tables.
# 
# ---

