library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(opendatascot)

# Get the data
opendatascot::ods_structure("scottish-child-payment-applications-and-payments")

# add date type
scp_data <- ods_dataset("scottish-child-payment-applications-and-payments") |>
  mutate(date_type = case_when(
    # Check for yyyy-mm format using regex
    grepl("^\\d{4}-\\d{2}$", refPeriod) ~ "monthly",
    # Check for yyyy/yyyy format using regex
    grepl("^\\d{4}/\\d{4}$", refPeriod) ~ "financial year",
    # Add a catch-all for any other formats
    TRUE ~ "other"
  ))|>
  mutate(value = as.numeric(value)
  )

# Create a lookup function for UK local authority codes
  scotland_las <- c(
    "S12000033" = "Aberdeen City",
    "S12000034" = "Aberdeenshire",
    "S12000041" = "Angus",
    "S12000035" = "Argyll and Bute",
    "S12000036" = "City of Edinburgh",
    "S12000005" = "Clackmannanshire",
    "S12000006" = "Dumfries and Galloway",
    "S12000042" = "Dundee City",
    "S12000008" = "East Ayrshire",
    "S12000045" = "East Dunbartonshire",
    "S12000010" = "East Lothian",
    "S12000011" = "East Renfrewshire",
    "S12000014" = "Falkirk",
    "S12000015" = "Fife",
    "S12000046" = "Glasgow City",
    "S12000017" = "Highland",
    "S12000018" = "Inverclyde",
    "S12000019" = "Midlothian",
    "S12000020" = "Moray",
    "S12000013" = "Na h-Eileanan Siar",
    "S12000021" = "North Ayrshire",
    "S12000044" = "North Lanarkshire",
    "S12000023" = "Orkney Islands",
    "S12000024" = "Perth and Kinross",
    "S12000038" = "Renfrewshire",
    "S12000026" = "Scottish Borders",
    "S12000027" = "Shetland Islands",
    "S12000028" = "South Ayrshire",
    "S12000029" = "South Lanarkshire",
    "S12000030" = "Stirling",
    "S12000039" = "West Dunbartonshire",
    "S12000040" = "West Lothian",
    "S12000047" = "Fife",
    "S12000048" = "Perth and Kinross",
    "S12000049" = "Glasgow City",
    "S12000050" = "North Lanarkshire",
    "S92000003" = "Scotland"
  )
  

# Function to convert codes to names
convert_la_code <- function(code) {
  la_lookup <- create_la_lookup()
  
  # If code exists in lookup, return the name, otherwise return NA
  if(code %in% names(scotland_las)) {
    return(scotland_las[code])
  } else {
    return(NA_character_)
  }
}

scp_data<- scp_data|>
  mutate(la = map(refArea, convert_la_code))
  

monthlyData<- scp_data|>
  filter(date_type == "monthly")|>
  mutate(refPeriod = as.Date(paste0(refPeriod, "-01")))

fyData<- scp_data|>
  filter(date_type == "financial year")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Scottish Child Payment Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select Metric",
                  choices = unique(scp_data$scottishChildPaymentIndicator),
                  selected = "Number of applications received"
      ),
      selectInput("view_type", "View Type",
                  choices = c("Monthly", "Financial Year"),
                  selected = "Monthly"
      ),
      selectInput("geography", "Area",
                  choices = unname(scotland_las),
                  selected = "Scotland"
      )
    ),
    
    mainPanel(
      card(
        card_header("Data Visualization"),
        plotOutput("main_plot")
      ),
      card(
        card_header("Summary Statistics"),
        textOutput("summary_text"),
        textOutput("n_records")
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    monthlyData |>
      filter(scottishChildPaymentIndicator == input$metric)|>
      filter(la == input$geography)
  })
  
  # Reactive filtered yearly
  filtered_yearly <- reactive({
    fyData|>
      filter(scottishChildPaymentIndicator == input$metric)|>
      filter(la == input$geography)
  })
  
  output$main_plot <- renderPlot({
    data <- filtered_data()
    dataY <- filtered_yearly()
    if (input$view_type == "Monthly") {
      ggplot(data, aes(x = refPeriod, y = value)) +
        geom_line(color = "#2c3e50", size = 1) +
        geom_point(color = "#2c3e50") +
        theme_minimal() +
        labs(
          title = input$metric,
          x = "Date",
          y = "Value"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      ggplot(dataY, aes(x = refPeriod, y = value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Financial Year Values",
             x = "Financial Year",
             y = "Value") +
        theme_minimal()
    }
  })
  
  output$summary_text <- renderText({
    data <- filtered_data()
    
    latest_value <- data %>%
      filter(refPeriod == max(refPeriod)) %>%
      pull(value)
    
    paste(
      "Latest value (", max(monthlyData$refPeriod), "): ", 
      format(latest_value, big.mark = ",")
    )
  })
  output$n_records <- renderText({
    data <- filtered_data()
    paste("Total records: ", nrow(data), sep = ""
    )
  })
}

shinyApp(ui, server)
