library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #1e1e1e; /* Dark background for body */
        color: #dcdcdc; /* Light text color for readability */
        margin: 0;
      }
      .welcome-container {
        text-align: center;
        padding: 50px;
        background: linear-gradient(to bottom, #2f3640, #1e272e);
        color: #ffffff;
        border-radius: 10px;
        margin: 20px;
      }
      .welcome-container h1 {
        font-size: 4em;
        margin-bottom: 20px;
      }
      .welcome-container p {
        font-size: 1.5em;
        margin-bottom: 30px;
      }
      .cta-button {
        padding: 15px 30px;
        font-size: 1.2em;
        background-color: #44bd32;
        color: #ffffff;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        box-shadow: 0 0 15px rgba(0,0,0,0.1);
        transition: background-color 0.3s ease;
      }
      .cta-button:hover {
        background-color: #1e9e2f;
      }
      .image-container {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 30px;
        gap: 20px;
      }
      .image-container img {
        width: 250px;
        height: auto;
        border-radius: 10px;
        box-shadow: 0 0 20px rgba(0,0,0,0.2);
        transition: transform 0.3s ease;
      }
      .image-container img:hover {
        transform: scale(1.1);
      }
      /* Dark sidebar and header styles */
      .sidebar {
        background-color: #2f3640;
        color: #ffffff;
        height:900px;
      }
      .sidebar .nav-pills > li > a {
        color: #ffffff;
      }
      .sidebar .nav-pills > li > a:hover {
        background-color: #44bd32;
      }
      .sidebar .active > a {
        background-color: #44bd32;
        color: white;
      }
      .sidebar .nav-pills {
        background-color: #2f3640;
      }
      .navbar {
        background-color: #1e272e;
      }
      /* Dark theme for About section */
      .about-container {
        background-color: #2f3640;
        color: #ffffff;
        padding: 40px;
        border-radius: 10px;
      }
      .footer {
        text-align: center;
        margin-top: 50px;
        padding: 20px;
        color: #dcdcdc;
        background-color: #2f3640;
        border-radius: 5px;
      }
    "))
  ),
  
  # Page layout
  uiOutput("ui_page")
)
# Define server
server <- function(input, output, session) {
  
  # Sample user data (replace with your real user data or a database for production)
  users <- data.frame(
    username = c("user1", "admin"),
    password = c("password1", "admin123"),
    stringsAsFactors = FALSE
  )
  
  user_logged_in <- reactiveVal(FALSE)
  
  # Load the dataset from CSV
  uber_data <- read.csv('/Users/souravpoonia/Desktop/Code/r programming lab/data1.csv')
  
  # Ensure 'pickup_datetime' is properly converted to POSIXct
  uber_data$pickup_datetime <- as.POSIXct(uber_data$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
  uber_data$Weekday <- weekdays(uber_data$pickup_datetime)
  uber_data$hour <- format(uber_data$pickup_datetime, "%H")
  
  # Filtered data reactive
  filtered_data <- reactive({
    req(input$weekday, input$miles_range, input$fare_range, input$hour_range, input$date_range)
    data <- uber_data %>%
      filter((Weekday == input$weekday | input$weekday == "All"),
             fare_amount >= input$fare_range[1], fare_amount <= input$fare_range[2],
             as.numeric(hour) >= input$hour_range[1], as.numeric(hour) <= input$hour_range[2],
             pickup_datetime >= input$date_range[1] & pickup_datetime <= input$date_range[2])
    validate(need(nrow(data) > 0, "No data available for the selected filters"))
    return(data)
  })
  
  # UI switching between the landing page and dashboard
  output$ui_page <- renderUI({
    if (!user_logged_in()) {
      fluidPage(
        div(class = "welcome-container",
            h1("Welcome to Uber Data Analysis"),
            p("Explore insightful analytics and powerful visualizations to understand Uber trip patterns."),
            actionButton("go_to_dashboard", "Start Exploring", class = "cta-button")
        ),
        
        # Added content for more details about the application
        div(class = "info-container",
            h2("What You Can Explore"),
            p("This application provides a wide range of analytical tools to explore Uber trip data."),
            p("You can filter trips based on various parameters like weekday, fare, hour, and more. The data is displayed using different types of visualizations like scatter plots, bar charts, pie charts, and predictive models."),
            h3("Key Features:"),
            tags$ul(
              tags$li("Filter data by weekday, fare range, hour range, and date range."),
              tags$li("View data visualizations including scatter plots and bar charts."),
              tags$li("Understand trip patterns and trends."),
              tags$li("Explore predictive models for fare prediction.")
            ),
            actionButton("go_to_dashboard", "Start Exploring", class = "cta-button")
        ),
        
        
        
        # Footer section for credits
        div(class = "footer",
            p("Developed by Sourabh Prajapat and Sourav Poonia"),
            p("For any queries, contact us at: example@email.com")
        )
      )
    } else {
      # Show the dashboard
      dashboardPage(
        dashboardHeader(
          title = "Uber Data Analysis", 
          titleWidth = 500,
          # Add logout button to the right side
          tags$li(class = "dropdown",
                  actionLink("logout", "Logout", icon = icon("sign-out"), class = "logout-button")
          )
        ),
        dashboardSidebar(
          width = 350,  # Increased sidebar width
          sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Filters", tabName = "filters", icon = icon("filter")),
            menuItem("Scatter Plot", tabName = "scatter", icon = icon("chart-line")),
            menuItem("Bar Chart", tabName = "bar", icon = icon("chart-bar")),
            menuItem("Pie Chart", tabName = "pie", icon = icon("chart-pie")),
            menuItem("New Charts", tabName = "new_charts", icon = icon("chart-area")),
            menuItem("Predictive Model", tabName = "model", icon = icon("calculator")),
            menuItem("Predicted Fare", tabName = "predicted_fare", icon = icon("money-bill-wave")),
            menuItem("Rush Hour Analysis", tabName = "rush_hour", icon = icon("clock")),
            menuItem("About", tabName = "about", icon = icon("info-circle"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "home", 
                    div(
                      h2("Welcome to Uber Data Analytics"),
                      p("This platform offers a comprehensive analysis of Uber trip data through interactive visualizations and data filters."),
                      h3("Key Highlights:"),
                      tags$ul(
                        tags$li("*Filter trips* by weekday, fare range, distance, hour, and date to explore specific patterns."),
                        tags$li("Visualize data using scatter plots, bar charts, pie charts, and more."),
                        tags$li("Analyze trends like popular trip hours, peak days, and fare distributions."),
                        tags$li("Predict fare amounts based on input parameters such as trip distance and time."),
                        tags$li("Discover insights into passenger behaviors and trip details.")
                      ),
                      h3("How to Use:"),
                      tags$ol(
                        tags$li("Use the *Filters* tab to narrow down your data selection."),
                        tags$li("Navigate to visualization tabs (Scatter Plot, Bar Chart, etc.) to explore different charts."),
                        tags$li("Try out the *Predictive Model* tab to estimate fare amounts."),
                        tags$li("Check out the *About* section to learn more about the project.")
                      ),
                      p("We hope you find this application insightful and easy to use.")
                    )
            ),
            tabItem(tabName = "filters", 
                    h2("Filters"),
                    selectInput("weekday", "Select Weekday", choices = c("All", unique(uber_data$Weekday)), selected = "All"),
                    sliderInput("miles_range", "Select Miles Range", min = 0, max = 20, value = c(0, 20)),
                    sliderInput("fare_range", "Select Fare Range", min = 0, max = 50, value = c(0, 50)),
                    sliderInput("hour_range", "Select Hour Range", min = 0, max = 23, value = c(0, 23)),
                    dateRangeInput("date_range", "Select Date Range", start = min(uber_data$pickup_datetime), end = max(uber_data$pickup_datetime))
            ),
            tabItem(tabName = "scatter", plotOutput("fare_passenger_scatter")),
            tabItem(tabName = "bar", plotOutput("weekday_bar_chart")),
            tabItem(tabName = "pie", plotlyOutput("passenger_pie_chart")),
            tabItem(tabName = "new_charts", 
                    h3("New Interactive Charts"),
                    plotOutput("new_chart_1"),
                    plotOutput("new_chart_2")
            ),
            tabItem(tabName = "model", 
                    h2("Predictive Model"),
                    plotOutput("prediction_chart")
            ),
            tabItem(tabName = "predicted_fare", 
                    h2("Predicted Fare"),
                    numericInput("miles_input", "Enter Miles", value = 10, min = 1, max = 100),
                    numericInput("hours_input", "Enter Hours", value = 2, min = 1, max = 24),
                    actionButton("predict_fare_button", "Predict Fare"),
                    textOutput("predicted_fare")
            ),
            tabItem(tabName = "rush_hour", 
                    h2("Rush Hour Analysis"),
                    plotOutput("rush_hour_analysis"),
                    p("This chart helps to identify the peak times for Uber trips, which can be critical for fare prediction and supply-demand optimization.")
            ),
            tabItem(tabName = "about", 
                    
                    div(class = "about-section", 
                        h3("About the Project"),
                        p("This dashboard is built to analyze Uber trips."),
                        p("Features include data filters, charts, and predictive models."),
                        p("Developer: Sourabh Prajapat and Sourav Poonia")
                    )
            )
          )
        )
      )
    }
  })
  
  # Handle redirect to the main dashboard on 'Start Exploring' button click
  observeEvent(input$go_to_dashboard, {
    showModal(modalDialog(
      title = "Login",
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("submit_login", "Login", class = "cta-button"),
      actionButton("register_button_modal", "Register", class = "cta-button"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Handle Login Authentication
  observeEvent(input$submit_login, {
    user <- input$username
    pass <- input$password
    
    # Validate user credentials
    if (user %in% users$username && pass == users$password[users$username == user]) {
      user_logged_in(TRUE)
      removeModal()  # Close the login modal
    } else {
      showModal(modalDialog(
        title = "Login Failed", 
        "Invalid username or password.",
        easyClose = TRUE
      ))
    }
  })
  
  # Registration functionality (example only, in practice use secure storage)
  observeEvent(input$register_button_modal, {
    showModal(modalDialog(
      title = "Register New User",
      textInput("new_username", "New Username"),
      passwordInput("new_password", "New Password"),
      actionButton("submit_register", "Register", class = "cta-button"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Handle User Registration
  observeEvent(input$submit_register, {
    new_user <- input$new_username
    new_pass <- input$new_password
    
    # Example: add new user to the data frame (replace with secure storage in production)
    users <<- rbind(users, data.frame(username = new_user, password = new_pass, stringsAsFactors = FALSE))
    removeModal()
    showModal(modalDialog(
      title = "Registration Success", 
      "You have successfully registered.",
      easyClose = TRUE
    ))
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    user_logged_in(FALSE)
  })
  
  # Render the scatter plot (updated code)
  output$fare_passenger_scatter <- renderPlot({
    ggplot(filtered_data(), aes(x = passenger_count, y = fare_amount)) +
      geom_point(color = "#6c5ce7", alpha = 0.7) +
      labs(title = "Fare vs Passenger Count", x = "Passenger Count", y = "Fare ($)") +
      theme_minimal()
  })
  # Predicted Fare Calculation
  observeEvent(input$predict_fare_button, {
    # Example prediction logic
    predicted_fare_value <- (input$miles_input * 2) + (input$hours_input * 3)
    output$predicted_fare <- renderText({
      paste("Predicted Fare: $", round(predicted_fare_value, 2))
    })
  })
  # Render the bar chart
  
  output$weekday_bar_chart <- renderPlot({
    trip_count_by_day <- uber_data %>%
      group_by(Weekday) %>%
      summarise(trip_count = n())
    
    ggplot(trip_count_by_day, aes(x = Weekday, y = trip_count)) +
      geom_bar(stat = "identity", fill = "#00cec9") +
      labs(title = "Trips by Weekday", x = "Weekday", y = "Trip Count") +
      theme_minimal()
  })
  
  # Render the pie chart (with Plotly)
  output$passenger_pie_chart <- renderPlotly({
    passenger_count_distribution <- uber_data %>%
      mutate(passenger_category = cut(passenger_count, breaks = c(0, 1, 2, 3, 4, 5), labels = c("1", "2", "3", "4", "5+"))) %>%
      group_by(passenger_category) %>%
      summarise(count = n())
    
    plot_ly(passenger_count_distribution, labels = ~passenger_category, values = ~count, type = 'pie') %>%
      layout(title = "Passenger Count Distribution", showlegend = TRUE)
  })
  # Prediction chart
  output$prediction_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = passenger_count, y = fare_amount)) +
      geom_point(color = "#6c5ce7", alpha = 0.6) +
      geom_smooth(method = "lm", color = "#44bd32", se = FALSE) +
      labs(title = "Fare Prediction Model", x = "Passenger Count", y = "Fare ($)") +
      theme_minimal()
  })
  # New Chart Outputs
  output$new_chart_1 <- renderPlot({
    ggplot(filtered_data(), aes(x = fare_amount, y = Miles)) + 
      geom_point() + 
      labs(title = "Fare vs Miles", x = "Fare Amount", y = "Miles")
  })
  
  output$new_chart_2 <- renderPlot({
    ggplot(filtered_data(), aes(x = pickup_datetime, fill = Weekday)) + 
      geom_histogram(bins = 50) +
      labs(title = "Trips by Weekday", x = "Date", y = "Number of Trips")
  })
  output$rush_hour_analysis <- renderPlot({
    # Get the filtered data based on user input
    data <- filtered_data() 
    
    # Extract the hour from the filtered data
    hour_distribution <- data %>%
      mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%  # Extract hour from datetime
      group_by(hour) %>%
      summarise(count = n(), .groups = "drop")  # Count number of trips for each hour
    
    # Create a line plot with better visual representation
    ggplot(hour_distribution, aes(x = hour, y = count)) +
      geom_line(color = "#ff6348", size = 1.5) +  # Line plot with a specific color and size
      geom_point(color = "#ff6348", size = 4) +  # Add points for clarity
      scale_x_continuous(breaks = 0:23) +  # Ensures x-axis shows all hours of the day
      labs(title = "Rush Hour Analysis", x = "Hour of Day", y = "Trip Count") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Angle x-axis labels for better readability
        axis.text.y = element_text(size = 10)
      )
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)