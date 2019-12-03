library(purrr)
library(shiny)
library(shinythemes)
library(sf)
library(broom)
library(gt)
library(tidyverse)


ui <- navbarPage(theme = shinytheme("sandstone"), "New York City Airbnb and Housing Prices",
                 tabPanel("Neighborhood Graphs",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  h4("About"),
                                  p("These graphs display neighborhoods of the New York City boroughs 
                                    and their associated median Airbnb price per night or median home value."),
                                  p("The borough to be viewed can be selected--as well as whether to display 
                                    all of the neighborhoods (with available data) or just the 15 most or 
                                    least expensive in a given category."),
                                  selectInput(inputId = "neighborhood",
                                              label = "Borough",
                                              choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                                  
                                  radioButtons(inputId = "radio", 
                                               label = "Featured Neighborhoods",
                                               choices = list("All" = 1, "Most Expensive" = 2, "Least Expensive" = 3), selected = 1)
                              ),
                              
                              mainPanel(
                                  plotOutput("graph1"),
                                  plotOutput("graph2")
                              )
                          )
                 ),
                 tabPanel("Price Map",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  h4("About"),
                                  p("This map shows the neighborhoods of New York City colored by 
                                  their price. More red neighborhoods are more expensive"),
                                  radioButtons(inputId = "data", 
                                               label = "Data",
                                               choices = list("Median House Value" = 1, "Median Airbnb Price Per Night" = 2), selected = 1),
                                  p("(gray neighborhoods are missing data)")
                              ),
                              
                              mainPanel(
                                  plotOutput("map")
                              )
                          )
                 ),
                 navbarMenu("Statistical Analysis",
                            tabPanel("Map",
                                     sidebarLayout(
                                         sidebarPanel(
                                             h4("About"),
                                             p("This map shows the boroughs of New York City colored by 
                                               their linear regression coefficient."),
                                             p("The coefficients come from a linear regression that explains
                                               median Airbnb price per night by median home value for a given
                                               neighborhood."),
                                             p("More red neighborhoods have higher coefficients, indicating
                                               that neighborhoods of this borough have high correlation 
                                               between airbnb prices and home values.")
                                         ),
                                         
                                         mainPanel(
                                             plotOutput("stats_map")
                                         )
                                     )
                            ),
                            
                            tabPanel("Graph",
                                     sidebarPanel(
                                         h4("About"),
                                         p("This graph plots neighborhoods with their median house value
                                           (in thousand dollars) on the x axis and median airbnb price
                                           per night (in dollars) on the y axis. 
                                           The neighborhoods are colored by their corresponding borough."),
                                         p("A linear regression was then run to explain the airbnb 
                                           price by median home value and the resulting best-fit line 
                                           plotted. Below the graph is the average coefficient value 
                                           (slope of the regression line), the 5th and 95th percentile 
                                           values to give an indication of uncertainty associated with 
                                           the term, and its corresponding r-squared value to give an 
                                           indication of fit.")
                                     ),
                                     
                                     mainPanel(
                                         plotOutput("stats"),
                                         h4("Linear Regression Information:"),
                                         tableOutput("stats_table")
                                     )
                            )
                            
                            
                            
                 ),
                 tabPanel("About",
                          h2("About The Project"),
                          p("This project aims to provide insights on house/lodging pricing in New York City.
                            New York City is one of the most analyzed cities in terms of housing and lodging prices,
                            not only because it is one of the largest cities in the US, but because it has a very unique
                            housing market that is subject to a number of stereotypes. Through this project, I hope to shed
                            some light on what ideas and theories about New York housing are false, and which hold some truth.
                            I also used statistical analysis to determine if housing prices in a neighborhood could explain
                            or even predict airbnb prices in the neighborhood."),
                          p("The results of analyzing this data confirmed some stereotypes about NYC. For example,
                            Manhattan had the highest median home values and the highest average Aibnb prices,
                            and Staten Island had the lowest. However, the two neighborhoods with the highest median Airbnb 
                            prices were in Staten Island! From my statstical analysis I found a very slight positive correlation
                            between median housing prices and median airbnb prices in different neighborhoods. Indicating that
                            generally as house prices increase so would airbnb prices in a neighborhood, but the correlation
                            was not very strong (and had a low r-squared value of .14) meaning housing prices probably wouldn't
                            be a very good predictor of airbnb prices. I did run this linear regression on each borough and found
                            this correlation was slightly positive across the board, with Staten Island having the highest 
                            correlation and Manhattan having the lowest."),
                          br(),
                          h2("About The Data"),
                          p("Zillow is an online real estate database. Zillow collects data on houses, apartments, condos, etc.
                            that are for sale and for rent all across the United States. It tracks a number of variables
                            (demographic, location, credit scores, etc) including listed prices and sale prices which are 
                            used to make algorithms which create accurate estimates of house values. The data for this 
                            project comes from Zillow's 'Zillow Research' platform (https://www.zillow.com/research/data/). 
                            Zillow research is independent from
                            Zillow's revenue center and aims to provide open, accurate data on the US housing market.
                            "),
                          p("The specific data from zillow used in this project is from September 30th 2019. 
                          The data contained information on this month's zhvi value for all homes (home, apartment,
                          condo, etc.) on the neighborhood level. Thus, there was data on neighborhoods of metro 
                          areas all across the US (I, however, selected only NYC data to analyze). The zhvi (Zillow Home Value Index) value is Zillow's smoothed, 
                          time-dependent measure of the median estimated home value across a given region in USD.
                            "),
                          br(),
                          p("Airbnb is a website that allows users to arrange and offer lodging. The company is one 
                            of the biggest lodging platforms, with big implications on local hospitality industries. 
                            The data used for this project comes from Inside Airbnb (http://insideairbnb.com/about.html), which 
                            is a set of data (independent from Airbnb the company) created to help people explore how Airbnb is really 
                            being used around the world."),
                          p("This specific data for this project contains host, lodging, location, price, and review information
                            for airbnbs in New York City. I also used data from Inside Airbnb that contained shapefiles for
                            all the neighborhoods in NYC in order to plot results on a map."),
                          br(),
                          h2("Contact"),
                          p("Hi! I am Molly Chiang, a Sophmore at Harvard College studying Human Evolutionary
                            Biology, with a new love for data science! I can be reached at mollychiang@college.harvard.edu."),
                          p("This project was created for my Gov 1005 final project. The code can be accessed from its
                            github repo at https://github.com/mollyechiang/nyc-lodging.")
                            
                 ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$graph1 <- renderPlot({
        
        nyc_data <- read_rds("nyc_data.rds")
        
        nyc_data %>% 
            
            # create new column ave_price which gives the average price per neighborhood
            
            group_by(neighbourhood) %>%
            mutate(ave_price = ave(price)) %>%
            
            # remove those neighborhoods that have less than 5 airbnbs contributing to this ave price
            # these neighborhoods have so little data they can easily be skewed
            
            group_by(neighbourhood, neighbourhood_group, ave_price) %>%
            count() %>%
            filter(n>5) %>%
            arrange(desc(ave_price)) %>%
            
            # filter for the selected borough and selected number of neighborhoods to show
            
            filter(neighbourhood_group == input$neighborhood) %>%
            ungroup() %>%
            slice(switch(input$radio,
                         '1' = 1:n(),
                         '2' = 1:15,
                         '3' = -1:-(n()-15))) %>%
            
            # plot this data - with ave price on y axis and neighborhood on x
            
            ggplot(aes(x = neighbourhood, 
                       y = ave_price, 
                       fill = neighbourhood_group)) +
            geom_col() +
            labs(y = "Average Price Per Night ($)", 
                 x = " ", 
                 title = paste(input$neighborhood, "Airbnb Prices by Neighborhood", sep = " ")) +
            guides(fill = FALSE) +
            theme_classic() +
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            
            # set a y-lim so all boroughs are compared on same scale 
            
            ylim(0,500)
        
    })
    
    
    output$graph2 <- renderPlot({
        
        ppn_nyc_data <- read_rds("ppn_nyc_data.rds")
        
        ppn_nyc_data %>% 
            
            # change the scale of zhvi to be in million dollars (so easier to understand on graph)
            
            ungroup(zhvi) %>%
            mutate(zhvi = zhvi/1000000) %>%
            
            #drop NA rows, and arrange descending by zhvi
            
            drop_na(zhvi, neighbourhood_group) %>% 
            arrange(desc(zhvi)) %>%
            
            # filter for the selected borough and selected number of neighborhoods to show
            
            filter(neighbourhood_group == input$neighborhood) %>%
            ungroup() %>%
            slice(switch(input$radio,
                         '1' = 1:n(),
                         '2' = 1:15,
                         '3' = -1:-(n()-15))) %>%
            
            # plot this data - with zhvi (median house value) on y axis and neighborhood on x
            # add labels, a theme, and modify the angle of text on x axis
            
            ggplot(aes(x = neighbourhood, 
                       y = zhvi, 
                       fill = neighbourhood_group)) +
            geom_col() +
            labs(y = "Median House Value (in million $)", 
                 x = " ", 
                 title = paste(input$neighborhood, "House Values by Neighborhood", sep = " ")) +
            guides(fill = FALSE) +
            theme_classic() +
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            
            # add a y limit so all boroughs are compared on the same scale
            
            ylim(0,3)
    })
    
    
    output$map <- renderPlot({
        
        nyc_shapes_full <- read_rds("nyc_shapes_clean.rds")
        
        # change units of zhvi before plotting
        
        nyc_shapes_full <- nyc_shapes_full %>%
            ungroup(zhvi) %>%
            mutate(zhvi = zhvi/1000)
        
        # use switch() to change the data to use in our heat map
        
        data <- switch(input$data, 
                       '1' = nyc_shapes_full$zhvi,
                       '2' = nyc_shapes_full$median_ppn)
        
        # plot the different data on a map using geom_sf
        # change the fill gradient colors
        # add labels and theme
        
        ggplot() + 
            geom_sf(data = nyc_shapes_full, aes(fill = data)) +
            scale_fill_gradient(low = "wheat1", high = "red") +
            labs(title = "Heat Map of NYC Neighborhoods",
                 fill = case_when(input$data == '1' ~ "Median House Value (thousand $)",
                                  input$data == '2' ~ "Median Airbnb Price per Night ($)")) +
            theme_minimal()
        
    })
    
    
    output$stats_map <- renderPlot({
        
        stats <- read_rds("nyc_statistics.rds")
        
        # plot coefficients explaining airbnb price by median home value by borough
        # plot this on a map using geom_sf
        # add labels and theme
        
        ggplot() + 
            geom_sf(data = stats, aes(fill = slope)) +
            scale_fill_gradient(low = "wheat1", high = "red") + 
            labs(title = "Boroughs Colored by Linear Regression Coefficient",
                 subtitle = "Coefficient Explains Airbnb Price Per Night by Home Value",
                 fill = "Coefficient") +
            theme_minimal()
        
    })
    
    
    output$stats <- renderPlot({
        
        ppn_nyc_data <- read_rds("ppn_nyc_data.rds")
        
        ppn_nyc_data %>%
            drop_na() %>%
            
            # change units of zhvi so it's easier to understand
            
            ungroup(zhvi) %>%
            mutate(zhvi = zhvi/1000) %>%
            
            # plot zhvi vs  median_ppn, add color to points
            # add regression line with geom_smooth
            # add titles and theme
            
            ggplot(aes(x = zhvi, y = median_ppn)) +
            geom_jitter(aes(color = neighbourhood_group)) +
            geom_smooth(method = "lm") + 
            labs(title = "Explaining Median Airbnb Price By Median House Value in NYC Neighborhoods",
                 subtitle = "A Linear Regression",
                 x = "Median House Value (in thousand $)",
                 y = "Median Airbnb Price Per Night ($)",
                 color = "Borough") +
            theme_minimal()
        
    })
    
    
    output$stats_table <- renderTable({
        
        ppn_nyc_data <- read_rds("ppn_nyc_data.rds")
        
        # first clean imported data by changing units on zhvi
        # and dropping rows with NA
        
        cleaner_ppn <- ppn_nyc_data %>%
            drop_na() %>%
            ungroup(zhvi) %>%
            mutate(zhvi = zhvi/1000)
        
        # run the linear regression
        
        model <- lm(data = cleaner_ppn, formula = median_ppn ~ zhvi)
        
        # use confint_tidy function from broom to get the confidence intervals
        # on the coefficient (select for only coefficient)
        # add a label so the table can be joined with others
        
        conf_ints <- confint_tidy(model, conf.level = .9) %>% 
            mutate(label = c("Intercept", "Coefficient")) %>%
            filter(label == "Coefficient")
        
        # use glance function from broom to get r squared volume 
        # on the linear regression
        # add a label so this table can be joined with the others
        
        r_squared <- glance(model) %>%
            select(r.squared) %>%
            mutate(label = "Coefficient")
        
        # join all tables (only coefficient values)
        # select relevant columns and rename them
        
        tidy(model) %>%
            mutate(label = c("Intercept", "Coefficient")) %>%
            filter(label == "Coefficient") %>%
            select(label, estimate) %>%
            inner_join(conf_ints, by = "label") %>%
            inner_join(r_squared, by = "label") %>%
            select(estimate, conf.low, conf.high, r.squared) %>%
            rename("Coefficient" = estimate,
                   "5th Percentile" = conf.low,
                   "95th Percentile" = conf.high,
                   "R Squared" = r.squared)
        
    })
    
    
}


shinyApp(ui = ui, server = server)
