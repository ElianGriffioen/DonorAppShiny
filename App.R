library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Check your donor strategy"),
  p("This Shiny application makes it possible to check your donor strategy. 
    The annual anti-RhD antibody production depends on the donation interval,
    the number of donations between boostering events and the donor pool size.
    After inserting these parameters, this application calculates the annual
    production, tells whether this production meets the (inserted) demand and
    shows how frequent all donors are boostered and donate by visualizing the
    trajectory of a donor."),
  sidebarLayout(
    sidebarPanel(
      #helpText("Select donation and boostering intervals to visualize the
      #         corresponding trajectory of donors. Insert the demand for
      #         anti-RhD products and donor pool size to check whether the
      #         anti-RhD production allows fulfilling the demand."),
      # donation interval slider
      sliderInput("If", h4("Donation Interval (days)"),
                  min = 14, max = 365, value = 30),
      
      # number of donations between boostering events slider
      sliderInput("n", h4("Number of donations between Boostering events"),
                  min = 1, max = 26, value = 3),
      
      # demand for anti-RhD products
      numericInput("demand", 
                   h4("Demand for anti-RhD products"), 
                   value = 1000),
      
      # donor pool size
      numericInput("donorpoolsize", 
                   h4("Size of the donor pool"), 
                   value = 300)
    ),
    
    mainPanel(
      
      # does strategy meet the demand
      h4("Does your strategy allow meeting the demand for anti-RhD products?"),
      # text
      textOutput("selected_demand"),
      br(),
      
      # what does the trajectory look like
      h4("What does the donor trajectory look like?"),
      p("The trajectory of a donor with the actual donation strategy is 
        visualized in the graph below. Orange dots show donations, the 
        increase in titre reflects a boostering event."),
      plotOutput(outputId = "donorplot",height = "320px")
      
    )
  )
)
# Define server logic ----
server <- function(input, output) {
  
  # set parameters based on data
  Babs <- 3.85
  c <- 0.37
  D <- 0.00081
  library(ggplot2)
  
  # reactive function that does only not depend on input$demand
  YieldPerYear <- reactive({
    # calculate yield per donor
    store<-rep(NA,input$n)
    for(i in 0:(input$n-1)){
      store[i+1] <- 2^((Babs+(c-1)*input$n*input$If*D)/c - i*D*input$If)
    }
    YieldPerDonor<-mean(store)
    
    # no of donations per year per donor = 365/If (donation interval)
    no.of.donation.year.donor <- floor(365/input$If)
    # no of donations per year = no of donations per year per donor * no of donors
    no.of.donation.year <- no.of.donation.year.donor*input$donorpoolsize
    # yield in year = no of donations per year * yield per donor
    YieldPerYear <- no.of.donation.year*YieldPerDonor
    return(YieldPerYear)
  })
  
  # text output for meeting the demand
  output$selected_demand <- renderText({
    
    # different messages based on whether demand is fulfilled or not
    if(YieldPerYear() > input$demand){
    paste("Good job! The annual production of anti-RhD products is", round(YieldPerYear()),
          "and is bigger than the demand of",input$demand,"anti-RhD products.")
    }else{
      paste("Oops! The annual production of anti-RhD products is", round(YieldPerYear()),
            "and is smaller than the demand of",input$demand,"anti-RhD products.")
    }
  })
  
  # plot output for visualizing trajectory
  output$donorplot <- renderPlot({
    
    # make dataset for donations with xp as time points and yp as titre points
    xp<-c()
    yp<-c()
    
    points <- ceiling(3650/input$If) # number of points in data
    
    # get time points by adding donation interval multiple times
    for(i in 1:points){
      xp[i] <- 1 + (i-1)*input$If
    }
    
    # get titre points between boostering events by inserting formula
    for(i in 1:input$n){
      yp[i] <- (Babs + (c-1)*input$n*input$If*D)/c - (i-1)*D*input$If
    }
    
    # repeat yp to make it as long as xp
    len.dat <- ceiling(points/input$n)
    yp2 <- rep(yp,len.dat)
    yp2 <- yp2[1:length(xp)]
    
    # bind data
    donations <- cbind.data.frame(xp,yp2)
    

    
    
    # make dataset for lines between donations with xl as time points and yl
    # as titre points. This dataset is slightly different compared to the 
    # previous
    
    # take set of points xp between boostering events
    xl.unit <- xp[1:(input$n+1)]
    
    # change final one
    xl.unit[input$n+1] <- xl.unit[input$n+1]-1
    
    # repeat this set multiple times
    xl<-c()
    for(i in 1:len.dat){
      xl.add <- xl.unit + (i-1)*input$n*input$If
      xl <- c(xl,xl.add)
    }
    
    # calculate titre values for points between boostering intervals
    yl.unit <- c()
    for(i in 1:(input$n+1)){
      yl.unit[i] <- (Babs + (c-1)*input$n*input$If*D)/c - (i-1)*D*input$If
    }
    
    # make multiple repetitions
    len.dat <- ceiling(points/input$n)
    yl <- rep(yl.unit,len.dat)
    yl <- yl[1:length(xl)]
    
    # bind variables to dataset
    donations.line <- cbind.data.frame(xl,yl)

    # start plot with lines and points
    trajectory <- ggplot() + 
      geom_line(data=donations.line, aes(x=xl,y=yl),col="deepskyblue",size=1.2) +
      geom_point(data=donations, aes(x=xp,y=yp2),col="darkorange",size=2) +
      scale_x_continuous(limits=c(0,1825)) +
      labs(x = "Time (days)", y="Titre") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    
    trajectory
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)