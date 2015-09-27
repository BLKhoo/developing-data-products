# Application to determine the low enough price based on stock CAGR and lowest point in the growth trend line based on linear 
# regression and also the log price assume stock price over time would have 2^t growth where t is the time.

library(shiny)
shinyUI(pageWithSidebar(
  
  headerPanel("What should be a low enough price considering Stock fundamental CAGR?"),

  sidebarPanel(
    
    h5("Please allow some time for the server to response due to  networklatency "),
    textInput('stk',"Update valid Stock code:","IBM"),
    #numericInput('id1', 'offset LM line',-1,min=-10,max=10,step = 1),
    h5(" It is assumed here that stock price reflects fundamental and more importantly the sentiments and market psychology which cause the gyrations.
       The general trend could be considered as the fundamental CAGR which can be positive or negative.  Is is safer to invest in POSITIVE CAGR stock"),
    h5(" The log plot on the right shows the plot of log price over time  with a LM line , considered as the compounded annual growth of the stock,. 
       Adjusting the offset to bring the lower black line to touch lowest price points (at least 2 points) would indicate the lowest price point to invest , resulting in higher probability of upside or gain"),
    h5("The red lm line represents the  neutral CAGR or fair CAGR. Any price below would provide more discount and the lower the better for long term positive CAGR stock"),
    sliderInput("id1","Enter offset:",value=0,min=0,max=4,step=0.1),
 #   dateInput("date", "Date:"),

    submitButton("Submit"),
    

 
    h6("DISCLAMER: Do not base any investment and or trading decision solely on the information provided here. 
       We accept no liability if you use the information to form trading or 
       investing decisions or to trade real money. You should seek your own investment 
       advice before making any decision based on the information from a licensed financial 
       professional who will consider your personal objectives and circumstances.
       
       Note that data source is obtained from yahoo & subject to availability and accuracy of the data as provided")
  ),

  mainPanel(
    
    h4( "Stock /Date / Value /CAGR"),
    verbatimTextOutput("ostk"),
    
    plotOutput('stkPlot'),
    plotOutput('stkPlot2')

  )

))