library(shiny)
library(quantmod)
library(lubridate)
library(date)
library(ggplot2)
library(grid)
library(scales)



getdata <- function(ticker) {
  
  symbol <<- ticker
  stk <<- getSymbols(ticker,auto.assign=FALSE)
  names(stk)[1] <<-"open"
  names(stk)[2] <<-"high"
  names(stk)[3] <<-"low"
  names(stk)[4] <<-"close"
  names(stk)[5] <<- "vol"
  names(stk)[6] <<- "price"
  sampleTimes <<- index(stk) 
  stk$year <<- year(index(stk))
  stk$mth <<- month(index(stk))
  
}

# run this first
fp <-function(ticker) {
  par(mfrow = c(2,2))
  getdata(ticker)
  d <<- stk[,6]
  breaks <- seq(min(d),max(d),by=0.1)
  d.cut     <- cut(d, breaks,right=FALSE)
  d.freq <- table(d.cut)
  barplot(d.freq,xlab="Price",ylab="Freq",main="Price histogram")
  vplot()
  plot(stk$price,main="Price vs Date",xlab="Date",ylab="Price")
  plot(stk$vol,main="volume vs Date",xlab="Date",ylab="Volume")
  #r <- cagr()
  #print(c("CAGR",  r),digits = 2)
  
#   print(tail(stk))
#   summary(d)
  
}

vplot <-function() {
  #stk = getSymbols(ticker,auto.assign=FALSE)
  #sampleTimes <<- index(stk) 
  d <- stk[,5]/1000
  breaks <- seq(min(d),max(d),by=1000)
  d.cut     <- cut(d, breaks,right=FALSE)
  d.freq <- table(d.cut)
  barplot(d.freq,xlab = "Volume (',000)",ylab="Freq",main="Volume histogram")
  
  return(summary(d))
}

facetp <- function(bin=1)
{
  
  
  l <- tail(stk,1)
  p1 <<- qplot(price,data=stk,facets=year ~ ., binwidth = bin ,fill = year)  +  
    geom_vline(xintercept=as.numeric(l$price)) + labs(title="Price distribution by year") +
    theme(legend.position="none") +xlab("Price")
  
  l <- tail(stk,1)
  p2 <<- ggplot(stk, aes(factor(year), price))  +  
    geom_boxplot() +labs(title="Box plot by year") + geom_hline(yintercept=as.numeric(l$price)) +
    xlab("Year") + geom_jitter(alpha = 0.2,aes(colour = "red") ) +  theme(legend.position="none")
  
  
  multiplot(p1,p2,cols=2)
}




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



lplot <- function() 
{ ggplot( data=stk,aes(x=index(stk),y=price)) +
    geom_smooth(method="lm",se=FALSE,col="red") +
    geom_line() + labs(title="Log Price") + xlab("Year") +
    scale_y_continuous(trans = log2_trans(),
                       breaks = trans_breaks("log2", function(x) 2^x),
                       labels = trans_format("log2", math_format(2^.x)))  + ylab( expression(paste("Log"[2],' Price')))
  
  }




lmplot <- function(offset=0)
{ p <- lplot()
  stk$o <- offset
  m <- lm(log(stk$price,base=2) ~ index(stk),offset=stk$o)
  l <- geom_abline(intercept = coef(m)["(Intercept)"], slope = coef(m)["index(stk)"],lwd=1)
  p + l
  
}


shinyServer(
  
  function  (input,output) {
    
      
      s <- reactive({ input$stk })
      
      
      output$ostk <- renderPrint({
        
        offset <- input$id1
        getdata( s() )

        
        stk$o <- offset
        m <- lm(log(stk$price,base=2) ~ index(stk),offset=stk$o)
        current.date <- index(tail(stk$price,1))    
# calculate the value from the offset lm line
        i <- coef(m)["(Intercept)"]
        x <- coef(m)["index(stk)"]
        if (x > 0 ) cagr <- "POSITIVE CAGR" else cagr <- "NEGATIVE CAGR"
        y <- i + x*as.numeric(current.date)
        value <- 2^y
        paste(s()," ",current.date," LM current calculated (black line) value:",sprintf("$ %3.2f", value)," ", cagr)
        
        })
      
   
      
        output$stkPlot <- renderPlot({

         temp <- s() # added to reference  for trigger when changed
         offset <- input$id1
         lmplot(offset)
         
               
        })

      output$stkPlot2 <- renderPlot({    
        temp <- s() 
        facetp()  
  
        })
    
  }
)