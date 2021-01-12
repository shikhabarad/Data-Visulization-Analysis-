
library(tcltk2)

filepath <- tclvalue(tkgetOpenFile()) # open file dialog box
filename <- basename(filepath)


bdata <- read.csv(filepath,TRUE,",") # read the csv file

if( filename == "Manufacturing_OS.csv")
{
  # list to slelect
  graph <- tk_select.list(c('Bargraph', 'Piechart'), preselect = NULL, multiple = FALSE, title = NULL)
  
  # year to be slected for the user
  year <- tk_select.list(c(2015, 2016, 2017, 2018), preselect = NULL, multiple = FALSE, title = NULL)
  
  if(graph == "Piechart"){
    # calculate the sum of page views for each operting system
    a1 = sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Android" ),])$Page.Views) 
    paste(a1) 
    a2 = sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "iOS" ),])$Page.Views)
    paste(a2)
    a3 = sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Macintosh" ),])$Page.Views)
    paste(a3)
    a4 = sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Windows" ),])$Page.Views)
    paste(a4)
    a5 = sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Linux" ),])$Page.Views)
    paste(a5)
    
    # calculate the sum of all page views
    rmg = subset(bdata, Year == year )
    a6 = sum(rmg$Page.Views) 
    paste(a6)
    
    #  calculate the other operating system
    a7 = a6-(a1+a2+a3+a4+a5) 
    paste(a7)
    paste("executed")
    
    slices <- c(a1, a2, a3, a4, a5, a7) 
    lbls <- c("Android", "ios", "Macintosh", "Windows", "Linux", "Others")
    pct <- round((slices/a6)*100, digits = 1)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    paste(lbls)
    
    # Give the chart file a name
    png(file = "OS_piechart_Test.jpg")
    
    # Plot the pie  chart 
    pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Users with different operating system")
   
    # Save the file
         dev.off()
  }
  if(graph == "Bargraph")
  {
    # calculate the sum of all page views
    rmg = subset(bdata, Year == year )
    a6 = sum(rmg$Page.Views)
    paste(a6)
    
    # calculate the percentage of page views for each operting system
    a1 = (sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Android" ),])$Page.Views)/a6)*100
    paste(a1)
    a2 = (sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "iOS" ),])$Page.Views)/a6)*100
    paste(a2)
    a3 = (sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Macintosh" ),])$Page.Views)/a6)*100
    paste(a3)
    a4 = (sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Windows" ),])$Page.Views)/a6)*100
    paste(a4)
    a5 = (sum((bdata[(bdata$Year == year ) & (bdata$Operating.System == "Linux" ),])$Page.Views)/a6)*100
    paste(a5)
    
    #  calculate the percentage of other operating system
    a7 = 100-(a1+a2+a3+a4+a5)
    paste(a7)
    
    
    H <- c(a1,a2,a3,a4,a5,a7)
    M <- c("Android","ios","Mac","Windows","Linux","others")
    
    # Give the chart file a name
    png(file = "os_bargraph_test2018.png")
    
    # Plot the bar chart 
    barplot(H,names.arg=M,xlab="OS",ylab="Users",col="blue",
            main="Bar graph of users with different operating system",border="red")
    
    # Save the file
    dev.off()
    
  }
  
}

if( filename == "Manufacturing_Day and Hour analysis.csv")
{
  
  select <- tk_select.list(c('Day analysis', 'Hour analysis'), preselect = NULL, multiple = FALSE, title = NULL)
  
  if(select == "Day analysis" )
  {
    
    colors = c("green","orange","brown","yellow","red","white","black","antiquewhite","aquamarine2","azure4","blueviolet","brown2","burlywood3","chartreuse1","cadetblue2","chocolate2","coral1","cornflowerblue","cornsilk4","darkolivegreen1","darkorchid2","darkseagreen2","deeppink1","firebrick1")
    Time = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
    Day = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
    
    total <- sum(bdata$Page.Views)
    
    i <- 1
    
    paste(i)
    
    #matrix declaration for holding 24 rows and 7 columns
    k <- matrix(, nrow = 24, ncol = 7)
    paste(k)
    
    j <- 1
    i <- 1
    
    while (j<25) {
      
      # calculate the percentage of page views for each day of the week
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Monday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 2
    while (j<25) {
      
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Tuesday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 3
    while (j<25) {
      
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Wednesday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 4
    
    while (j<25) {
      
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Thursday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 5
    
    while (j<25) {
      
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Friday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 6
    
    while (j<25) {
      
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Saturday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    j <- 1
    i <- 7
    
    while (j<25) {
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == j-1)& (bdata$Day.of.Week.Name == "Sunday" ),])$Page.Views))/total)*100
      j = j+1
    }
    
    
    
    paste(k)
    
    #Values <- matrix(while(j<169){c(k[j])}, nrow = 24, ncol = 7, byrow = TRUE)
    
    # Give the chart file a name
    png(file = "barchart_stacked_day_test1.png")
    
    # Create the bar chart
    barplot(k, main = "Users viewing in different day of the week", names.arg = Day, xlab = "Day", ylab = "Time", col = colors)
    
    # Add the legend to the chart
    legend("topleft", Time, cex = 0.5, fill = colors)
    
    # Save the file
    dev.off()
    
  }
  if(select == "Hour analysis")
  {
    
    
    colors = c("green","orange","brown","yellow","red","white","black")
    Time = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
    Day = c("Monday","Tuesday","Wednesday","Thrusday","Friday","Saturday","Sunday")
    
    
    total <- sum(bdata$Page.Views);
    
    i <- 1
    paste(i)
    
    
    #matrix declaration for holding 24 rows and 7 columns
    k <- matrix(, nrow = 7, ncol = 24)
    paste(k)
    
    while(i<25){
      
      j <- 1;
      
      # calculate the percentage of page views for each hour of the day
      
      k[j,i] <- ((sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Monday" ),])$Page.Views))/total)*100
      
      j <- j+1
      
      
      k[j,i] <- (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Tuesday" ),])$Page.Views)/total)*100
      
      j <- j+1
      
      k[j,i] = (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Wednesday" ),])$Page.Views)/total)*100
      
      j <- j+1
      
      k[j,i] = (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Thursday" ),])$Page.Views)/total)*100
      
      j <- j+1
      
      k[j,i] = (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Friday" ),])$Page.Views)/total)*100
      
      j <- j+1
      
      k[j,i] = (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Saturday" ),])$Page.Views)/total)*100
      
      j <- j+1
      
      k[j,i] = (sum((bdata[(bdata$Hour == i)& (bdata$Day.of.Week.Name == "Sunday" ),])$Page.Views)/total)*100
      
      
      i <- i+1
    }
    
    paste(k)
    
    #Values <- matrix(while(j<169){c(k[j])}, nrow = 7, ncol = 24, byrow = TRUE)
    
    # Give the chart file a name
    png(file = "barchart_stacked_hour1.png")
    
    # Create the bar chart
    barplot(k, main = "Users viewing in different hour of the day", names.arg = Time, xlab = "Time", ylab = "Day", col = colors)
    
    # Add the legend to the chart
    legend("topleft", Day, cex = 1.3, fill = colors)
    
    # Save the file
    dev.off()
    
  }
  
}


if( (filename == "Agriculture_Browser.csv") | (filename == "Manufacturing_Browser.csv") )
{
  
  graph <- tk_select.list(c('Bargraph', 'Piechart'), preselect = NULL, multiple = FALSE, title = NULL)
  
  year <- tk_select.list(c(2015, 2016, 2017, 2018), preselect = NULL, multiple = FALSE, title = NULL)
  
  if(graph == "Piechart")
  {
    a1 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Chrome" ),])$Page.Views)
    paste(a1)
    a2 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Firefox" ),])$Page.Views)
    paste(a2)
    a3 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Opera" ),])$Page.Views)
    paste(a3)
    a4 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "UC Browser" ),])$Page.Views)
    paste(a4)
    a5 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Internet Explorer" ),])$Page.Views)
    paste(a5)
    a6 = sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Safari" ),])$Page.Views)
    paste(a6)
    rmg = subset(bdata, Year == year )
    a7 = sum(rmg$Page.Views)
    paste(a7)
    a8 = a7-(a1+a2+a3+a4+a5+a6)
    paste(a8)
    
    slices <- c(a1, a2, a3, a4, a5, a6, a8) 
    lbls <- c("Chrome", "Firefox", "Opera", "UC Browser", "Internet Explorer", "Safari", "Others")
    pct <- round((slices/a7)*100, digits=1)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    # Give the chart file a name
    png(file = "browser_ariculture_test1.jpg")
    
    # Create the pie chart
    pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Users with different browser ")
    
    # Save the file
    dev.off()
    
  }
  
  if(graph == "Bargraph")
  {
    rmg = subset(bdata, Year == year )
    a7 = sum(rmg$Page.Views)
    paste(a7)
    
    a1 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Chrome" ),])$Page.Views)/a7)*100
    paste(a1)
    a2 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Firefox" ),])$Page.Views)/a7)*100
    paste(a2)
    a3 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Opera" ),])$Page.Views)/a7)*100
    paste(a3)
    a4 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "UC Browser" ),])$Page.Views)/a7)*100
    paste(a4)
    a5 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Internet Explorer" ),])$Page.Views)/a7)*100
    paste(a5)
    a6 = (sum((bdata[(bdata$Year == year ) & (bdata$Browser == "Safari" ),])$Page.Views)/a7)*100
    paste(a6)
    
    a8 = 100-(a1+a2+a3+a4+a5+a6)
    
    
    H <- c(a1,a2,a3,a4,a5,a6,a8)
    M <- c("Chrome", "Firefox", "Opera", "UC", "InternetExp", "Saf", "Othrs")
    
    # Give the chart file a name
    png(file = "agriculture_Browser_bargraph_test2018.png")
    
    # Plot the bar chart 
    barplot(H,names.arg=M,xlab="Browser",ylab="Users",col="Red",
            main="Bargraph of users different Browser in agriculture sector 2018",border="red")
    
    # Save the file
    dev.off()
    
    
  }
  
}
