#####Packages
library(ggplot2)
library(yarrr)
#####Parameters
directory <- "C:\\Dario\\PhD\\EEG lab\\oscilloscope outputs\\All OSC files\\part 1s"
upper_val <- -0.25
lower_val <- -0.55

######file list
list <- list.files(path = directory)
nFiles <- NROW(list)

######loop
lapFile = 0
col_count = 0#for collapsing datafiles
stat_count = 0#for collapsing statistics summaries

for (lapFile in 1:nFiles) {
  fileName <- list[lapFile]#capturing filename
  file_dir <- paste(directory, "\\", fileName, sep="")#file path
  osc_data = read.csv(file_dir,#open file
                      header = TRUE)#keep first row as header
  ###GGplot of the raw data
  osc_plot <- ggplot(data=osc_data, aes(x=Time, y=Volt)) +
    geom_line()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
  
  osc_plot+xlim(41, 42)#if you want to see the whole curve, remove xlim
  
  ###data transformation: simplifying all the unstables upper and lower
  ###values based on the upper_val and lower_val on the paremeter section
  Osc_NResp <- length(osc_data$Volt)#checking number of data points
  
  nresp = 0#starting point
  
  for (nresp in 1:Osc_NResp) {
    volt <- osc_data$Volt[nresp]#captures volt value
    if(volt > upper_val){
      osc_data$Clean[nresp] <- upper_val#transforms upper value to upper_val
      osc_data$Filename[nresp] <- fileName#adds the corresponding filename to each datapoint
    } else if (volt < lower_val) {
      osc_data$Clean[nresp] <- lower_val#transforms lower value to lower_val
      osc_data$Filename[nresp] <- fileName
    } else {
      osc_data$Clean[nresp] <- volt#keeps the original value
      osc_data$Filename[nresp] <- fileName
    }
  }
  
  ###new ggplot that should have a "smoother" line than the previous one
  osc_plot <- ggplot(data=osc_data, aes(x=Time, y=Clean)) +
    geom_line()+
    scale_color_brewer(palette="Paired")+
    theme_minimal()
  
  osc_plot+xlim(41, 42)#if you want to see the whole curve, remove xlim
  
  ###Estimating distances in time
  ###First: labeling when the signal increases or decreases
  ###logic: if value -1D is BIGGER from value D, then is DECREASE;
  ###if value -1D is SMALLER from value D, then is INCREASE
  Osc_NResp <- length(osc_data$Clean)#number of data points
  nresp = 0#reset to zero
  
  point_B <- osc_data$Clean[1]#we need this so the first jump to work
  
  for (nresp in 1:Osc_NResp) {
    point_A <- osc_data$Clean[nresp]
    if((point_A < point_B) && (point_A == lower_val)){
      osc_data$Changes[nresp] <- "Decrease"
    } else if ((point_A > point_B) && (point_A == upper_val)) {
      osc_data$Changes[nresp] <- "Increase"
    } else {
      osc_data$Changes[nresp] <- "None"
    }
    point_B <- osc_data$Clean[nresp]
  }
  
  ###second: measuring the distances between each decrease
  Osc_NResp <- length(osc_data$Changes)#number of responses
  
  nresp = 0#reset to zero
  count_D = 0#counter of 
  
  for (nresp in 1:Osc_NResp) {
    alert <- osc_data$Changes[nresp]
    if (alert == "Decrease") {#if the data point is a "decrease" then...
      count_D = count_D + 1
      if (count_D == 1) {#it saves the data point as the first pont of the interval
        time_A = osc_data$Time[nresp]
      } else if (count_D == 2) {#or it saves the data point as the end of the interval
        time_B = osc_data$Time[nresp]
        osc_data$intervals[nresp] = time_B - time_A#then it estimates the duration of the interval
        count_D = 1#reset this to 1, so next "decreases" will jump to the "else if"
        time_A = time_B#current b point will be the next a point of the interval
      }
    } else {
      osc_data$intervals[nresp] = 0#if point is not a decrease, it is save as zero
    }
  }
  
  ###descriptives for the specific file
  #details following the patterns 240-20-012_002-H_200ms-D_0s-64K-2.csv
  osc_data$Rrate <- substring(fileName,1,3)
  osc_data$Freq <- substring(fileName,5,6)
  osc_data$TrialFrame <- substring(fileName,8,10)
  osc_data$OnFrame <- substring(fileName,12,14)
  
  #transforming sting variables into numbers
  osc_data$Rrate <- as.numeric(osc_data$Rrate)
  osc_data$Freq <- as.numeric(osc_data$Freq)
  osc_data$TrialFrame <- as.numeric(osc_data$TrialFrame)
  osc_data$OnFrame <- as.numeric(osc_data$OnFrame)
  
  #removing extra column X if it exists
  ncolumnsosc <- NCOL(osc_data)
  
  if (ncolumnsosc != 11){
    #colName <- colnames(osc_data, do.NULL = FALSE)
    #colName[1]
    osc_data <- osc_data[,-1]
  }
  
  ###collapsing data with previous loops
    if (col_count == 0){
      collapsed_osc <- osc_data
      col_count = 1
    } else if (col_count == 1) {
      collapsed_osc <- rbind(collapsed_osc, osc_data)
    }
  #summary(collapsed_osc)
  #summary(osc_data)
  ###subsetting data to data that only contains interval durations
  osc_data_int <- subset(osc_data, osc_data$intervals > 0)
  
  
  ###creating dataset for statistic summaries
  #creating dataset
  stat_osc <- data.frame(osc_data[1,])
  #creating the statistics
  stat_osc$min <- min(osc_data_int$intervals)
  stat_osc$max <- max(osc_data_int$intervals)
  stat_osc$median <- median(osc_data_int$intervals)
  stat_osc$mean <- mean(osc_data_int$intervals)
  stat_osc$sd <- sd(osc_data_int$intervals)
  #collapsing statistic data with previous loops
  if (stat_count == 0){
    coll_stat_osc <- stat_osc
    stat_count = 1
  } else if (stat_count == 1) {
    coll_stat_osc <- rbind(coll_stat_osc, stat_osc)
  }
}

#####saving data frames
###collapsed overall data
write.csv(collapsed_osc, "collapsed_osc.csv")

###interval overall data
#subsetting
collapsed_osc_int <- subset(collapsed_osc, collapsed_osc$intervals > 0)
#saving
write.csv(collapsed_osc_int, "collapsed_osc_int.csv")

###statistics data
write.csv(coll_stat_osc, "coll_stat_osc.csv")

#####overall pirate plot
###pirate plot
osc_plot <- yarrr::pirateplot(formula = intervals ~ Freq + Rrate,
                              data = collapsed_osc_int,
                              main = "intervals",
                              xlab = "none",
                              ylab = "intervals in time")

writeJPEG(osc_plot)


