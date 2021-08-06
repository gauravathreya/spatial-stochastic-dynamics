#A simple R code to simulate two-dimensional random walk.
#Original code by Vishwesha Guttal, CES, IISc
#written on 5th Aug 2021.
#Modified by Shikhara Bhat, IISER Pune
#Modified on 6th Aug 2021.


#Make a function to retrieve the MSD given params
get_MSD <- function(mean, #Mean of the dist
                    var, #Variance of the normal dist
                    runs, #Number of realizations to average over
                    T, #Timesteps for which to run the simulation
                    animate, #boolean. Do you want an animation of the path?
                    plot=FALSE){ #boolen. Do you want to plot the path taken once simulation is complete?
  
  MSDx <- rep(0,T)
  MSDy <- rep(0,T)
  MSD <- rep(0,T)
  
  for (run in 1:runs){
    
    x<-numeric(T)
    y<-numeric(T)
    for (i in 2:T){
      
      #WLOG, set origin as starting point
      x[1]<-0
      y[1]<-0
      
      #Draw N(0,1) rvs for each coordinate
      #Change it with your favourite random distribution and see how pattern changes!
      randx <- rnorm(1,mean,var)
      randy <- rnorm(1,mean,var)
      #Add that to the current location
      x[i] <- x[i-1] + randx
      y[i] <- y[i-1] + randy
      
      #In case you want individual 1D MSDs
      MSDx[i] <- MSDx[i] + ((x[i]-x[1])**2)/runs
      MSDy[i] <- MSDy[i] + ((y[i]-y[1])**2)/runs
      
      
      MSD[i] <- MSD[i] + ((x[i]-x[1])**2)/runs + ((y[i]-y[1])**2)/runs
      
      if (animate == 1)
      {
        #Visualise line an animation
        plot(x[1:i],y[1:i], type='o', col='black',lwd=2, pch=16)
        points(x[1],y[1],pch=22,col="blue",cex=2,lwd=3)
        points(x[i],y[i],pch=22,col="red",cex=2,lwd=3)
        
        #This command is crucial to observe animation
        Sys.sleep(0.1)
      }
    }
  }
  
  if (plot){
    #Make a plot of the path taken
    plot(x[1:T],y[1:T], type='o', col='black',lwd=2, pch=16)
    points(x[1],y[1],pch=22,col="blue",cex=2,lwd=3)
    points(x[T],y[T],pch=22,col="red",cex=2,lwd=3)
  }

  
  return (MSDx)
}

###############################################################

#Set param values

#Do you want to animate, set to 1 (it will be slow to run)
#If you only want the final plot, set to 0 (fast)
animate <- FALSE

T<-1000 #number of timesteps to simulate

#Set number of realizations to average over
runs = 1000
#Set variances that should go into the simulation
varlist <- seq(5)

###############################################################
#Simulate

df <- data.frame(Time=seq(T))
for (i in 1:length(varlist)){
  MSD <- get_MSD(0,varlist[i],runs,T,animate)
  df <- cbind(df,var=MSD)
  colnames(df)[i+1] <- as.character(varlist[i])
}

###############################################################
# Plot

library(ggplot2)
library(reshape2)


#Reshape into the form ggplot likes
df2 <- melt(df,id.vars='Time',variable.name = 'Standard_Deviation')

#Make the plot
MSD_plot <- ggplot(df2,aes(x=Time,y=value)) + geom_point(aes(color= Standard_Deviation))
MSD_plot <- MSD_plot + xlab('Time') + ylab('MSD') + theme_light()

#aesthetics
MSD_plot <- MSD_plot + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))

MSD_plot

###############################################################

#Calculate slope and plot

#calculate slope
slopelist = numeric(length(varlist))
for (i in 1:length(varlist)){
  slope <- lm(df[2:1000,i+1]~df[2:1000,1])$coefficients[2]
  slopelist[i] <- as.numeric(slope)
}

slope_df <- data.frame(cbind(varlist,slopelist))

#plot it
slope_plot <- ggplot(slope_df,aes(x=varlist,y=slopelist)) + geom_point()
slope_plot <- slope_plot + xlab('Standard Deviation') + ylab('Slope of the \nMSD in \nx direction vs Time line') + theme_light()

#aesthetics
slope_plot <- slope_plot + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))

#Fit a quadratic
slope_plot <- slope_plot + stat_smooth(method='lm',formula=y~x+I(x^2))

slope_plot