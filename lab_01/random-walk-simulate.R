#A simple R code to simulate two-dimensional random walk.
#by Vishwesha Guttal, CES, IISc
#written on 5th Aug 2021.

#Do you want to animate, set to 1 (it will be slow to run)
#If you only want the final plot, set to 0 (fast)
animate <- 1

T<-1000 #number of timesteps to simulate

#create vectors to store values of coordinates
x<-numeric(T)
y<-numeric(T)

#Without loss of generality, assign the initial location to origin.
x[1]<-0
y[1]<-0

#Simulate

for (i in 2:T){
  
  #Draw a uniform random numbers between -1 and 1 for each coordinate
  #Change it with your favourite random distribution and see how pattern changes!
  randx <- runif(1,-1,1)
  randy <- runif(1,-1,1)
  
  #Add that to the current location
  x[i] <- x[i-1] + randx
  y[i] <- y[i-1] + randy
  
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

plot(x[1:T],y[1:T], type='o', col='black',lwd=2, pch=16)
points(x[1],y[1],pch=22,col="blue",cex=2,lwd=3)
points(x[T],y[T],pch=22,col="red",cex=2,lwd=3)