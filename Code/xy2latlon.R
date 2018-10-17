library(cowplot)
# Define origin in lat/lon
lat <- 32.63146
lon <- -117.24725
brg <- 45
label <- "origin"
base <- data.frame(lat,lon,brg,label)
# Read obs observations
obs <- read.csv("xy_example.csv")
row.names(obs) <- NULL

# Function for converting x/y points to lat/lon
xy2latlon <- function(x,y,lat,lon){
  require(ggplot2);require(swfscMisc)
  # Calculate shift in X direction
  # X is positive to the East
  dest1 <- destination(lat,lon,90,x/1000,units="km")
  df1 <- data.frame(dest1[grep("lat",names(dest1))],dest1[grep("lon",names(dest1))])
  names(df1) <- c("lat","lon")  
  # Calculate shift in Y direction  
  # Y is positive to the North
  dest2 <- destination(df1$lat,df1$lon,0,y/1000,units="km")
  df2 <- data.frame(dest2[grep("lat",names(dest2))],dest2[grep("lon",names(dest2))])
  names(df2) <- c("lat","lon")
  # Remove row names
  row.names(df2) <- NULL
  # Add sequence for text labels
  df2$label <- seq(1,nrow(df2),1)
  # add original x/y data to data frame
  df2$x <- x
  df2$y <- y
  return(df2)
}
# Calculate new locations
new.df <- xy2latlon(obs$x,obs$y,base$lat,base$lon)


# Plot original (x/y) and new (lat/lon) data
orig.plot <- ggplot(new.df,aes(x,y)) + geom_text(aes(label=label))
new.plot <- ggplot(new.df,aes(lon,lat)) + geom_text(aes(label=label),colour="red") + geom_text(data=base,aes(lon,lat,label=label))
# Arrange on the same plot
plot_grid(orig.plot,new.plot,nrow = 1)

