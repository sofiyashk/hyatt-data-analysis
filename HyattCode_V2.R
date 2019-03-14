# ******************************************************************************************************************************
#                                            Installing Packages
# ******************************************************************************************************************************
install.packages("sqldf")
library(sqldf)

install.packages("zipcode")
library(zipcode)

install.packages("ggmap")
library(ggmap)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("modeest")
library(modeest)

install.packages("reshape2")
library(reshape2)

install.packages("ggplot2")
library(ggplot2)

EnsurePackage <- function(x) {
  x <- as.character(x)
  
  if(!require(x,character.only = T)){
    install.packages(pkgs = x,repos = "http://cran.r-project.org")
    require(x,character.only = T)}
}

# ********************************************************************************************************************************
#                                            Appending multiple data files into one file
# ********************************************************************************************************************************

path <- "C:/Users/sofia/OneDrive/Desktop/687 Course Work/687 Project Work/Data files/IST687-data/IST687-data"
files <- list.files(path = "C:/Users/sofia/OneDrive/Desktop/687 Course Work/687 Project Work/Data files/IST687-data/IST687-data", pattern = "*.csv")
Amenity_DF <- NULL

#reading each file within the range and append them to create one file
for (i in 1:length(files)){
  
  #read the file
  currentFile = read.csv(paste(path,files[i], sep = "/"))
  
  # Selecting columns for Amenities
  AmenityData <- subset(currentFile[,c(9,28,199:227,232,137:145)])
  AmenityCleanData <- na.omit(AmenityData, cols= AmenityData$Likelihood_Recommend_H, invert = FALSE)
  #Append the current file
  Amenity_DF = rbind(Amenity_DF, AmenityCleanData) 
  
}

Location_DF <- NULL

#reading each file within the range and append them to create one file
for (i in 1:length(files)){
  
  #read the file
  currentFile = read.csv(paste(path,files[i], sep = "/"))
  
  # Selecting columns for Amenities
  LocationData <- subset(currentFile[,c(9,46,54:56,65,161:164,166:178,184,182,232,137:145)])
  LocationCleanData <- na.omit(LocationData, cols= LocationData$Likelihood_Recommend_H, invert = FALSE)
  #Append the current file
  Location_DF = rbind(Location_DF, LocationCleanData) 
  
}

Final_DF <- NULL

for (i in 1:length(files)){
  
  #read the file
  currentFile = read.csv(paste(path,files[i], sep = "/"))
  
  #Selecting columns for Location
  FinalData <- subset(currentFile[,c(2 ,9 ,12 ,13 ,21:24 ,32 ,38 ,40 ,41 ,46 ,54:56 ,59 ,64:66 ,75:78 ,83 ,89 ,110 ,126 ,127 ,131:156 ,161:164 ,166:178 ,182 ,184 ,199:227 ,231 ,232)])
  FinalCleanData <- na.omit(FinalData, cols= FinalData$Likelihood_Recommend_H, invert = FALSE)
  #Append the current file
  Final_DF = rbind(Final_DF, FinalCleanData)
  cat("Upload of file no ", i, "done")
 
}

# Subsetting observations for only USA entries
FinalUSA_DF <- sqldf("select * from Final_DF where Country_PL == 'United States'")


# ******************************************************************************************************************************
#                                         Generating Plain USA map
# ******************************************************************************************************************************


# Reading in HyattBind dataset and storing it in a different variable
LocationData <- Hyatt_Bind


# Creating state table
statemap<-data.frame(state.abb)
statemap$Lon<-state.center$x
statemap$Lat<-state.center$y
colnames(statemap)<-c("StateAbbrev","Lon","Lat")
statemap<-statemap[!(statemap$StateAbbrev=="HI"),]
statemap<-statemap[!(statemap$StateAbbrev=="AK"),]
statemap<-statemap[!(statemap$StateAbbrev=="DC"),]

# Creating the map for USA
us <- map_data("state")
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
dummyDF<-dummyDF[!(dummyDF$state=="hawaii"),]
dummyDF<-dummyDF[!(dummyDF$state=="alaska"),]
EnsurePackage("ggplot2")
EnsurePackage("ggmap")
EnsurePackage("ggplot")
map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple + geom_map(map = us, fill="white",color="black")+ expand_limits(x = us$long, y = us$lat)+ coord_map() + ggtitle("basic map of USA")
map.simple <- map.simple + geom_text(aes(x=statemap$Lon, y=statemap$Lat, label=statemap$StateAbbrev), size=2)
map.simple

# ******************************************************************************************************************************
#                                           Zipcode with median likelihood to recommend
# ******************************************************************************************************************************

USAZipCodeMap = LocationData[c(76,36,85)]
EnsurePackage("zipcode")
data(zipcode)
colnames(USAZipCodeMap)<-c("Postal_Code","LTR","Hotel_Brand")
USAZipCodeMap<-merge(USAZipCodeMap, zipcode, by.x='Postal_Code', by.y='zip')
USAZipCodeMap <- sqldf("select * from USAZipCodeMap where state != 'HI'")

map.zip <- map.simple + geom_point(data=USAZipCodeMap,aes(x=USAZipCodeMap$longitude,y=USAZipCodeMap$latitude,color=USAZipCodeMap$LTR))+expand_limits(x=USAZipCodeMap$longitude,y=USAZipCodeMap$latitude)+coord_cartesian(xlim = c(-125,-65), ylim = c(50,20))+coord_map()+scale_color_gradient2(midpoint=5,low="red",mid="yellow",high="blue")+ggtitle("Zipcode with Median Likelihood to Recommend")
map.zip


# *********************************************************************************************************************************
#                                    Analysis on % Dectrators and % Promoters accross states
# *********************************************************************************************************************************

USASummary <- sqldf("select State_PL,NPS_Type,count(NPS_Type) as Count from LocationData group by 1,2")

# Reshaping the data table
EnsurePackage("reshape2")
USASummary_wide <- dcast(USASummary, State_PL ~ NPS_Type, value.var="Count")
USASummary_wide <- USASummary_wide[-1,]
rownames(USASummary_wide) <- NULL
# Calculating the detractor and promoter percentages according to the state

for (i in 1:nrow(USASummary_wide)) {
  
  USASummary_wide$Total[i] <- sum(USASummary_wide[i,2:4])
  USASummary_wide$Dect_Per[i] <- ((USASummary_wide[i,2])/(USASummary_wide[i,5])*100)
  USASummary_wide$Prom_Per[i] <- (USASummary_wide[i,4]/USASummary_wide[i,5])*100
}

# Creating the US map for % dectractors state wise

USASummary_wide$statelower <- tolower(USASummary_wide$State_PL)
USASummary_wide <- USASummary_wide[,2:8]
USASummary_wide$StateAbb <- statemap$StateAbbrev

# Creating the simple map first
map.simple1 <- ggplot(USASummary_wide, aes(map_id = statelower))
map.popColor <- map.simple1 + geom_map(map = us, fill="white",color="black")+ expand_limits(x = us$long, y = us$lat)+ coord_map() 
map.popColor
# Creating the heat map for detractor percentage
map.popColor <- map.popColor+geom_map(map = us, aes(fill=USASummary_wide$Dect_Per)) + expand_limits(x = us$long, y = us$lat)+ coord_map() + scale_fill_gradient(low = "white", high = "red", guide = "colorbar")+ ggtitle("Dectractor Percentage accross USA")+ theme(plot.title= element_text(hjust=0.5))
map.popColor
# Putting state  names on the map
map.popColor <- map.popColor + geom_text(aes(x=statemap$Lon, y=statemap$Lat, label=statemap$StateAbbrev), size=2)
map.popColor


# Creating the US map for % promoters state wise

# Creating the simple map first
map.simple2 <- ggplot(USASummary_wide, aes(map_id = statelower))
map.popColor2 <- map.simple2 + geom_map(map = us, fill="white",color="black")+ expand_limits(x = us$long, y = us$lat)+ coord_map() 
map.popColor2
# Creating the heat map for promoter percentage
map.popColor2 <- map.popColor2+geom_map(map = us, aes(fill=USASummary_wide$Prom_Per)) + expand_limits(x = us$long, y = us$lat)+ coord_map() + scale_fill_gradient(low = "white", high = "dark green", guide = "colorbar")+ ggtitle("Promoter Percentage accross USA")+ theme(plot.title= element_text(hjust=0.5))
map.popColor2
# Putting state  names on the map
map.popColor2 <- map.popColor2 + geom_text(aes(x=statemap$Lon, y=statemap$Lat, label=statemap$StateAbbrev), size=2)
map.popColor2



# **********************************************************************************************************************************
# **********************************************************************************************************************************

#map of CALIFORNIA
CALState <- map_data("state", region = "california")
map.cal<- ggplot() + geom_polygon(data = CALState, aes(x =CALState$long, y = CALState$lat, group = group), fill="white", color="black")
map.cal
CALdata <- sqldf("select * from USAZipCodeMap where State =='CA'")

EnsurePackage("RColorBrewer")
myColors <- brewer.pal(9,"Set1")
names(myColors) <- levels(CALdata$Hotel_Brand)
colScale <- scale_colour_manual(name = "grp",values = myColors)

# Bar plot showing which brand of hyatt hotel is the most famous in Cali

best_brands <- sqldf("select Hotel_Brand, count(Hotel_Brand) as fav_count from CALdata group by 1")

g <- ggplot(best_brands,aes(x=reorder(Hotel_Brand, fav_count),y=fav_count, group=1))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# *************************************** For Plotting something on the CAli map*****************************************

NYdata <- NYdata[,1:7]
NYdata$Postal_Code<- clean.zipcodes(NYdata$Postal_Code)
NYData1 <- sqldf("select Postal_Code, avg(Likelihood_to_recommend) as Median_Likelihood,Hotel_Brand, City_X,State_X, Latitude_X,Longitude_X  from NYdata group by 1,3")

Xcenter <- NYState$long
Ycenter <- NYState$lat
Ylimit <- c(Ycenter-6, Ycenter+6)
Xlimit <- c(Xcenter-6, Xcenter+6)


map.zip <- map.ny + geom_point(aes(x=NYData1$Longitude_X,y=NYData1$Latitude_X,size= NYData1$Median_Likelihood,shape=NYData1$Hotel_Brand,color=NYData1$Postal_Code), data = NYData1)+coord_map()+expand_limits()+ggtitle("Zipcode with Median Likelihood to Recommend")+scale_x_continuous(expand = c(0,0))+scale_y_continuous(expand = c(0,0))
map.zip

# *******************************************************************************************************************************
# *******************************************************************************************************************************

# Bar plot for Hyatt Regency based on Region

region <- LocationData[,c(75,85)]
colnames(region)<-c("Sub_region", "Hotel_Brand")
region_brand <- sqldf("select Sub_region , Hotel_Brand, count(Hotel_Brand) as count from region group by 1,2")
region_brand <- region_brand[-1:-9,]
rownames(region_brand)<-NULL
region_brand2 <- sqldf("select * from region_brand where Hotel_Brand = 'Hyatt Regency'")

g1 <- ggplot(region_brand2,aes(x=Sub_region,y=count,fill=count, group=1))
g1 <- g1 + geom_bar(stat="identity", color="white", fill="blue")
g1 <- g1 +scale_fill_brewer()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Booking with Hyatt Regency across regions")+ theme(plot.title= element_text(hjust=0.5))
g1

# **********************************************************************************************************************************
# **********************************************************************************************************************************

mfv(LocationUSA_DF$Likelihood_Recommend_H)



