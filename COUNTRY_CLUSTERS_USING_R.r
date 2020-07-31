library(XML)

gdp1 <-readLines(url("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)")) 

gdp <- readHTMLTable(gdp1, asText= TRUE)

View(gdp)
worldbank <- gdp[[3]]
View(worldbank)
length(worldbank)
class(worldbank)
gdp_world_bank <- worldbank[5:196,2:3]
View(gdp_world_bank)
colnames(gdp_world_bank)[1] <- "country"
colnames(gdp_world_bank)[2] <- "gdp"
gdp_world_bank$gdp <- as.numeric(gsub(",","",gdp_world_bank$gdp))
gdp_world_bank$country <- as.character(gdp_world_bank$country)
gdp_world_bank$country[3] <- "China"
gdp_world_bank$country[12] <- "Russia"
gdp_world_bank$country[12] <- "South Korea"
gdp_world_bank <- as.data.frame(gdp_world_bank[-2,])
#####################################################################################################


population <- readLines(url("https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"))

polu <- readHTMLTable(population,asText=TRUE)
View(polu)
populationwise <- polu[[4]]
class(populationwise)
View(populationwise)
populationwise <- populationwise[2:234,c(-2,-3,-4)]
View(populationwise)
populationwise <- populationwise[ ,-3]
colnames(populationwise)[1] <- "country"
colnames(populationwise)[2] <- "population"
populationwise$population <- as.numeric(gsub(",","",populationwise$population))
mean(populationwise$population)
populationwise$country <- as.character(populationwise$country)
populationwise$country[1] <- "China"
populationwise$country[22] <- "France"
populationwise$country[25] <- "Tanzania"
populationwise$country[30] <- "Spain"
populationwise$country[33] <- "Ukraine"
populationwise$country[44] <- "Malaysia"
populationwise$country[56] <- "Taiwan"
populationwise$country[55] <- "Australia"
populationwise$country[90] <- "Azerbaijan"
populationwise$country[99] <- "Serbia"
populationwise$country[116] <- "Finland"
populationwise$country[119] <- "Norway"
populationwise$country[121] <- "Palestine"
populationwise$country[131] <- "Moldova"
populationwise$country[132] <- "Georgia"
populationwise$country[157] <- "Mauritius"
populationwise$country[158] <- "Cyprus"
populationwise$country[174] <- "Guadeloupe"
populationwise$country[233] <- "Vatican City"
###################################################################################################

Cindx <- readLines(url("https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate"))
crime <- readHTMLTable(Cindx,asText=TRUE)
View(crime)
crimerate <- crime[[4]]
View(crimerate)
crimerate <- as.data.frame(crimerate[-1,])
crimerate <- crimerate[ ,c(-2,-3,-5,-6,-7)]
colnames(crimerate)[1] <- "country"
colnames(crimerate)[2] <- "crimerate"
crimerate$country <- as.character(crimerate$country)
crimerate$crimerate <- as.numeric(levels(crimerate$crimerate))[crimerate$crimerate]
###################################################################################################

eduindex <- readLines(url("https://en.wikipedia.org/wiki/Education_Index"))
education <- readHTMLTable(eduindex,asText=TRUE)
View(education)
educationindex <- education[[2]]
View(educationindex)
educationindex <- educationindex[-1,c(-1,-4,-5,-6,-7)]
View(educationindex)
colnames(educationindex)
colnames(educationindex)[1] <- "country"
colnames(educationindex)[2] <- "education index"
educationindex$country <- as.character(educationindex$country)
educationindex$`education index` <- as.numeric(levels(educationindex$`education index`))[educationindex$`education index`]
###################################################################################################

hindex <- readLines(url("https://en.wikipedia.org/wiki/World_Health_Organization_ranking_of_health_systems_in_2000"))
healthindex <- readHTMLTable(hindex,asText=TRUE)
View(healthindex)
countrywisehealth <- healthindex[[2]]
View(countrywisehealth)
countrywisehealth <- countrywisehealth[-1,c(-2,-3,-4,-5,-6)]
View(countrywisehealth)
colnames(countrywisehealth)
colnames(countrywisehealth)[1] <- "country"
colnames(countrywisehealth)[2] <- "health index"
countrywisehealth$country <- as.character(countrywisehealth$country)
class(countrywisehealth$`health index`)
countrywisehealth$`health index` <- as.numeric(levels(countrywisehealth$`health index`))[countrywisehealth$`health index`]
countrywisehealth$country[36] <- "China"
###################################################################################################
incomepercapita <- readLines(url("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita"))
ICPC <- readHTMLTable(incomepercapita,asText=TRUE)
View(ICPC)
percapitaincome <- as.data.frame(ICPC[[3]])
View(percapitaincome)
percapitaincome <- percapitaincome[-1,-1]
colnames(percapitaincome)[1] <- "country"
colnames(percapitaincome)[2] <- "GDPperCAPITA"
percapitaincome$country <- as.character(percapitaincome$country)
percapitaincome$GDPperCAPITA <- as.numeric(gsub(",","",percapitaincome$GDPperCAPITA))
###################################################################################################



finaldataframe <- merge(gdp_world_bank,populationwise)
View(finaldataframe)
finaldataframe1 <- merge(finaldataframe,crimerate)
finaldataframe2 <- merge(finaldataframe1,educationindex)
finaldataframe3 <- merge(finaldataframe2,countrywisehealth)
finaldataframe4 <- merge(finaldataframe3,percapitaincome)
View(finaldataframe4)
summary(finaldataframe4)
###################################################################################################

finaldataframe_without_coutry_name <- finaldataframe4[,-1]
finaldataframe_scale <- scale(finaldataframe_without_coutry_name)
mydata <- finaldataframe_scale

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "Number of clusters",ylab = "Within SS")


set.seed(10000)
country_cluster <- kmeans(finaldataframe_scale,6)


countrywise_clustering <- cbind(finaldataframe4,cluster_numbr=country_cluster$cluster)

View(countrywise_clustering)

##################################################################################################
#cluster profiling
mean(countrywise_clustering$gdp)
tapply(countrywise_clustering$gdp,countrywise_clustering$cluster_numbr,mean)
a <- tapply(countrywise_clustering$gdp,countrywise_clustering$cluster_numbr,mean)
barplot(a)

mean(countrywise_clustering$population)
tapply(countrywise_clustering$population,countrywise_clustering$cluster_numbr,mean)
b <- tapply(countrywise_clustering$population,countrywise_clustering$cluster_numbr,mean)
barplot(b)

mean(countrywise_clustering$GDPperCAPITA)
tapply(countrywise_clustering$GDPperCAPITA,countrywise_clustering$cluster_numbr,mean)
f <- tapply(countrywise_clustering$GDPperCAPITA,countrywise_clustering$cluster_numbr,mean)
barplot(f)


mean(countrywise_clustering$`education index`)
tapply(countrywise_clustering$`education index`,countrywise_clustering$cluster_numbr,mean)
c <- tapply(countrywise_clustering$`education index`,countrywise_clustering$cluster_numbr,mean)
barplot(c)

mean(countrywise_clustering$`health index`)
tapply(countrywise_clustering$`health index`,countrywise_clustering$cluster_numbr,mean)
d <- tapply(countrywise_clustering$`health index`,countrywise_clustering$cluster_numbr,mean)
barplot(d)

mean(countrywise_clustering$crimerate)
tapply(countrywise_clustering$crimerate,countrywise_clustering$cluster_numbr,mean)
e <- tapply(countrywise_clustering$crimerate,countrywise_clustering$cluster_numbr,mean)
barplot(e)

