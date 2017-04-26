setwd("/Users/yezhuang/documents/R")
videoGames <- read.csv("videogamesales.csv", stringsAsFactors = F)
videoGames <- videoGames[videoGames$Global_Sales >= 0.1,]
vg <- videoGames
vg$Year <- as.numeric(vg$Year)
vg$Company[vg$Platform %in% c('3DS','DS','GB','GBA','GC','N64','NES','SNES','Wii','WiiU')] <- 'Nintendo'
vg$Company[vg$Platform == 'PC'] <- 'PC'
vg$Company[vg$Platform %in% c('PS','PS2','PS3','PS4','PSP','PSV')] <- 'Sony'
vg$Company[vg$Platform %in% c('X360', 'XB', 'XOne')] <- 'Microsoft'
vg$Company[vg$Platform %in% c(2600, 'DC','GEN','NG','TG16','WS','SAT','SCD')] <- 'Others' 

pub <- as.data.frame(table(vg$Publisher))
pub[order(-pub$Freq),] 


library(Hmisc)
library(ggplot2)
library(reshape2)
describe(videoGames)
# Sales in each Market

vg_sales <- vg[,c(1,4,5,7,8,9,10,12)]
vg_m <- melt(vg_sales, id=c('Rank', 'Year', 'Genre', 'Company'))
names(vg_m)[5] <- 'Market'
names(vg_m)[6] <- 'Sales'
vg_m$Market <- as.character(vg_m$Market)
vg_m$Market[vg_m$Market == 'NA_Sales'] <- 'North America'
vg_m$Market[vg_m$Market == 'EU_Sales'] <- 'Europe'
vg_m$Market[vg_m$Market == 'JP_Sales'] <- 'Japan'
vg_m$Market[vg_m$Market == 'Other_Sales'] <- 'Other'

ggplot(vg_m, aes(x=Year, y=Sales, fill= Market)) + 
  geom_bar(stat = "identity", position = 'fill') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vg_m, aes(x=Genre, y=Sales, fill= Market)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vg_g_y <- aggregate(vg$Global_Sales, by=list(vg$Year, vg$Company), sum)
names(vg_g_y) <- c('Year', 'Company', 'GlobalSales')
ggplot(vg_g_y, aes(x=Year, y=GlobalSales, fill=Company)) + 
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette="Set2")







# compant
vg$Platform
ggplot(vg, aes(x = Company)) + geom_bar(fill = "#85add6")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
# year
vg[vg$Year == 2020, 'Year'] <- 2009
vg[vg$Year == "N/A", 'Year'] <- NA
vg$Year <- as.numeric(vg$Year)



# Genere
ggplot(vg, aes(x=Genre)) + geom_bar(fill = "#85add6") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title = 'Numbers of games in each genre')

Genre_Global <- aggregate(vg$Global_Sales, by = list(vg$Genre), sum)
names(Genre_Global) <- c('Genre', 'Sales')
ggplot(Genre_Global, aes(x=Genre, y=Sales)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Sales
library(psych)
corr.test(vg_cor)

vg$Platform <- as.factor(vg$Platform)
levels(vg$Platform)



head(vg, 10)
head(vg_m, 10)
# Rank
head(vg[order(-vg$NA_Sales),],1)
head(vg[order(-vg$EU_Sales),],1)
head(vg[order(-vg$JP_Sales),],1)
head(vg[order(-vg$Other_Sales),],1)
# Rank-Genre
ggplot(vg, aes(x=Genre, y=Rank)) + geom_boxplot(lwd=1)
# Rank-Year
ggplot(vg, aes(x=as.factor(Year), y=Rank)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Rank-Company
ggplot(vg,aes(x=as.factor(Company), y=Rank, col=Company)) + 
  geom_boxplot(lwd=1)
# Rank-Year-Company
ggplot(vg, aes(x=as.factor(Year), y=Rank, fill=Company)) + 
  geom_bar(stat = 'identity', position = 'fill') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = 'Accent')
  
# Year 
ggplot(vg, aes(x=as.factor(Year))) + geom_bar(fill='#34699d') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # numbers
year_global<-aggregate(vg$Global_Sales, by=list(vg$Year), sum)
names(year_global) <- c('Year', 'Sales')
# Year-Sales
ggplot(year_global, aes(x=Year,y=Sales)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(1980, 2016, 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Year-Company
ggplot(vg, aes(x=Year, fill=Company)) + geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Accent")
c_s_y <- aggregate(vg$Global_Sales, by=list(vg$Year, vg$Company), sum)
names(c_s_y) <- c('Year', 'Company', 'Sales')
ggplot(c_s_y, aes(x=Year, y=Sales, col=Company)) +
  geom_line(lwd = 1) +
  scale_color_brewer(palette = "Accent")
# Year-Sales-Company
y_s_g<-aggregate(vg$Global_Sales, by=list(vg$Year, vg$Company), FUN=sum)
names(y_s_g) <- c('Year', 'Company', 'Sales')
ggplot(y_s_g, aes(x=Year, y=Sales, col=Company)) +
  geom_line(lwd=1) +
  scale_colour_brewer(palette = "Accent")


ggplot(vg, aes(x=Year, y=Global_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent')
ggplot(vg, aes(x=Year, y=NA_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Accent')
ggplot(vg, aes(x=Year, y=EU_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent')
ggplot(vg, aes(x=Year, y=JP_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent')
ggplot(vg, aes(x=Year, y=Other_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent') 

ggplot(vg_m, aes(x=Year, y=Sales, fill=Market)) +
  geom_bar(stat='identity', position = 'stack')

# Year-Sales(mean)
copy_per_game <- aggregate(vg$Global_Sales, by=list(vg$Year), mean)
names(copy_per_game) <- c('Year', 'MeanSales')
copy_per_game$Level[copy_per_game$MeanSales >=1] <- 'greater than 1m'
copy_per_game$Level[copy_per_game$MeanSales < 1] <- 'less than 1m'
ggplot(copy_per_game, aes(x=Year, y=MeanSales, fill=Level)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1980, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

# Year-Market





