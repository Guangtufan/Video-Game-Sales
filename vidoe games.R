library(ggplot2)

videoGames <- read.csv("videogamesales.csv", stringsAsFactors = F)
videoGames <- videoGames[videoGames$Global_Sales >= 0.1,] # select games sold more than 100,000 copies
vg <- videoGames
vg$Year <- as.numeric(vg$Year)
describ
# Set companies for each game according platform
vg$Company[vg$Platform %in% c('3DS','DS','GB','GBA','GC','N64','NES','SNES','Wii','WiiU')] <- 'Nintendo'
vg$Company[vg$Platform == 'PC'] <- 'PC'
vg$Company[vg$Platform %in% c('PS','PS2','PS3','PS4','PSP','PSV')] <- 'Sony'
vg$Company[vg$Platform %in% c('X360', 'XB', 'XOne')] <- 'Microsoft'
vg$Company[vg$Platform %in% c(2600, 'DC','GEN','NG','TG16','WS','SAT','SCD')] <- 'Others' 

library(Hmisc)
describe.data.frame(vg) # There are missing values in column Year

# Reshape dataset and creat a variable -- Market
library(reshape2)
vg_sales <- vg[,c(1,4,5,7,8,9,10,12)]
vg_m <- melt(vg_sales, id=c('Rank', 'Year', 'Genre', 'Company'))
names(vg_m)[5] <- 'Market'
names(vg_m)[6] <- 'Sales'
vg_m$Market <- as.character(vg_m$Market)
vg_m$Market[vg_m$Market == 'NA_Sales'] <- 'North America'
vg_m$Market[vg_m$Market == 'EU_Sales'] <- 'Europe'
vg_m$Market[vg_m$Market == 'JP_Sales'] <- 'Japan'
vg_m$Market[vg_m$Market == 'Other_Sales'] <- 'Other'

# Release
p1<-ggplot(vg, aes(x=as.factor(Year))) + geom_bar() + 
  ggtitle('Release of Years') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(label = 'Year') +
  ylab(label = 'Num of Release') 


help(ggsave)

ggplot(vg, aes(x=Genre)) + geom_bar() + 
  ggtitle('Release of Genres') +
  ylab(label = 'Num of Release') 

ggplot(vg, aes(x=Company)) + geom_bar() + 
  ggtitle('Release of Companies') +
  ylab(label = 'Num of Release')


publisher <- as.data.frame(table(vg$Publisher))
names(publisher) <- c('Publisher', 'Release')
publisher <- publisher[order(-publisher$Release),]
publisher <- publisher[20:1,]
list<-as.vector(publisher$Publisher)
publisher$Publisher <- factor(publisher$Publisher, levels=list)
ggplot(publisher, aes(x=Publisher, y=Release)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Release of Publishers(Top 20)') +
  coord_flip() +
  ylab(label = 'Num of Release')

# Rank
ggplot(vg, aes(x=as.factor(Year), y=Rank)) + geom_boxplot(lwd=.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Rank of Games in Each Year') +
  xlab(label = 'Year') + ylab(label = 'Distribution of Rank')

ggplot(vg, aes(x=Genre, y=Rank)) + geom_boxplot(lwd=.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Rank of Games in Each Genre') +
  ylab(label = 'Distribution of Rank')

ggplot(vg, aes(x=Company, y=Rank)) + geom_boxplot(lwd=1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Rank of Games of Each Company') +
  ylab(label = 'Distribution of Rank') 
  
ggplot(vg[vg$Publisher %in% list,], aes(x=Publisher, y=Rank)) + geom_boxplot(lwd=1) +
  ggtitle('Rank of Games of Each Publisher(Top 20)') +
  ylab(label = 'Distribution of Rank') +
  coord_flip()

# Sales 
g_y <- aggregate(vg$Global_Sales, by=list(vg$Year), sum)
names(g_y) <- c('Year', 'SSales')
ggplot(g_y, aes(x=Year, y=SSales)) + geom_bar(stat = 'identity')+
  ggtitle('Sales(SUM) of Years') + ylab(label = 'Copies of Sales')
g_y_m <- aggregate(vg$Global_Sales, by=list(vg$Year), mean)
names(g_y_m) <- c('Year', 'MSales')
g_y_m$fill[g_y_m$MSales > 1] <- 1
g_y_m$fill[g_y_m$MSales < 1] <- 0
ggplot(g_y_m, aes(x=Year, y=MSales, fill=as.factor(fill))) + geom_bar(stat = 'identity')+
  ggtitle('Sales(Mean) of Years') + 
  ylab(label = 'Average Copies of Sales per Game') +
  guides(fill=F)

g_g <- aggregate(vg$Global_Sales, by=list(vg$Genre), sum)
g_g_m <- aggregate(vg$Global_Sales, by=list(vg$Genre), mean)
names(g_g) <- c('Genre', 'Sales')
names(g_g_m) <- c('Genre', 'MSales')
g_g_m$fill[g_g_m$MSales > 1] <- 1
g_g_m$fill[g_g_m$MSales < 1] <- 0
ggplot(g_g, aes(x=Genre, y=Sales)) + geom_bar(stat = 'identity')+
  ggtitle('Sales(SUM) of Genres') + 
  ylab(label = 'Copies of Sales')
ggplot(g_g_m, aes(x=Genre, y=MSales, fill=as.factor(fill))) + geom_bar(stat = 'identity')+
  ggtitle('Sales(Mean) of Genres') + 
  ylab(label = 'Average Copies of Sales per Game') +
  guides(fill=F)


g_c <- aggregate(vg$Global_Sales, by=list(vg$Company), sum)
g_c_m <- aggregate(vg$Global_Sales, by=list(vg$Company), mean)
names(g_c) <- c('Company', 'Sales')
names(g_c_m) <- c('Company', 'MSales')
g_c_m$fill[g_c_m$MSales > 1] <- 1
g_c_m$fill[g_c_m$MSales < 1] <- 0
ggplot(g_c, aes(x=Company, y=Sales)) + geom_bar(stat = 'identity')+
  ggtitle('Sales(SUM) of Companies') + 
  ylab(label = 'Copies of Sales')
ggplot(g_c_m, aes(x=Company, y=MSales, fill=as.factor(fill))) + geom_bar(stat = 'identity')+
  ggtitle('Sales(Mean) of Companies') + 
  ylab(label = 'Average Copies of Sales per Game') +
  guides(fill=F)

publisher2 <- aggregate(vg$Global_Sales, by=list(vg$Publisher), sum)
publisher2 <- head(publisher2[order(-publisher2$x),], 20)
names(publisher2) <- c('Publisher', 'Sales')
ggplot(publisher2, aes(x=Publisher, y=Sales)) + geom_bar(stat = 'identity')+
  ggtitle('Sales(SUM) of Publishers(Top 20)') + 
  ylab(label = 'Copies of Sales') +
  coord_flip()
publisher2_m <- aggregate(vg$Global_Sales, by=list(vg$Publisher), mean)
publisher2_m <- head(publisher2_m[order(-publisher2_m$x),], 20)
names(publisher2_m) <- c('Publisher', 'MSales')
publisher2_m$fill2[publisher2_m$Publisher %in% pn] <- "In Top 20 Total Sales"


ggplot(publisher2_m, aes(x=Publisher, y=MSales, fill=as.character(fill2))) + 
  geom_bar(stat = 'identity')+
  ggtitle('Sales(Mean) of Publishers') + 
  ylab(label = 'Average Copies of Sales per Game') +
  guides(fill=F) +
  coord_flip() # Red publishers are botn in top 20 total sales and average sales

pn <- publisher2$Publisher[publisher2$Publisher %in% publisher2_m$Publisher]




# Sales - Year
ggplot(vg, aes(x=Year, y=Global_Sales, fill=Genre)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Paired') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  theme(legend.position=c(0.15,0.7))
ggplot(vg, aes(x=Year, y=Global_Sales, fill=Genre)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Paired') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  ylab(label = 'Proportion of Sales') 
  
ggplot(vg, aes(x=Year, y=Global_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Accent') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  theme(legend.position=c(0.15,0.7))
ggplot(vg, aes(x=Year, y=Global_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  ylab(label = 'Proportion of Sales') 

ggplot(vg, aes(x=Year, y=Global_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Accent') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  theme(legend.position=c(0.15,0.7))
ggplot(vg, aes(x=Year, y=Global_Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Accent') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  ylab(label = 'Proportion of Sales')

ggplot(vg_m, aes(x=Year, y=Sales, fill=Market)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Set3') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  theme(legend.position=c(0.15,0.7))
ggplot(vg_m, aes(x=Year, y=Sales, fill=Market)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Set3') +
  scale_x_continuous(breaks = seq(1980,2015,5)) +
  ylab(label = 'Proportion of Sales') 

# marke-sale
ggplot(vg_m, aes(x=Genre, y=Sales, fill=Market)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Set3') +
  ylab(label = 'Proportion of Sales') +
  theme(legend.position=c(0.9,0.9))

ggplot(vg_m, aes(x=Market, y=Sales, fill=Company)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = 'Set3') +
  ylab(label = 'Proportion of Sales') +
  theme(legend.position=c(0.9,0.9))

# year-market-genre-sale
ggplot(vg_m, aes(x=Year, y=Sales, fill=Company)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Set3') +
  facet_grid(Market~.) +
  theme(legend.position=c(0.1,0.9))
