# Video-Game-Sales
This is a video games sales datasets, recording sales and related information from 1980-2016. Games in this dataset have sale copies more than 100,000
#### import dataset into R and tidy data
>videoGames <- read.csv("videogamesales.csv", stringsAsFactors = F)  
videoGames <- videoGames[videoGames$Global_Sales >= 0.1,] # select games sold more than 100,000 copies  
vg <- videoGames  
vg$Year <- as.numeric(vg$Year)  

### Create variable _Company_, _Market_
1. __Company__
>vg$Company[vg$Platform %in% c('3DS','DS','GB','GBA','GC','N64','NES','SNES','Wii','WiiU')] <- 'Nintendo'  
vg$Company[vg$Platform == 'PC'] <- 'PC'  
vg$Company[vg$Platform %in% c('PS','PS2','PS3','PS4','PSP','PSV')] <- 'Sony'  
vg$Company[vg$Platform %in% c('X360', 'XB', 'XOne')] <- 'Microsoft'  
vg$Company[vg$Platform %in% c(2600, 'DC','GEN','NG','TG16','WS','SAT','SCD')] <- 'Others'   

2. __Market__
>library(reshape2)  
vg_sales <- vg[,c(1,4,5,7,8,9,10,12)]  
vg_m <- melt(vg_sales, id=c('Rank', 'Year', 'Genre', 'Company'))  
names(vg_m)[5] <- 'Market'  
names(vg_m)[6] <- 'Sales'  
vg_m$Market <- as.character(vg_m$Market)  
vg_m$Market[vg_m$Market == 'NA_Sales'] <- 'North America'  
vg_m$Market[vg_m$Market == 'EU_Sales'] <- 'Europe'  
vg_m$Market[vg_m$Market == 'JP_Sales'] <- 'Japan'  
vg_m$Market[vg_m$Market == 'Other_Sales'] <- 'Other'  

### Release
![Release of Years](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/release%20of%20years.png)  
* There are three times dramatic increase of release, 1995-1998, 2000-2002 and 2006-2008 seperately. And release are decreasing in recent years.

![Release of Genres](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/release%20of%20genres.png)  
* Top3 popular game genres are _Action_, _Sports_ and _Misc_   

![Release of Companies](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/release%20of%20companies.png)
* Platforms of Sony and Nintendo have much more games than others
### Rank
Rank of games don't have significantly difference among companies, genres or publishers.
![rank-company](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/rank-company.png)
![rank-genre](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/rank-genre.png)
![rank-publisher](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/rank-publisher.png)  
But games released during 1984-1990 have better rank (i.e. saled more copies) than games released in other years. Golden Age of Video Games ?? (lol)
![rank-year](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/rank-year.png)  
