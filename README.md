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

### Sales
![sale-company](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-company.png)
![sale-company](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-company%202.png)  
Sony has the most sum sales copies(unit is hundred of thousand), while Nintendo and Nintendo and Microsoft rank at 2nd and 3rd seperately. But, in terms of average sale copies(unit is hundred of thousand) per game, Nintendo got the 1st place while Microsoft and Sony got 2nd and 3rd.
![sale-genre](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-genre.png)
![sale-genre2](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-genre%202.png)  
According to total sales, the best popular game genre are _Action_, _Sports_ and _Shooter_, while in average sales only _Platform_ and _Shooter_ surpass 100,000 sale copies per game.  
![sale-year](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-year.png)  
![sale-year2](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-year%202.png)  
Though, no matter release and sales in 21st century presented a properous scene, when it comes to average level 1984-1990 stands out again. So which is the _Golden Age_? 21st century or 1984-1990?

### Chanhes as time goes by
![sale-year-company](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-year-company%202.png)  
The first appearance of the three major companies(i.e. Sony, Nintendo and Microsoft) is in 1983, Nintendo and immediately occupied more than 60% of video game market. Nintendo kept increase of marketshare untile sony joined. Then the video game market is almost divided into two parts--Nintendo and Sony. But as Microsoft joined in 2000, the marketshare gradually is divided into three parts. The previous platform (i.e.in _Others_ group) nearly faded out after 21st century, while only PC is still competing with other three companies. And In recent years, Nintendo is shirinkaging while Sony and Microsoft are expanding.
![sale-year-genre](https://github.com/Guangtufan/Video-Game-Sales/blob/master/plots/sale-year-genre%202.png)

