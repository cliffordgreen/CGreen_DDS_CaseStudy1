---
title: "GDP & EDU World Bank Data Analysis"
output: html_document
---

###Introduction
+ The following code compares two datasets provided by the world bank. The first is GDP data for 190 ranked countries. I used this data to see how individual countries are ranked against other countries throughout the world. The second is Educational data for countries around the world, it also splits countries in Income groups, again after I merged the two files, I was able to extract useful information and compare every countries GDP to its assigned income group per your request.  Using the two data sets I discovers the 13th country with the lowest GDP, I calculated the average GDP for "High income: OECD" and "High income: nonOECD”, I plotted the data by income group so it could be obverse visually, and I split up the countries into quantiles based on GDP ranking and then found the number of countries in each quantile versus income group. Through all this we can better understand the data.


####First I downloaded the two files needed for the dataset 

```r
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv ", destfile="gdp_data.csv")#downloads the first file and give it the name gdp_data.csv

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv ", destfile="edu_data.csv")#downloads the second file and names it edu_data.csv 
```
Next I read in the two CSV files


```r
EDU <- read.csv("edu_data.csv")#creating a data set from a csv file
GDP <- read.csv("gdp_data.csv") 
```

Tidy up the data a bit, I added column names to the GDP data and removed empty rows from important columns


```r
colnames(GDP)<- c("CountryCode", "ranking", "", "Long.Name", "GDP")#adding header names
GDP <- GDP[!(GDP$CountryCode==""),]#The GDP dataset is set as the GDP data set without empty values in Country Code column
EDU <- EDU[!(EDU$CountryCode==""),]#The EDU dataset is set as the GDP data set without empty values in Country Code column
```

###1. Match the data based on the country shortcode. How many of the IDs match? 


```r
Matches <- length(intersect(GDP$CountryCode, EDU$CountryCode))
MergeWithout <- merge(EDU, GDP, by= 1)
```

```
## Warning in merge.data.frame(EDU, GDP, by = 1): column names 'NA', 'NA',
## 'NA', 'NA' are duplicated in the result
```

```r
MergeEDU <- merge(EDU, GDP, by= 1, all=T)
```

```
## Warning in merge.data.frame(EDU, GDP, by = 1, all = T): column names 'NA',
## 'NA', 'NA', 'NA' are duplicated in the result
```

```r
length(MergeWithout$CountryCode)
```

```
## [1] 224
```

Using merge I was able to test how many values matched the Country Code. This value is called MergeWithout. By using the length function I can see that there are 224 values that match between the GDP and EDU datasets


###2. Sort the data frame in ascending order by GDP rank (so United States is last). What is the 13th country in the resulting data frame?


```r
MergeEDU$GDP <- as.numeric(gsub(",","",MergeEDU$GDP))#remove commas and make the numbers numeric
```

```
## Warning: NAs introduced by coercion
```

```r
GDP_Low <- MergeEDU[order(MergeEDU$GDP),]#reorder the dataset set by lowest gdp to highest
GDP_Low[13, "Long.Name.x"]
```

```
## [1] St. Kitts and Nevis
## 234 Levels: American Samoa Antigua and Barbuda ... World
```

###3. What are the average GDP rankings for the "High income: OECD" and "High income: nonOECD" groups? 

```r
subsetHigh <- subset(MergeEDU, Income.Group == "High income: OECD")#create a subset of the High Income: OECD group
subsetHighNon <- subset(MergeEDU, Income.Group == "High income: nonOECD")#create a subset of the High Income: nonOECD

O <- as.numeric(subsetHigh$ranking)#list of ranking from subsethigh as numeric
nonO <- as.numeric(subsetHighNon$ranking)#numeric list of subsetnon

mean(nonO)#take the means of both lists
```

```
## [1] 58.64865
```

```r
mean(O)
```

```
## [1] 110.0667
```


###4.Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.


```r
library(ggplot2)#load ggplot
noGDP <- MergeEDU[complete.cases(MergeEDU[,35]),]
OnlyIncome <- noGDP[!(noGDP$Income.Group==""),]#removes blank values from GDP colum
ggplot(OnlyIncome, aes(x=Long.Name.x, y=GDP, colour=OnlyIncome$Income.Group)) + geom_point()#plot long name vs GDP and then color each income group
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

###5. Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?

```r
OnlyGDP <- noGDP[!(noGDP$ranking==""),]#Remove blanks from ranking
OnlyGDP$Quartiles <- with(OnlyGDP, cut(GDP, breaks = quantile(GDP, probs = seq(0,1, by=.2), na.rm = TRUE), include.lowest=TRUE))#cuts into quatile groups

OnlyGDP$Quartiles <- as.numeric(OnlyGDP$Quartiles)#assigns numeric values to quartiles
tablecomp <- with(OnlyGDP, table(Quartiles, Income.Group))#creats table comparing quartiles v incomegroup

tablecomp[5 ,5]#refrences low middle income and the top quartile
```

```
## [1] 5
```

###Conclusion
+ I found that the country with the 13th lowest GDP out of the 190 countries ranked was St. Kitts and Nevis. The average GDP ranking for High income: OECD was 58.6 and High income: nonOECD 110. This makes sense because the Organization for Economic Co-operation and Development was founded to stimulated ecumenic progress and trade ultimately increasing the participation countries gross domestic products. If you look at the plot of GDP vs country, you can see that there are a couple outliers in the High Income OECD group that throw off the plot a bit but the conclusion can still be drawn that the majority of countries with larger GDPs fall in the High Income GDP group. This is a similar finding as that of the average GDP ranking we did previously. An interesting observation from this plot is that there seems to also be a couple countries from the lower middle income group that have very large GDP. To determine how many countries from the lower middle income group fall into the upper 20% when ranked by GDP i created quantiles and then put them in a table versus income groups. By doing this I was able to see that 5 countries in the lower middle income group fall in the top 20% for GDP.
