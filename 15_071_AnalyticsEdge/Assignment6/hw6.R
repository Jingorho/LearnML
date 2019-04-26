setwd("/Users/yukako/WorkSpace/LearnML/15_071_AnalyticsEdge/data/")
wms <- read.csv("WeeklyMoisturizerSales.csv")
head(wms)
str(wms)

plot(wms$Date, wms$MoisturizerSales)
plot(wms$Date, wms$GoogleTrendVolumeEczema)

wms$pMoisturizerSales[2:nrow(wms)] <- wms$MoisturizerSales[1:(nrow(wms)-1)]
wms$pGoogleTrendVolumeEczema[2:nrow(wms)] <- wms$GoogleTrendVolumeEczema[1:(nrow(wms)-1)]

plot(wms$MoisturizerSales, wms$pMoisturizerSales)
data.frame(m=wms$MoisturizerSales, p=wms$pMoisturizerSales)


reviews = read.csv("airbnb-small.csv", stringsAsFactors = F)
head(reviews)
nchar(reviews$comments)
head(reviews$comments)
reviews$comments[2076]
# Error in nchar(Airbnb$comments) : invalid multibyte string, element 2076
