# Callin Switzer
# plot responses to survey

#direct <- file.choose() # handy way to change directory
direct <- "/Users/callinswitzer/Dropbox/Harvard/RByCallin/R Course for OEB (Responses) - Form Responses 1.csv"
surv <- read.csv(direct, stringsAsFactors = FALSE)

# how many people responded
nrow(surv)

# separate the possible times into a single vector
TIMES <- unlist(strsplit(surv$What.times.would.work.for.you.to.participate., split = ", "))

# make it a data frame, so we can plot it with ggplot
TimesDF <- data.frame(TimePref = TIMES)
library(ggplot2) # load library

# plot it
aa <- ggplot(TimesDF, aes(x = TimePref)) + 
     geom_bar() + 
     theme_bw() + 
     theme(axis.text.x=element_text(angle=45, hjust = 1))
aa

# set the levels, to reorder the bars
TimesDF$TimePref <- factor(TimesDF$TimePref, levels = names(sort(table(TimesDF$TimePref), 
                               decreasing=TRUE)))

# define a custom theme
library(grid) # for the unit() function
my.theme <- theme_bw() + 
               theme(plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_blank(),
                    axis.text.y = element_text(size=10),
                    axis.ticks.length=unit(-0.15, "cm"),
                    axis.ticks.margin=unit(0.4, "cm"), 
                    legend.title=element_blank(), 
                    legend.position = "none", 
                    legend.justification = c(1, 1),
                    axis.ticks.y = element_line(),
                    axis.text.x=element_text(angle=45, hjust = 1), 
                    axis.ticks.x = element_blank()) 

plot1 <- ggplot(TimesDF, aes(x = TimePref)) + 
     geom_bar(alpha = 0.5, aes(fill = TimePref))  
     
plot1
     
     
plot1 +  my.theme 
     
plot1 + my.theme + labs(x = "Time Preference", y = "Count")
     
plot1 + my.theme + labs(x = "Time Preference", y = "Count") + scale_fill_brewer(palette="OrRd")
     

plot2 <- ggplot(TimesDF, aes(x = TimePref)) + 
     geom_bar(aes(fill = grepl(pattern = "M", x = TimesDF$TimePref))) + 
     my.theme
plot2 + scale_fill_manual(values = c("pink", "palegoldenrod"))

# colors() find color names


plot2 + scale_fill_manual(values = c("335589", "443344"))

# find who can come on 2nd most popular day (Tuesday)
WhoCanCome <- grepl(pattern = levels(TimesDF$TimePref)[2], x = surv$What.times.would.work.for.you.to.participate.)
surv$Name.[ WhoCanCome]

# find who cannot come
surv$Name.[!WhoCanCome]

# visualize experience
lvs <- levels(factor(surv$How.would.you.rate.your.experience.with.coding.in.R.))
surv$How.would.you.rate.your.experience.with.coding.in.R. <- factor(surv$How.would.you.rate.your.experience.with.coding.in.R., levels = lvs[c(3,1,4,2)])


ggplot(surv, aes(x = How.would.you.rate.your.experience.with.coding.in.R.)) + 
     geom_bar() + 
     my.theme + 
     scale_y_continuous(expand = c(0,0))


phylo <- grepl(pattern = "[Pp]hylo", x = surv$What.are.you.most.excited.to.learn..using.R.)

excite <- surv$What.are.you.most.excited.to.learn..using.R.
#split by space or return
wordsUsed <- (unlist(strsplit(excite, split = " |\n")))


# remove punctuation 
wdsCln <- tolower(gsub("[[:punct:]]", replacement = "", x = wordsUsed))

wdsCln <- data.frame(wds = wdsCln[wdsCln != ""])
articles <- c("to", "and", "a", "of", "for", "the", "in", "with", "be", "also", "some", "as", "so", "at", "although", "this", "all", 'it', "but", "is", "i", "interested", "very", "that", "would", "am", "really", "about")
wdc <- wdsCln$wds[!(wdsCln$wds %in% articles)]

wrdTab <- sort(table(wdc), decreasing = TRUE)[1:25]


wrdDF <- data.frame(BuzzWords = wdc[wdc %in% names(wrdTab)])
wrdDF <- within(wrdDF, 
                BuzzWords <- factor(BuzzWords, levels = names(wrdTab)))
LearnPlot <- ggplot(wrdDF, aes(x=BuzzWords)) + 
     geom_bar(alpha = 0.9) + 
     my.theme + 
     theme(axis.ticks.x = element_blank()) + 
     scale_y_continuous(expand = c(0,0)) + 
     labs(x = "Words used to describe interest", y = "Frequency")
     
LearnPlot



# library(maps)
# map("usa")


# loading the required packages
library(ggmap)
library(ggplot2)


dd <- "/Users/callinswitzer/Dropbox/BeeCourse/labelInfo.csv"
labs <- read.table(dd, stringsAsFactors = FALSE, header = T, sep = ",")
latsLong <- strsplit(labs$Coordinates, split = ", ")


lats <- as.numeric(sapply(1:length(latsLong), FUN = function(x) latsLong[[x]][1]))
longs <- as.numeric(sapply(1:length(latsLong), FUN = function(x) latsLong[[x]][2]))

df <- as.data.frame(cbind(longs,lats))

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$longs), lat = mean(df$lats)), zoom = 6,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
     geom_point(data = df, aes(x = longs, y = lats, alpha = 0.8, shape = 20)) +
     guides(fill=FALSE, alpha=FALSE, size=FALSE)


data2 <- aggregate(df$lats,by=list(y=df$lats,x=df$longs),length)
data2
names(data2)[3] <- "count"

# xtabs(~lats, df)
# 
# reshape(data2, varying = "count", direction = "long")
# library(reshape2)
# dat <- lapply(1:nrow(data2), FUN = function(i) data.frame(long = rep(data2$x[i], data2[i, "count"]), lat = rep(data2$y[i], data2[i, "count"])))
# 
# dat[[2]]
# library(plyr)
# ldply(dat, rbind)




m2 <- get_map(location = c(lon = mean(data2$x), lat = mean(data2$y)), 
              zoom = 8,
              maptype = "terrain", scale = 2)
ggmap(m2) +
 geom_point(aes(x=data2$x,y=data2$y, size = log(data2$count)), shape = 19, alpha = 0.87, color = "red") + 
     guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
     scale_size(range = c(4, 10))

