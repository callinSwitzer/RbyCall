# plot responses to survey

#direct <- file.choose()
direct <- "/Users/callinswitzer/Dropbox/Harvard/RByCallin/R Course for OEB (Responses) - Form Responses 1.csv"
surv <- read.csv(direct, stringsAsFactors = FALSE)

TIMES <- unlist(strsplit(surv$What.times.would.work.for.you.to.participate., split = ", "))

TimesDF <- data.frame(TimePref = TIMES)
library(ggplot2)


ggplot(TimesDF, aes(x = TimePref)) + 
     geom_bar() + 
     theme_bw() + 
     theme(axis.text.x=element_text(angle=45, hjust = 1))


# set the levels, to reorder the bars

TimesDF$TimePref <- factor(TimesDF$TimePref, levels = names(sort(table(TimesDF$TimePref), 
                               decreasing=TRUE)))
 
library(grid) # for the unit() function
my.theme <- theme_bw() + 
               theme(plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_blank(),
                    axis.text.y = element_text(size=0),
                    axis.ticks.length=unit(-0.25, "cm"),
                    axis.ticks.margin=unit(0.4, "cm"), 
                    legend.title=element_blank(), 
                    legend.position = "none", 
                    legend.justification = c(1, 1),
                    axis.ticks.y = element_blank(),
                    axis.text.x=element_text(angle=45, hjust = 1))


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

# find who can come
WhoCanCome <- grepl(pattern = levels(TimesDF$TimePref)[1], x = surv$What.times.would.work.for.you.to.participate.)
surv$Name.[ WhoCanCome]

# find who cannot come
surv$Name.[!WhoCanCome]

# visualize experience
lvs <- levels(factor(surv$How.would.you.rate.your.experience.with.coding.in.R.))
surv$How.would.you.rate.your.experience.with.coding.in.R. <- factor(surv$How.would.you.rate.your.experience.with.coding.in.R., levels = lvs[c(3,1,4,2)])


ggplot(surv, aes(x = How.would.you.rate.your.experience.with.coding.in.R.)) + 
     geom_bar() + 
     my.theme


phylo <- grepl(pattern = "[Pp]hylo", x = surv$What.are.you.most.excited.to.learn..using.R.)

excite <- surv$What.are.you.most.excited.to.learn..using.R.
#split by space or return
wordsUsed <- (unlist(strsplit(excite, split = " |\n")))


# remove punctuation
wdsCln <- tolower(gsub("[[:punct:]]", replacement = "", x = wordsUsed))

wdsCln <- data.frame(wds = wdsCln[wdsCln != ""])
articles <- c("to", "and", "a", "of", "for", "the", "in", "with", "be", "also", "some", "as", "so", "at", "this")
wdc <- wdsCln$wds[!(wdsCln$wds %in% articles)]

wrdTab <- sort(table(wdc), decreasing = TRUE)[1:25]


wrdDF <- data.frame(BuzzWords = wdc[wdc %in% names(wrdTab)])
wrdDF <- within(wrdDF, 
                BuzzWords <- factor(BuzzWords, levels = names(wrdTab)))
ggplot(wrdDF, aes(x=BuzzWords)) + 
     geom_bar() + 
     my.theme + 
     theme(axis.ticks.x = element_blank()) + 
     scale_y_continuous(expand = c(0,0))
