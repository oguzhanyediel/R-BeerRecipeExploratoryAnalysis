used_packages <- c("Amelia", "dplyr", "futile.logger", "GGally", "ggplot2", 
                   "lattice", "readr", "reshape", "reshape2")
lapply(used_packages, require, character.only = TRUE)

recipeData <- read_csv(
  "~/recipeData.csv", 
  na = "N/A")

# row count and length of data
nrow(recipeData)
length(recipeData)

# dimension of data (row count + length)
dim(recipeData)

# general info about data
summary(recipeData)

# the first six and last six row of data
head(recipeData) 
tail(recipeData)

# columns names
colnames(recipeData)

# missing data visualization
missmap(head(recipeData, 500))
missmap(recipeData)
flog.info("It takes the time to appear the plot/graphic/image, 
          please wait a little!")

# Priming Method & Priming Amount
null_priming <- table(is.na(recipeData$PrimingMethod))[2][["TRUE"]]
sprintf("Priming Method is null on %s rows out of %s, so %s percent of the time",
        null_priming, length(recipeData$PrimingMethod),
        round((null_priming/length(recipeData$PrimingMethod))*100, 2))

style_cnt <- recipeData[,c("Style", "PrimingMethod")]
style_cnt$NullPriming <- is.na(recipeData$PrimingMethod)
style_cnt$Count <- as.numeric(1)

style_cnt_grp <- 
  style_cnt %>% 
  group_by(Style) %>% 
  summarise(Count=sum(Count), NullPriming = sum(NullPriming))

style_cnt_grp <- style_cnt_grp[order(-style_cnt_grp$NullPriming),]

if(FALSE){
  height <- as.matrix(style_cnt_grp[1:20,][,c("Count")])
  barplot(height = height,
          beside = TRUE,
          # legend.text = c(style_cnt_grp[1:20,][,c("Style")]),
          legend.text = TRUE,
          main = "Missing Values in PrimingMethod column, per style",
          xlab = "Count of Beer Recipes",
          horiz = TRUE)
}


# Please, look at the graphics in bigger screen :)
ggplot(style_cnt_grp[1:20,]) +
  geom_bar(mapping = aes(x=Style, y=Count, fill=NullPriming),
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(x=Style, y=Count, label = paste("NullP:",NullPriming, sep=" ")), 
            size = 3, position = position_stack(vjust = 0.5)) +
  coord_flip() + 
  theme_bw() +
  labs(title = "Missing Values in PrimingMethod column, per style", 
       x = "Style", 
       y = "Count of Beer Recipes", 
       fill = "Priming Method is null")
# Part of geom_text is optional. 
# If you do not want to see, you can remove from the ggplot code.

ggplot(style_cnt_grp[1:20,], aes(x=Style, y=Count, fill=NullPriming)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~ NullPriming) +
  coord_flip() + 
  theme_bw() +
  labs(title = "Missing Values in PrimingMethod column, per style", 
       x = "Style", y = "Count of Beer Recipes", fill = "Priming Method is null")

# Categorical Features
sapply(colnames(recipeData), function(x) class(recipeData[[x]]))

recipeData[, sapply(recipeData, class) == 'character']
recipeData[sapply(recipeData, function(recipeData) !is.numeric(recipeData))]
# Two sentences have same result

number_of_chr <- table(sapply(colnames(recipeData), 
                              function(x) class(recipeData[[x]])))
flog.info("Recipe Data has %s chr/factor attr", number_of_chr[["character"]])

# That's the seven features:
chrs <- sapply(recipeData, is.character)
chrCols <- names(recipeData[, chrs])
chrCols

unique(recipeData$PrimingAmount)
unique(recipeData$SugarScale)

ggplot(recipeData, aes(SugarScale)) +
  scale_x_discrete(limits=c("Specific Gravity", "Plato")) +
  geom_bar(aes(fill = SugarScale)) +
  theme(legend.position = "none") +
  labs(title = "Frequency table of possible values in SugarScale")

flog.info("SugarScale has %s null values", sum(is.na(recipeData$SugarScale)))

ggplot(recipeData, aes(BrewMethod)) +
  geom_bar(aes(fill = BrewMethod)) +
  theme(legend.position = "none") +
  labs(title = "Frequency table of possible values in BrewMethod")

flog.info("BrewMethod has %s null values", sum(is.na(recipeData$BrewMethod)))
flog.info("PrimingMethod has %s unique values", 
          length(unique(recipeData$PrimingMethod)))
print((unique(recipeData$PrimingMethod))[1:20])

recipeData[sapply(recipeData, function(recipeData) !is.character(recipeData))]
num <- sapply(recipeData, is.numeric)
numCols <- names(recipeData[, num])
numCols

get_sg_from_plato <- function(plato) {
  sg <- 1 + (plato / (258.6 - ( (plato / 258.2) * 227.1) ) )
  return(sg)
}

recipeData$OG_sg <- NA
for(i in 1:length(recipeData$SugarScale)) {
  recipeData$OG_sg[i] <- ifelse(recipeData$SugarScale[i] == 'Plato',
                                get_sg_from_plato(recipeData$OG[i]),
                                recipeData$OG[i]) 
}

recipeData$FG_sg <- NA
for(i in 1:length(recipeData$SugarScale)) {
  recipeData$FG_sg[i] <- ifelse(recipeData$SugarScale[i] == 'Plato',
                                get_sg_from_plato(recipeData$FG[i]),
                                recipeData$FG[i])
}

recipeData$BoilGravity_sg <- NA
for(i in 1:nrow(recipeData)){
  recipeData$BoilGravity_sg[i] <- ifelse(recipeData$SugarScale[i] == 'Plato',
                                         get_sg_from_plato(recipeData$BoilGravity[i]),
                                         recipeData$BoilGravity[i])
}

num_feats_list = recipeData[,c('Size(L)', 'OG_sg', 'FG_sg', 'ABV', 'IBU',
                               'Color', 'BoilSize', 'BoilTime', 'BoilGravity_sg',
                               'Efficiency', 'MashThickness', 'PitchRate', 
                               'PrimaryTemp')]
summary(num_feats_list)
quantile(num_feats_list$BoilSize, c(0.25, 0.5, 0.75), type = 8)
quantile(num_feats_list$BoilTime, c(0.25, 0.5, 0.75), type = 1)
# For detailed info: 
# https://tolstoy.newcastle.edu.au/R/e17/help/att-1067/Quartiles_in_R.pdf

# should be defined a function that will categorize the features automatically
vlow_scale_feats = recipeData[,c('OG_sg', 'FG_sg', 'BoilGravity_sg', 'PitchRate')]
low_scale_feats = recipeData[,c('ABV', 'MashThickness')]
mid_scale_feats = recipeData[,c('Color', 'BoilTime', 'Efficiency', 'PrimaryTemp')]
high_scale_feats = recipeData[,c('IBU', 'Size(L)',  'BoilSize')]

boxplot(vlow_scale_feats, value~variable, 
        col='purple', 
        main='Boxplots of very low scale features in Beer Recipe dataset',
        horizontal = TRUE,
        notch = TRUE)

boxplot(low_scale_feats, value~variable, 
        col=c('blue', 'green'), 
        main='Boxplots of low scale features in Beer Recipe dataset',
        horizontal = TRUE)

boxplot(mid_scale_feats, value~variable, 
        col=c('blue', 'black', 'red', 'purple'), 
        main='Boxplots of medium scale features in Beer Recipe dataset',
        horizontal = TRUE)

boxplot(high_scale_feats, value~variable, 
        main='Boxplots of high scale features in Beer Recipe dataset',
        horizontal = TRUE)

flog.info("There are %s different styles of beer", 
          length(unique(recipeData$StyleID)))

top10_style = c(style_cnt_grp$Style[1:10])
style_cnt_other = style_cnt_grp[,c("Style", "Count")]
style_cnt_other$Style <- ifelse(style_cnt_other$Style %in% top10_style,
                                style_cnt_other$Style,
                                'Other')  
style_cnt_other = 
  style_cnt_other %>% 
  group_by(Style) %>% 
  summarise(Count = sum(Count))

style_cnt_other$Ratio <- 
  round((style_cnt_other$Count / as.numeric(nrow(recipeData))), 3) * 100

lbls = paste(style_cnt_other$Style, " ", style_cnt_other$Ratio,"%",sep="")
pie(style_cnt_other$Count, 
    labels = lbls,
    col = rainbow(length(style_cnt_other$Count)),
    main = 'Ratio of styles across dataset')

op <- par(cex = 0.85)
axis(2, at = 0:5, labels = 0:5)
barplot(style_cnt_other$Ratio,
        names.arg = style_cnt_other$Style,
        main = 'Ratio of styles across dataset',
        horiz = TRUE,
        ylab = 'Style',
        col = terrain.colors(n=length(style_cnt_other$Style)),
        legend = lbls,
        args.legend = list(x="bottomright"))

# Other Options for barplot color:
# col = rainbow(length(style_cnt_other$Style))
# col = heat.colors(n=length(style_cnt_other$Style))

pairplot_df = recipeData[,c("Style", "OG_sg", "FG_sg", "ABV", "IBU", "Color")]
flog.info("Please, wait a little 
          until the following graphic functions are finished")
pairs(pairplot_df[2:6])
ggpairs(pairplot_df[2:6]) 
flog.info("ggpairs is really explanatory graphic function!")

style_cnt_grp = style_cnt_grp[order(-style_cnt_grp$Count),]
top5_style <- style_cnt_grp[0:5,]$Style
if(style_cnt_grp[0:5,]$Style %in% pairplot_df[["Style"]]){
  top5_style_df <- pairplot_df[pairplot_df$Style %in% top5_style,]
}

ggplot(top5_style_df, aes(x=Style, y=OG_sg, fill=Style)) +
  geom_violin()

ggplot(top5_style_df, aes(x=Style, y=OG_sg)) +
  geom_violin() +
  geom_boxplot(width=0.1)

recipeData$Top5_Style <-
  ifelse(recipeData$Style == top5_style[1], top5_style[1],
         ifelse(recipeData$Style == top5_style[2], top5_style[2],
                ifelse(recipeData$Style == top5_style[3], top5_style[3],
                       ifelse(recipeData$Style == top5_style[4], top5_style[4],
                              ifelse(recipeData$Style == top5_style[5], top5_style[5], 
                                     'Other')))))

View(recipeData[,c("Style","Top5_Style")])
