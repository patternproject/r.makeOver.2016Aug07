## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
## Make over Monday Entry for Wk 32
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools)
devtools::install_github("hadley/ggplot2")
pacman::p_load(ggplot2)

pacman::p_load(dplyr,tidyr,scales,grid,stringr,rvest)
pacman::p_load(RColorBrewer)
#pacman::p_load(ellipse)
pacman::p_load(ggthemes)
pacman::p_load(stats)
#pacman::p_load(plotly)
pacman::p_load(lubridate)

## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Constants ####
## `````````````````````````````````````````````
## color codes ####
col.grey = "#707070"
col.teal = "#368C8C"
col.blue = "#4682B4"
col.mid.green = "#98EFC1"
col.lig.green = "#B8FFD1"
col.dark.red = "darkred"
col.white = "white"

col.l.grey = "#D3D0CB"
col.m.grey = "#9FB1BC"
col.d.grey = "#6E8898"
col.d.blue = "#2E5266"
col.d.yellow = '#E2C044'
## point size####
size.l.point = 3
size.s.point = 1
## `````````````````````````````````````````````



## `````````````````````````````````````````````
#### Read Data ####
## `````````````````````````````````````````````
setwd("D:/2. Bianca/1. Perso/14. MakeoverMonday/30. 2016 Aug 07/r.makeOver.2016Aug07")

## df.master ####
df.master = read.csv(
  "2. Data/Olympic Medal Table.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Manipulate Data ####
## `````````````````````````````````````````````
names(df.master) = tolower(names(df.master))

# master replica
df.1 = df.master

# setting correct data types
df.1$country = as.factor(df.1$country)
df.1$country.group = as.factor(df.1$country.group)

# group by edition and then rank and keep top 5 only

# http://stackoverflow.com/questions/27766054/getting-the-top-values-by-group-using-dplyr
# From ?top_n, "The variable to use for ordering [...] defaults to the last
# variable in the tbl". The last variable in your data set is "grp", which is
# not the variable you wish to rank, and which is why your top_n attempt
# "returns the whole of d". Thus, if you wish to rank by "x" in your data set,
# you need to specify wt = x.

# If n is positive, selects the top n rows. If negative, selects the bottom n rows.

df.2 =   
  df.1 %>% 
  arrange(edition,rank) %>%
  group_by(edition) %>%
  top_n(n=-5,wt=rank)

# for each edition - calculate:
# 1) gold won by all 5 >> t5.g
# 2) total won by all 5 >> t5.t
# 3) average gold won by all 5 >> t5.a.g
# 4) average medals won by all 5 >> t5.a.t
df.3 = 
  df.2 %>%
  ungroup() %>%
  #filter(edition %in% c(1896,1900)) %>%
  group_by(edition) %>%
  summarise(t5.g = sum(gold),
            t5.t = sum(total))


# for each edition:
# 5) calculate (gold won by rank 1) / t5.g >> r1.g
# 6) calculate (total won by rank 1) / t5.t >> r1.t

df.4 =   
  df.2 %>% 
  arrange(edition) %>%
  group_by(edition) %>%
  top_n(n=-1,wt=rank) %>%
  left_join(df.3, by="edition") %>%
  summarise(r1.g = gold / t5.g,
          r1.t = total / t5.t,
          t5.a.g = mean(gold),
          t5.a.t = mean(total)) 
  

# rounding off the numbers
df.4$r1.g = round(df.4$r1.g,2)
df.4$r1.t = round(df.4$r1.t,2)

# melt it for plotting
df.5 = 
  df.4 %>%
  gather(metric,value,-edition)

# all gold related values
df.gold = 
  df.5 %>%
  filter(metric %in% c("r1.g", "t5.a.g"))

# all total related values
df.total = 
  df.5 %>%
  filter(metric %in% c("r1.t", "t5.a.t"))

## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Visualize Data ####
## `````````````````````````````````````````````

# https://www.safaribooksonline.com/library/view/r-graphics-cookbook/9781449363086/ch04.html

g.1 = ggplot() + theme_minimal()

# line plot
g.1 = g.1 + geom_line(data=df.gold,aes(x=edition,y=value,color=metric),position=position_dodge(0.2),linetype="dashed") 

# add points
g.2 = g.1 + geom_point(data=df.gold,aes(x=edition,y=value,color=metric, fill=metric),size=3, shape=21,position=position_dodge(0.2))

# customise colors
g.2 = g.2 + 
  scale_fill_manual(values=c(r1.g=col.d.yellow,t5.a.g=col.blue)) + 
  scale_color_manual(values=c(r1.g=col.grey,t5.a.g=col.grey))


# https://rud.is/b/2016/06/28/making-time-rivers-in-r/
# remove axis label and add subtitle (ensure latest ggplot version)
g.2 = g.2 + labs(x=NULL, y=NULL,
          title="Country ranking 1 compared to the Top 5, 1896-2012",
          subtitle="Yellow is GOLD by rank 1 as % of gold by Top 5. Blue is average of GOLD by Top 5.",
          caption="Data source: http://www.nbcolympics.com/medals")

g.2 <- g.2 + theme(plot.subtitle=element_text(margin=margin(b=15)))
g.2 <- g.2 + theme(legend.title=element_text(face="bold"))
g.2 <- g.2 + theme(plot.margin=margin(20,20,20,20))

# faceting based on metric
g.2 = g.2 + facet_grid(metric ~ .,scales="free_y")

# remove facet labels
g.2 = g.2 + theme(strip.text.y = element_blank())

# This removes all legends
g.2 = g.2 + theme(legend.position="none")

# changing axis text to grey, removing extra margins
g.2 = g.2 + theme(axis.text.x = element_text(color=col.m.grey, vjust=0.5,margin=margin(-15,-10,0,0)))
g.2 = g.2 + theme(axis.text.y = element_text(color=col.m.grey, hjust=1,margin=margin(0,-10,0,0)))
g.2 = g.2 + theme(panel.grid.major.y = element_line(size=0.15,color=col.l.grey))
g.2 = g.2 + theme(panel.grid.major.x = element_blank())
g.2 = g.2 + theme(panel.grid.minor = element_blank())

g.2

