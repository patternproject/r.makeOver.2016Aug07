## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
## Make over Monday Entry for Wk 32
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
devtools::install_github("hadley/ggplot2")

if (!require("pacman")) install.packages("pacman")
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

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
#df.1$year = y(df.1$edition)

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

# for each edition:
# 1) calculate gold won by all 5 >> t5.g
# 2) calculate total won by all 5 >> t5.t
df.3 = 
  df.2 %>%
  ungroup() %>%
  #filter(edition %in% c(1896,1900)) %>%
  group_by(edition) %>%
  summarise(t5.g = sum(gold),
            t5.t = sum(total))


# for each edition:
# 3) calculate (gold won by rank 1) / t5.g >> r1.g
# 4) calculate (total won by rank 1) / t5.t >> r1.t

df.4 =   
  df.2 %>% 
  arrange(edition) %>%
  group_by(edition) %>%
  top_n(n=-1,wt=rank) %>%
  left_join(df.3, by="edition") %>%
  summarise(r1.g = gold / t5.g,
          r1.t = total / t5.t) 
  

# rounding off the numbers
df.4$r1.g = round(df.4$r1.g,2)
df.4$r1.t = round(df.4$r1.t,2)

# melt it for plotting
df.5 = 
  df.4 %>%
  gather(metric,value,r1.g:r1.t)
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Visualize Data ####
## `````````````````````````````````````````````
# g.1 = ggplot()
# 
# g.1 = g.1 + geom_line(data=df.5,aes(x=edition,y=value,group=metric),position=position_dodge(0.2))
# g.1
# 
# g.2 = g.1 + geom_point(data=df.5,aes(x=edition,y=value,fill=metric),size=4, shape=21,position=position_dodge(0.2))
# g.2

# https://www.safaribooksonline.com/library/view/r-graphics-cookbook/9781449363086/ch04.html

g.1 = ggplot() + theme_minimal()

g.1 = g.1 + geom_line(data=df.5,aes(x=edition,y=value,color=metric),position=position_dodge(0.2),linetype="dashed")
g.1

g.2 = g.1 + geom_point(data=df.5,aes(x=edition,y=value,color=metric, fill=metric),size=3, shape=21,position=position_dodge(0.2))
g.2 = g.2 + scale_fill_manual(values=c(col.d.yellow,col.d.blue)) + scale_color_manual(values=c(col.d.yellow,col.d.blue))

# This removes all legends
#g.2 = g.2 + theme(legend.position="none")

# https://rud.is/b/2016/06/28/making-time-rivers-in-r/
# remove axis label
g.2 = g.2 + labs(x=NULL, y=NULL,
          title="Rank 1 Compared to the Top 5, 1896-2012",
          subtitle=" Yellow is gold won by rank 1 as % of gold won by top5. Blue is total medals by rank 1 as % of total medals by top5",
          caption="Data source: http://www.nbcolympics.com/medals")

# g.2 = g.2 + theme(axis.title.y = element_blank())
# g.2 = g.2 + theme(axis.title.x = element_blank())

g.2 <- g.2 + theme(plot.subtitle=element_text(margin=margin(b=15)))
g.2 <- g.2 + theme(legend.title=element_text(face="bold"))
g.2 <- g.2 + theme(legend.position=c(0.05, 0.6))
g.2 <- g.2 + theme(plot.margin=margin(20,20,20,20))


g.2

# explore further
