
#################################################################
# Packages para instalar

# Não funcionou be instalando todas de uma vez

#install.packages(c("broom", "coefplot", "cowplot", "GGally",
#                    "ggrepel", "gapminder", "ggridges", "gridExtra", 
#                    "here", "interplot","margins", "maps", "mapproj", 
#                     "mapdata", "MASS", "quantreg", "rlang", "survey", 
#                     "srvyr", "viridis", "viridisLite"))



# Funcionou instalando uma por vez
install.packages("survey")
install.packages("coefplot")
install.packages("cowplot")
install.packages("GGally")
install.packages("ggrepel")
install.packages("ggridges")
install.packages("gridExtra")
install.packages("here")
install.packages("interplot")
install.packages("interactionTest")
install.packages("margins")
install.packages("maps")
install.packages("mapproj")
install.packages("mapdata")
install.packages("MASS")
install.packages("quantreg")
install.packages("rlang")
install.packages("srvyr")
install.packages("viridis")
devtools::install_github("kjhealy/socviz")


# Loading the libraries
library(tidyverse)
library(broom)
library(coefplot)
library(cowplot)
library(GGally)
library(ggrepel)
library(ggridges)
library(gridExtra)
library(here)
# library(interplot)
library(margins)
library(maps)
library(mapproj)
library(mapdata)
library(MASS)
library(quantreg)
library(rlang)
library(survey)
library(srvyr)
library(viridis)
library(viridisLite)
library(devtools)
library(socviz)
library(gapminder)
library(ggplot2)
library(scales)
library(here)
library(gapminder)
library(dplyr)
library(socviz)


glimpse(gapminder)

# ploting life expectancy x gdpPercap
p <- ggplot(data = gapminder, mapping = aes(x=gdpPercap, y=lifeExp)) +
    theme_gray() # sets the default theme of ggplot

# by continent
p.cont <- ggplot(data = gapminder, mapping = aes(x=gdpPercap, 
                                                 y=lifeExp,
                                                 colour=continent)) +
       theme_dark() # use a dark theme
         

# making a sccater plot
p + geom_point()

# desenha a smooth curve com o médoto default "gam"
p + geom_point() + geom_smooth()
# desenha com o método lm (linear model)
p + geom_point() + geom_smooth(method = "lm")


# Using functions from scales library to reformat the numbers showed on the axis x or y
p + geom_point(alpha=0.3) + geom_smooth(method = "gam") + 
                   scale_x_log10(labels=comma) + 
                  labs(x = "GDP per capita", y = "Life Expectancy",
                       title="GDP x Life Expectancy", 
                       subtitle="Data points are country years",
                       caption="source: gapminder")  


# our plot segregated by continent
p.cont + geom_point() + geom_smooth(method = "loess") +
  scale_x_log10(labels=dollar)

# only one smooth
p + geom_point(mapping = aes(colour=continent)) + 
  geom_smooth(method = "loess") +  
  scale_x_log10(label=dollar)

# mapping the population as a gradient in a logarithm base
p + geom_point(mapping = aes(colour= log(pop))) + scale_x_log10(label=comma)

p + geom_point(mapping = aes(colour= year)) + scale_x_log10(label=comma)

p.cont + geom_jitter() + geom_boxplot() + scale_x_log10()


# See how the Life Expectancy evolves over time per continent
life.exp <- ggplot(data=gapminder, mapping = aes(x=continent, y=lifeExp))
life.exp + geom_jitter(mapping = aes(colour=year)) + geom_boxplot() + 
           labs(title="Expectativa de vida por continente nos anos")

# See how the Life Expectancy evolves over time per continent
gdp <- ggplot(data=gapminder, mapping = aes(x=continent, y=gdpPercap))
gdp + geom_jitter(mapping = aes(colour=year)) + geom_boxplot() + 
  labs(title="GDP por continente nos anos")




###################################################
# PART 2
# gss_m


# Usando Facets para ver como o GDP dos países se comporta com o passar dos anos
p2 <- ggplot(data=gapminder, mapping=aes(x=year, y=gdpPercap))
p3 <- ggplot(data=gapminder, mapping=aes(x=year, y=lifeExp))
# USando facet_warp em vez de facet_grid
p2 + geom_line(aes(group=country)) + facet_wrap(~continent)

# Improving...
p2 + geom_line(aes(group=country), colour="gray70") + 
     geom_smooth(size=1.1, se=FALSE, method = "loess") + 
     # muda a escala do eixo y, já que as linhas valores estão muito "pegadas"  
     scale_y_log10(labels=dollar) + 
     facet_wrap(~continent, ncol = 5) + 
     labs(title="GDP per capita over time on Five Continents")


p3 + geom_line(aes(group=country), colour="gray70") + 
  geom_smooth(size=1.1, se=FALSE, method = "loess") + 
  # muda a escala do eixo y, já que as linhas valores estão muito "pegadas"  
  facet_wrap(~continent, ncol = 5) + 
  labs(title="Life Expectancy over time on Five Continents")



###############################
p <- ggplot(data = gss_sm, mapping=aes(x=age, y=childs)) +
   theme_gray()

p + geom_point() + 
  geom_smooth() + 
  facet_grid(sex~race)


###############################
# assim não ficará correto
pbar <- ggplot(data = gss_sm, mapping = aes(x=bigregion, fill=bigregion))
        
pbar + geom_bar() + theme_gray()

# para ficar correto (mexemos na função estatística do eixo y, 
# para pegar a proporção)
pbar + geom_bar(mapping = aes(y = ..prop.., group=1))

pbar <- ggplot(data = gss_sm, mapping = aes(x=religion, fill=religion))
# Usamos guides(fill=FALSE) para tirar a legenda redundante
pbar + geom_bar() + guides(fill=FALSE) + labs(title="Religion")

# mapeando religiao por região em barras
pbar <- ggplot(data=gss_sm, mapping = aes(x = bigregion, 
                                          fill=religion))
pbar + geom_bar() + labs(title="Religion per Region - Stacked", subtitle="Não permite a adequada visualização das proporções")



# ajustando o preenchimento para facilitar a comparação entre as religiões dentro
#de cada região. No entanto, perdemos a capacidade de ver a proporção 
# de uma determinada religião em relação ao todo (país todo)
pbar + geom_bar(position = "fill") + labs(title="Religion per Region - Stacked", subtitle="Permite a comparação de proporção dentro de cada grupo")

# mudando o "position"
pbar + geom_bar(position = "dodge")
# muda novamente para ajustar a escala do eixo y
pbar + geom_bar(position = "dodge", mapping = aes(y=..prop.., group=religion)) +
  labs(title="Religions per Region", subtitle="Proporção relativa ao total")


# Mostrando em facets
pbar <- ggplot(data=gss_sm, mapping = aes(x=religion))
pbar + geom_bar(position = "dodge", mapping = aes(y=..prop.., group=bigregion))+
   facet_wrap(~bigregion, ncol=1)  



####################################
# Histograms and density plots

glimpse(midwest)
summary(midwest$area)

p <- ggplot(data=midwest, mapping = aes(x=area)) + 
     theme_gray()
p + geom_histogram(bins=10)  

# comparando histograms
oh_wi <- c("OH", "WI")
dados <- subset(midwest, subset = state %in% oh_wi)
p2 <- ggplot(data = dados, mapping = aes(x = percollege, fill=state)) +
      theme_gray()
p2 + geom_histogram(alpha=0.4)

# Kernel density 
p <- ggplot(data =midwest, mapping=aes(x=area)) + theme_gray()
p + geom_density()

?midwest

p <- ggplot(data=midwest, mapping = aes(x=area, fill=state, colour=state))
p +   geom_density(alpha=0.3)

# When we have a summarized table, we "cancel" the default stats transformation
# by saying "identity" to the stat parameter


# Ploting the titanic summarized data in a bar chart using "identity"
ptitanic <- ggplot(data = titanic, mapping = aes(x=fate, y=percent, 
                                                 fill=sex))
ptitanic + geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = "top") + theme_light() + 
  labs(title = "Titanic survivor statistics")


# OECD Table

# let's plot the differences between the US life expectancy and 
# OECD countries

tail(oecd_sum)

p <- ggplot(data=oecd_sum, mapping = aes(x=year, y=diff, fill=hi_lo))
p + geom_bar(stat = "identity") + theme_light() + 
  labs(x="", y="Difference in years",
       title="The US Life Expectancy Gap",
       subtitle="Difference between US and OECD average life Expectancy: 1960-2015",
       caption="Data: OECD") +
  guides(fill=FALSE)  
oecd_sum  
oecd_sum$perc <-  (oecd_sum$diff / oecd_sum$usa) * 100
tail(oecd_sum)
  
p <- ggplot(data=oecd_sum, mapping = aes(x=year, y=perc, fill=hi_lo))
p + geom_bar(stat = "identity") + theme_light() + 
  labs(x="", y="Difference %",
       title="The US Life Expectancy Gap %",
       subtitle="Difference between US and OECD average life Expectancy: 1960-2015",
       caption="Data: OECD") +
  guides(fill=FALSE)  








  
  



     









