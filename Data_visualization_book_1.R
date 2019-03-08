install.packages("gapminder")
install.packages(c("broom", "coefplot", "cowplot", "GGally",
                    "ggrepel", "ggridges", "gridExtra", 
                    "here", "interplot","margins", "maps", "mapproj", 
                     "mapdata", "MASS", "quantreg", "rlang", "survey", 
                     "srvyr", "viridis", "viridisLite"), repos="http://cran.rstudio.com" )
library(gapminder)
library(ggplot2)
library(scales)
library(here)

install.packages("survey")
install.packages("coefplot")
install.packages("GGally")
install.packages("ggrepel")
install.packages("ggridges")
install.packages("gridExtra")
install.packages("here")
install.packages("interplot")
install.packages("margins")
install.packages("MASS")
install.packages("quantreg")
install.packages("rlang")
install.packages("srvyr")
install.packages("viridis")


devtools::install_github("kjhealy/socviz")

library(gapminder)
library(dplyr)


glimpse(gapminder)

# ploting life expectancy x gdpPercap
p <- ggplot(data = gapminder, mapping = aes(x=gdpPercap, y=lifeExp))
# by continent
p.cont <- ggplot(data = gapminder, mapping = aes(x=gdpPercap, 
                                                 y=lifeExp,
                                                 colour=continent))


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

# mapping the population as a gradient ina logarithm base
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



gapminder

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





     









