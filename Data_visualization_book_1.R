install.packages("gapminder")

library(gapminder)
library(ggplot2)
library(scales)


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












