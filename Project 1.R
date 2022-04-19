attach(alcohol_consumption_vs_gdp_per_capita)
library(tidyverse)
library(janitor)
library(ggridges)
library(patchwork)
alc= alcohol_consumption_vs_gdp_per_capita
names(alc) =c("country","Code","year","alcohol_consumption","gdp_per_cap","population_est","continent")
str(alc)
head(alc)
data.frame1 = data.frame(alc$alcohol_consumption, alc$gdp_per_cap, alc$population_est)
summary(data.frame1)
#data types
class(alc$alcohol_consumption)
class(alc$gdp_per_cap)
class(alc$country)
# explore data
# Viet Nam đứng thứ bao nhiêu về tiêu thụ rượu
v1<- (alc %>% drop_na() %>%
  filter(year ==2015))$country
which(v1=="Vietnam")
# 3 nước tiêu thụ rượu trên đầu người nhiều nhất năm 2015
v2 <- alc %>%
  drop_na()%>%
  filter(year==2015)%>% arrange(desc(alcohol_consumption))%>%
  select(countrym, alcohol_consumption)%>%
  head(3)
v2

#visualization

p1 <- alc %>% 
  ggplot(aes(x = alcohol_consumption, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03))
p2 <- alc %>% 
  ggplot(aes(x = gdp_per_cap, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03))
p3 <- alc %>% 
  ggplot(aes(x = population_est, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03))
p1/p2/p3
#
alc %>%
  ggplot(aes(x = log(population_est), y = gdp_per_cap)) +
  geom_point(aes(color = alcohol_consumption)) + 
  facet_wrap(~continent)
#
alc <- alc %>%
  mutate(alcohol_use = case_when(
    alcohol_consumption < 5 ~ "low",
    alcohol_consumption < 10 ~ "medium",
    alcohol_consumption > 15 ~ "high"
  ))
head(alc)
#
alc %>%
  drop_na() %>%
  ggplot(aes(x = log(population_est), y = gdp_per_cap)) +
  geom_jitter(aes(color = alcohol_use)) + 
  facet_wrap(~alcohol_use, nrow = 2)
#
alc %>%
  drop_na() %>%
  ggplot(aes(x = continent, y =alcohol_consumption)) + geom_point()
#
alc%>%
  drop_na()%>%
  ggplot(aes(x=continent,y= alcohol_consumption,color=continent))+geom_boxplot()
#
alc%>%
  drop_na()%>%
  ggplot(aes(x=continent,y= alcohol_consumption,color=continent))+geom_jitter()
#
alc%>%
  drop_na()%>%
  ggplot(aes(x=continent,y= alcohol_consumption,color=continent))+geom_jitter(aes(shape=continent))









