library(tidyverse)
library(rms)
library(palmerpenguins)

head(penguins)

lm <- lrm(sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm, penguins)

ddist <- datadist(penguins %>% select(sex,bill_length_mm,bill_depth_mm,flipper_length_mm))
options(datadist='ddist')

nom <- nomogram(lm, fun = plogis, funlabel="Penguin Sex")
plot(nom)

