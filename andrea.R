library(tidyverse)
SixA <- data.frame(X = c("x","y","z","v_x","v_y","t","s","Theta1", "Theta2"), U1 = c("x","y","z", NA, NA, NA, NA, NA, NA), U2 = c("x","y","z","v_x","v_y", NA, NA, NA, NA), U3 = c("Theta1","t", NA, NA, NA, NA, NA, NA, NA), U4 = c("Theta2", "t", NA, NA, NA, NA, NA, NA, NA), U5 = c("s","Theta1", "Theta2", NA, NA, NA, NA, NA, NA), V1 = c("Theta1", NA, NA, NA, NA, NA, NA, NA, NA), V2 = c("Theta2", NA, NA, NA, NA, NA, NA, NA, NA), V3 = c("t", NA, NA, NA, NA, NA, NA, NA, NA))


SixA %>%
  pivot_longer(cols = 1:9, names_to = "sensor", values_to = "entity") %>%
  arrange(desc(sensor)) -> SixA

A <- function(stalk) {
  r_1x <- -73.662574
  r_1y <- 42.733838
  x <- stalk$value[stalk$entity=="x"][[1]]
  y <- stalk$value[stalk$entity=="y"][[1]]
  v_x <- stalk$value[stalk$entity=="v_x"][[1]]
  v_y <- stalk$value[stalk$entity=="v_y"][[1]]
  t <- stalk$value[stalk$entity=="t"][[1]]
  return(data.frame(entity = c("Theta1", "t"), value = c(atan2(x + v_x*t - r_1x, y + v_y*t-
                                                                 r_1y), t)))
}

#this is a correct function. 
B <- function(stalk) {
  r_1x <- -73.662574
  r_1y <- 42.733838
  #pick out values we want
  x <- stalk$value[stalk$entity=="x"][[1]]
  y <- stalk$value[stalk$entity=="y"][[1]]
  v_x <- stalk$value[stalk$entity=="v_x"][[1]]
  v_y <- stalk$value[stalk$entity=="v_y"][[1]]
  t <- stalk$value[stalk$entity=="t"][[1]]
  return(data.frame(entity = c("Theta2", "t"), value = c(atan2(x + v_x*t - r_1x, y + v_y*t -
                                                                 r_1y), t)))
}

# also correct. 
C <- function(stalk) {
  r_1x <- -73.662574
  r_1y <- 42.733838
  s_x <- stalk$value[stalk$entity=="s_x"][[1]]
  s_y <- stalk$value[stalk$entity=="s_y"][[1]]
  return(data.frame(entity = "Theta1" , value = atan2(s_x - r_1x, s_y - r_1y)))
}


D <- function(stalk) {
  r_2x <- -77.0897
  r_2y <- 38.935
  s_x <- stalk$value[stalk$entity=="s_x"][[1]]
  s_y <- stalk$value[stalk$entity=="s_y"][[1]]
  return(data.frame(entity = "Theta2", value = atan2(s_x - r_2x, s_y - r_2y)))
}

E <- function(stalk) {
  x <- stalk$value[stalk$entity=="x"][[1]]
  y <- stalk$value[stalk$entity=="y"][[1]]
  v_x <- stalk$value[stalk$entity=="v_x"][[1]]
  v_y <- stalk$value[stalk$entity=="v_y"][[1]]
  t <- stalk$value[stalk$entity=="t"][[1]]
  return(data.frame(entity = c("s", "Theta1", "Theta2"), value = c(x + v_x*t, y + v_y*t)))
}

pr1xpr2 <- function(stalk){
  stalk[stalk$entity=="x"|stalk$entity=="y"|stalk$entity=="z"|
          stalk$entity=="v_x"|stalk$entity== "v_y",]
}

#pr1 for u2 -> u1
U2_pr1 <- function(stalk){
  stalk[stalk$entity=="x"|stalk$entity=="y"|stalk$entity=="z",]
}

#pr1 for u3 -> v1
U3_pr1 <- function(stalk){
  stalk[stalk$entity=="Theta1",]
}

#pr2 for u3 -> v3
U34_pr2 <- function(stalk){
  stalk[stalk$entity=="t",]
}

#pr1 for u4 -> v2
U4_pr1 <- function(stalk){
  stalk[stalk$entity=="Theta2",]
}

SixB <- tibble(SSource = c("X", "X", "X", "X", "U2", "U3", "U3" , "U5", "U5", "U4",
                           "U4"),
               SDest = (c("U2", "U3", "U5", "U4", "U1", "V1", "V3",
                          "V1", "V2", "V3", "V2")),
               DMap = c(pr1xpr2, A, E, B, U2_pr1, U3_pr1, U34_pr2, C, D, U34_pr2,
                        U4_pr1))
as.data.frame(SixB) -> SixB

Table1 <- read.csv("Assignment2.csv")

Table1 %>%
  select(Key, entity, Case1) %>% #selecting vars for ...
  group_by(Key) %>% #grouping by key for ...
  rename(value = Case1) %>% #renaming case1 to value for test.
  nest(entity = entity, value = value) -> andrea  # andrea suggested I not use the nested table, bc of how it was presenting, she suggested two different columns. 

SixB %>%
  left_join(andrea, by = c(SSource = "Key")) -> SixB2

SixB2 %>%
  mutate(StalkOutput = invoke_map(.f = DMap, .x = entity))

