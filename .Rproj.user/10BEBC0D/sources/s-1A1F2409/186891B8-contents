## 5.1.1 Turning real numbers to integers while preserving totals

# df <- tst %>% 
#   select(-x) %>% 
#   gather(var, actual) %>% 
#   mutate(
#     var = 'age',
#     round = round(actual),
#     floor = floor(actual)
#   )
library(dplyr)
library(tidyr)

long <- tibble(
  ages = c('age_1','age_2','age_3','age_4','age_5','age_6',
           'age_7','age_8','age_9','age_10'),
  actual= c(5.3, 6.6, 8.3, 20.3, 25.3, 30.8, 31.3, 22.3, 18.3, 14.5)
)

df <- long %>% 
  mutate(
    round = round(actual),
    floor = floor(actual),
    fraction = actual - floor(actual),
    rank = row_number(desc(fraction))
  )

# get totals
sumry<-df %>% 
  select(actual:floor) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(missing = actual - floor) 

# final adjustment and output
sumry <- df %>% 
  mutate(adjusted = ifelse(rank<=sumry$missing,floor+1,floor)) %>% 
  summarise(whole =sum(adjusted)) %>% 
  crossing(sumry,.)


# display in wide format
df %>% 
  mutate(cols = ages) %>% 
  gather(type, value, -cols) %>% 
  filter(type != 'ages') %>% 
  spread(cols, value) %>% 
  mutate_at(vars(2:length(.)),as.numeric)


## enhanced method
df <- long %>% 
  mutate(
    round = round(actual),
    floor = floor(actual),
    fraction = actual - floor(actual)
  ) %>% 
  rowwise() %>% 
  mutate(
    random = fraction + runif(1,0.001,0.01)
  )  %>% 
  ungroup() %>% # needed to un-rowwise
  mutate(
    rank = row_number(desc(random))
  )
  
# get totals
sumry<-df %>% 
  select(actual:floor) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(missing = actual - floor) 

# final adjustment and output
df2 <- df %>% 
  mutate(adjusted = ifelse(rank<=sumry$missing,floor+1,floor))

sumry <- df2 %>% 
  summarise(whole =sum(adjusted)) %>% 
  crossing(sumry,.)


## 5.1.2 Reconciling Totals by Adding. Rows are made to sum, columns not required to sum.
# negative numbers allowed (migration data). 

ctl_tot <- 169

dat_in <- tibble(
  ages = c('age_1','age_2','age_3','age_4','age_5','age_6',
           'age_7','age_8','age_9','age_10'),
  actual= c(1.1, 12.2, 12.8, 26.8, 32.4, 40.3, 28.4, 17.4, 13.1, 1.3)
)

# summary (totals) data
sumry <- dat_in %>% 
  summarize(sum=sum(actual),ctl = ctl_tot, diff = round(sum-ctl))



dat_in <- dat_in %>% 
  mutate(
    interim = actual - (sumry$diff/nrow(dat_in)),
    floor = floor(interim),
    fraction = interim - floor
  ) %>% 
  rowwise() %>% 
  mutate(
    random = fraction + runif(1,0.001,0.01)
  ) %>% ungroup() %>% 
  mutate(
    rank = row_number(desc(random))
  )
  
sumry <- dat_in %>% 
  summarise(sum_flr = sum(floor), missing = sum_flr - sumry$ctl) %>% 
  crossing(sumry,.)
  
df2 <- dat_in %>% 
  mutate(
    adjusted = if_else(rank<=abs(sumry$missing),floor+1, floor)
  ) 

df2 %>% 
  summarise(
    adjust = sum(adjusted)
  ) %>% 
  crossing(sumry,.)


# negative numbers not allowed (migration data). 
dat_in <- tibble(
  ages = c('age_1','age_2','age_3','age_4','age_5','age_6',
           'age_7','age_8','age_9','age_10'),
  actual= c(1.1, 12.2, 12.8, 26.8, 32.4, 40.3, 28.4, 17.4, 13.1, 1.3)
)

# summary (totals) data
# sumry <- dat_in %>% 
#   summarize(sum=sum(actual),ctl = ctl_tot, diff = round(sum-ctl))
# 
# dat_in <- dat_in %>% 
#   mutate(
#     interim = actual - (sumry$diff/nrow(dat_in)),
#     interim = if_else(interim < 0, 0, interim)
#   )
# 
# sumry <- dat_in %>% 
#   summarise(ctl = ctl_tot,int_sum=sum(interim),diff = round(int_sum-ctl))
# 
# dat_in %>% 
#   mutate(
#     interim1 = interim - (sumry$diff/nrow(dat_in)),
#     interim1 = if_else(interim1 < 0, 0, interim1)
#   ) %>% 
#   summarise(ctl = ctl_tot,int_sum=sum(interim1))
# 

interim_fuc <- function(x,ctl_tot){
  y <- x %>% 
    summarise(ctl = ctl_tot,int_sum=sum(actual),diff = round(int_sum-ctl))
  if(y$diff != 0){
    x = x %>% 
      mutate(
        actual = actual - (y$diff/nrow(x)),
        actual = if_else(actual < 0, 0, actual)
      )
    interim_fuc(x,ctl_tot)
  }else{
    x <- x %>% 
      rename(interim = actual)
    out <- list(x,y)
    return (out) 
  }
}

# recursively get values to match by subtracting
frac_list <- interim_fuc(dat_in, ctl_tot)

df2 <- frac_list[[1]]

#convert from fractions to integers
df <- df2 %>% 
  mutate(
    round = round(interim),
    floor = floor(interim),
    fraction = interim - floor(interim)
  ) %>% 
  rowwise() %>% 
  mutate(
    random = fraction + runif(1,0.001,0.01)
  )  %>% 
  ungroup() %>% # needed to un-rowwise
  mutate(
    rank = row_number(desc(random))
  )

# get totals
sumry<-df %>% 
  select(interim:floor) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(missing = interim - floor) 

# final adjustment and output
df3 <- df %>% 
  mutate(adjusted = ifelse(rank<=sumry$missing,floor+1,floor))

sumry <- df3 %>% 
  summarise(whole =sum(adjusted)) %>% 
  crossing(sumry,.)



# Function using ...
f <- function(...,a) {
  print(a)
  names(list(...))
}
f(a = 1, b = 2)
