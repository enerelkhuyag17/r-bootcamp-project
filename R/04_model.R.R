setwd("C:/Schmidlu/Dropbox/_Uni Luzern/Lehre/Natural Experiments using R")

data_path <- "./Data/Out_Data/"


# Lecture 1: Introduction -----


# Guided practice 1.1:  Collider bias

library(tidyverse)

set.seed(1234)
df <- data.frame(quality=rnorm(10000), 
                 location=rnorm(10000))
cor(df$quality,df$location)

df %>% summarize(correlation = cor(quality, location))

df$x <- df$quality + df$location
df$high_rating <- 1*(df$x > quantile(df$x, c(.75)))

head(df)

df %>% filter(high_rating == 1) %>%
  summarize(correlation = cor(quality, location))

ggplot(df ,
       aes(x=quality,y=location)) +
  geom_point()


summary(lm(quality~location,
           data=df %>% filter(high_rating == 1)))

ggplot(data=df,
       aes(x=quality,y=location)) +
  geom_point()

ggplot(data=df %>% filter(high_rating == 1),
       aes(x=quality,y=location)) +
  geom_point()




# Lecture 2: Experiments -----


library(tidyverse)

# Example 2.4: Estimation

df <- data.frame(high_quality=c(0,0,1,1,1,1,0,0),
                 good_accountants=c(1,0,1,0,1,0,1,0), 
                 y0=c(5,4,6,3,7,9,8,4),
                 y1=c(7,4,7,4,8,11,9,8))


df$d <- c(0,1,1,0,1,1,0,0)
df$y <- df$d*df$y1+(1-df$d)*df$y0

summary(lm(y~d,df))

df %>% group_by(d) %>%summarize(y_mean=mean(y))


# Lecture 3: Regression -----


library(tidyverse)

# Example 3.1: Example: Ratings and Revenue

df <- read.csv(paste0(data_path,"lecture3_ratings_revenues.csv"),
               sep=";", header = T)

ggplot(df, aes(x=x1,y=y)) +  
  geom_point(size=3) +
  geom_smooth(se=F,method=lm,fullrange=T ) +
  theme_bw_finegrid(base_size=42) +
  scale_x_continuous(limits=c(0,10)) +
  xlab("Rating (average) \n") + ylab("\n Revenues")  

summary(lm(y~x1,df))
summary(lm(y~x1+x2,df))
summary(lm(y~x1+x2+z,df))




# Lecture 4: Instrumental variables ----


library(AER)
data(CollegeDistance) 

# Example 4.1: Example: Education and wages

# (i) Regression with no covariates

CollegeDistance$wage_log <- log(CollegeDistance$wage)
(reducedform <-cov(CollegeDistance$wage_log,CollegeDistance$distance))
(firststage<-cov(CollegeDistance$education,CollegeDistance$distance))
(iv_estimate <- reducedform/firststage)

ivmodel1 <- ivreg(wage_log ~ education | distance ,
                  data=CollegeDistance)
summary(ivmodel1)

# (ii) Regression with covariates

firststage<-lm(education ~ unemp + ethnicity + gender + urban + 
                 distance, data = CollegeDistance)

summary(firststage)
CollegeDistance$education_hat <- predict(firststage)

secondstage<-lm(wage_log ~ education_hat + unemp + ethnicity + 
                  gender + urban , data = CollegeDistance)
summary(secondstage)

ivmodel2 <- ivreg(wage_log ~ unemp + ethnicity + gender + urban + education | 
                    unemp + ethnicity + gender + urban + distance, 
                  data = CollegeDistance) 
summary(ivmodel2)



# Lecture 5: Regression discontinuity design ----

df <- read.csv(paste0(data_path,"lecture5_ratings_revenues.csv"),
               sep=";", header = T)

ggplot(df,aes(x=rating_score)) +
  geom_histogram(color="black",fill="grey",bins=8) +
  theme_bw(base_size=28) 

library(rddensity)
rddensity(X = df$rating_score, c = 4.75) %>% 
  summary()

library(rdrobust)
covars <- c("quality","location","age")
for (var in covars){
  df$depvar <- df[[var]]
  rd_model <- rdrobust(y = df$depvar, x = df$rating_score, 
                       c = 4.75,all=TRUE)
  summary(rd_model)
}

ggplot(df,aes(x=rating_score,y=revenue,group=treat)) +
  geom_point(size=4) +
  geom_smooth(method="lm",se=F) +
  theme_bw(base_size=24) +
  geom_vline(xintercept=4.75,linetype="dashed",size=2)

rd_model <- rdrobust(y = df$revenue, x = df$rating_score, 
                     c = 4.75,all=TRUE)
summary(rd_model)


# Lecture 6: Panel data -----


library(lfe)

# (i) Read in data and rename variables, generate first differences (_fd)

df <- read.csv(paste0(data_path,"lecture7_ratings_revenue.csv"),
               sep=",", header = T)

df <- df %>% rename(rating=x,revenue=y) %>% group_by(group) %>% 
  mutate(rating_fd=rating-lag(rating),revenue_fd=revenue-lag(revenue)) %>%
  mutate(rating_mean=mean(rating),revenue_mean=mean(revenue)) %>%
  select(group,year,revenue,rating,rating_mean,revenue_mean,everything()) %>%
  arrange(group)

head(df)
summary(df)

# (ii) Descriptive evidence

ggplot(df,aes(rating,revenue)) +
  geom_point(size=4) +
  theme_bw(base_size=44) +
  xlab('Rating') + ylab('Revenue (per month, in 1000 CHF)') +
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(beta[1],beta[1]+beta[3]+beta[2]*10))


ggplot(df,aes(rating,revenue,color=factor(group))) +
  geom_point(size=4) +
  scale_color_manual(values=c("darkgreen","darkblue","red"),breaks=c("Montagna  ","The Motel  ","Monopoly  ")) +
  theme_bw(base_size=44) +
  xlab('Rating') + ylab('Revenue (per month, in 1000 CHF)') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(beta[1],beta[1]+beta[3]+beta[2]*10))


summary(lm(revenue~rating+group,df)) 
beta <- coef(lm(revenue~rating+group,df))

ggplot(df,aes(rating,revenue,color=factor(group))) +
  geom_point(size=4) +
  scale_color_manual(values=c("darkgreen","darkblue","red"),breaks=c("Montagna  ","The Motel  ","Monopoly  ")) +
  theme_bw(base_size=44) +
  geom_segment(x=0,xend=10,y=beta[1],yend=beta[1]+beta[2]*10,color="darkgreen",size=2)+
  geom_segment(x=0,xend=10,y=beta[1]+beta[3],yend=beta[1]+beta[3]+beta[2]*10,color="darkblue",size=2)+
  geom_segment(x=0,xend=10,y=beta[1]+beta[4],yend=beta[1]+beta[4]+beta[2]*10,color="red",size=2)+
  xlab('Rating') + ylab('Revenue (per month, in 1000 CHF)') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(beta[1],beta[1]+beta[3]+beta[2]*10))


ggplot(df,aes(rating_tilde,revenue_tilde)) +
  geom_point(size=4,aes(color=factor(group))) +
  scale_color_manual(values=c("darkgreen","darkblue","red"),breaks=c("Montagna  ","The Motel  ","Monopoly  ")) +
  theme_bw(base_size=44) +
  geom_smooth(method="lm",size=2,se=F)+
  xlab(TeX('Transformed Rating  ($\\tilde{X}_{it}$)')) + ylab(TeX('\n Transformed Revenue ($\\tilde{Y}_{it}$)')) +
  theme(legend.position="bottom",legend.title = element_blank())

options(scipen=999)
summary(lm(revenue_tilde~rating_tilde,df))

# (iii) Fixed effect regressions

model1 <- felm(revenue~rating|group|0|0,df)# felm(formula|fe|iv|cluster)
summary(model1) 

model2 <- felm(revenue~rating+factor(year)|group|0|0,df)# felm(formula|fe|iv|cluster)
summary(model2) 


model3 <- lm(revenue_tilde~rating_tilde+factor(year),df)

# (iv) Correcting the standard errors

library(lmtest)
library(sandwich)
coeftest(model3)
coeftest(model3,vcov=vcovHC(model3,type="HC3"))

model4 <- felm(revenue~rating+factor(year)|group|0|group,df)# felm(formula|fe|iv|cluster)
summary(model4)
model5 <- felm(revenue~rating+factor(year)|group|0|group+year,df)# felm(formula|fe|iv|cluster)
summary(model5)


# Lecture 7: Difference-in-differences -----


library(lfe)

# (i) Read in data and rename variables, generate first differences (_fd)

df <- read.csv(paste0(data_path,"lecture8_ratings_revenue.csv"),
               sep=",", header = T)

# (ii) Regressions

summary(lm(revenue~treat*d+factor(year),df))
summary(lm(revenue_tilde~treat_tilde+factor(year),df)) 
summary(felm(revenue~treat+factor(year)|group|0|0,df)) # felm(formula|fe|iv|cluster)


# Lecture 8: Synthetic control method -----

library(Synth)
library(readstata13)

# Example 8.1: Compulsory Voting and Turnout

cvoting <- read.dta13(paste0(data_path,"lecture8_compulsory_voting.dta")) %>%
           mutate(id=as.numeric(id)) %>%
           as.data.frame()

cvoting %>% head()

# (i) data preparation

dataprep.out <-
  dataprep(
    foo = cvoting,
    dependent = "turnout_mean",
    predictors =c("over_40_i", "over_50_i", "over_60_i", "pub_revenue_pc_i", 
                  "pub_spending_pc_i", "log_population_i", "urban_pop_i",
                  "work_pop_pop_i", "work_sec1_pop_i", "work_sec2_pop_i", 
                  "motor_pop_i"),
    unit.variable = "id",
    time.variable = "dat_num",
    special.predictors = list(
      list("turnout_mean",1:16, "mean"),
      list("turnout_mean",17:32, "mean")),
    treatment.identifier = 22,
    controls.identifier = c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 23, 24, 25),
    time.predictors.prior = 1:32,
    time.optimize.ssr = 1:32,
    unit.names.variable = "canton",
    time.plot = 1:130)

# (ii) Do Synthetization

synth.out <- 
  synth(
    data.prep.obj=dataprep.out
  )

synth.tables <- 
  synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out)

print(synth.tables)


# (iii) Show results

path.plot(
  dataprep.res = dataprep.out,
  synth.res = synth.out,
  Ylab = "Turnout"
)

gaps.plot(
  dataprep.res = dataprep.out,
  synth.res = synth.out,
  Ylab = "Turnout"
)

# (iv) Plot weights

synth.tables$tab.w$id <- as.numeric(as.factor(synth.tables$tab.w$unit.names))

ggplot(synth.tables$tab.w,aes(x=w.weights,y=reorder(factor(unit.names),-id))) +  
  geom_point(size=7.5) +
  theme_bw(base_size = 32) +       
  ylab("") + xlab("Weight")  +
  scale_y_discrete() +
  scale_shape_manual(values=c(2,16)) +
  geom_vline(xintercept=0,linetype="dashed")+
  geom_vline(xintercept=1,linetype="dashed") +  
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  labs(fill="") + 
  theme(legend.title=element_blank())  
  


# # Lecture: Matching (not covered) -----

# 
# library(tidyverse)
# library(MatchIt)
# library(cobalt)
# library(readstata13)
# 
# # Guided practice 4.1:  Lalonde
# 
# # (i) Read in data and print head of data
# 
# lalonde <- read.dta13(paste0(data_path,"lecture4_lalonde.dta"))%>%
#                         select(-sample,-educcat4 ) %>% 
#                         select(treat,re78,re74,re75,everything())  %>% 
#                         arrange(-treat)
# 
# # (ii) Assess differences between treatment and control group
# 
# lalonde %>% group_by(treat) %>% mutate(Nobs=n()) %>% summarize_all(list("mean")) 
# 
# 
# # (iii) Propensity score matching
# 
# # Step 1 - Matching
# 
# m.out <- matchit(treat ~ age + educ + black +  married + hisp + nodegree + re74 + 
#                    re75, data = lalonde, ratio=1,
#                  method = "nearest",distance = "logit")
# 
# summary(m.out)
# plot(m.out,type = "hist",cex.main=3, cex.lab=2, cex.axis=3)
# 
# 
# love.plot(m.out) + theme_bw(base_size=42) +
#   theme(legend.position="bottom") 
# 
# bal.tab(m.out, un = TRUE, binary = "std")
# 
# # Step 2 - restrict data to matched 
# 
# m.data=match.data(m.out,distance="pscore") 
# head(m.data %>% arrange(pscore))
# 
# # Step 3 - Estimate treatment effects
# # NOTE: We use weights here,to account for control observations that were  
# #       matched to multiple treated observations
# 
# summary(lm(re78~treat,m.data,weights=weights))
# 
# y_t<-match.data$re78[treatment]
# y_c<-match.data$re78[control]
# 
# matched.cases<-cbind(matches,y_t,y_c)
# t.test(matched.cases$y_t,matched.cases$y_c,paired=TRUE)
# 
# 