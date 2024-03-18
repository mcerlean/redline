#### DATA READING ####

rm(list=ls()) # clean R workspace enviro

# load needed packages:
library(ggplot2)          # for plotting, graphing, and maps
library(pROC)             # for roc curve
library(dplyr)            # for piping
library(randomForest)     # for fitting random forests
library(tidyr)            # for tidying data during cleaning (specifically 
                          # separate)
library(ggcorrplot)       # for plotting a correlation matrix in PCA
library(FactoMineR)       # for multivariate analysis in PCA
library(factoextra)       # for visualization in PCA
library(choroplethr)      # for map making
library(choroplethrMaps)  # for mapmaking
library(maps)             # For map data

# read in main data (from fivethirtyeight)
file_path = 'https://raw.githubusercontent.com/fivethirtyeight/data/master/redlining/metro-grades.csv'
red<- read.table(file_path,header = TRUE,sep = ',')

#### CLEANING ####
red<-red%>%
  # (1) create new binary variable holc_bin (A/B=1, C/D=0)
  mutate(holc_bin=ifelse(holc_grade%in%c("A","B"),1,0))%>%
  # (2) Split city and state into different columns on ","
  separate(metro_area, c("city", "state"), ", ")%>%
  # (3) Split state on "-", only keep first state
  separate(state, c("state", "States"), "-")%>%
  # (4) Made a "region" column with lowercase full state names (for mapmaking)
  mutate(region=tolower(state.name[match(state,state.abb)]))%>%
  mutate(across(c("city", "state","holc_bin"), factor))%>%
  # (5) Remove unused State, holc_grade, count population columns.
  select(-c(States, holc_grade,surr_area_asian_pop,surr_area_black_pop,
            surr_area_hisp_pop,surr_area_other_pop,surr_area_white_pop,
            white_pop,black_pop,hisp_pop,asian_pop,other_pop,total_pop))

#### RANDOM FOREST ####

# set train/test dfs
RNGkind(sample.kind="default")
set.seed(12345)
train.idx<-sample(x=1:nrow(red),size=.8*nrow(red))
train.df<-red[train.idx,]
test.df<-red[-train.idx,]

# baseline forest
myforest<-randomForest(holc_bin~pct_white+pct_black+pct_hisp+pct_hisp+
                         pct_other+lq_white+lq_black+lq_hisp+
                         lq_asian+lq_other+surr_area_pct_white+
                         surr_area_pct_black+surr_area_pct_hisp+
                         surr_area_pct_asian+surr_area_pct_other,
                       data=train.df,
                       ntree=1000,
                       mtry=4)

# tuning baseline forest:

# tuning mtry (14 since anything after gives an "invalid mtry" warning)
mtry<-c(1:14)

# make room for pairs of m and OOB error
keeps<-data.frame(m=rep(NA,length(mtry)),OOB_err_rate=rep(NA,length(mtry)))

# loop over each element of mtry
for(idx in 1:length(mtry)){
  print(paste0("Fitting m=",mtry[idx]))
  tempforest<-randomForest(holc_bin~pct_white+pct_black+pct_hisp+pct_hisp+
                             pct_other+lq_white+lq_black+lq_hisp+
                             lq_asian+lq_other+surr_area_pct_white+
                             surr_area_pct_black+surr_area_pct_hisp+
                             surr_area_pct_asian+surr_area_pct_other,
                           data=train.df,
                           ntree=1000,
                           mtry=mtry[idx])
  keeps[idx,"m"]<-mtry[idx]
  keeps[idx,"OOB_err_rate"]<-mean(predict(tempforest)!=train.df$holc_bin)
}

# plot keeps to see which m is lowest
ggplot(data=keeps)+
  geom_line(aes(x=m,y=OOB_err_rate))+
  scale_x_continuous(breaks=c(1:18))

# m of 7 would be ideal for minimizing OOB error based on plot
final_forest<-randomForest(holc_bin~pct_white+pct_black+pct_hisp+pct_hisp+
                             pct_other+lq_white+lq_black+lq_hisp+
                             lq_asian+lq_other+surr_area_pct_white+
                             surr_area_pct_black+surr_area_pct_hisp+
                             surr_area_pct_asian+surr_area_pct_other,
                           data=train.df,
                           importance=TRUE,
                           ntree=501,
                           mtry=7)
final_forest

# ROC curve
pi_hat = predict(final_forest, test.df, type = "prob")[,"1"]
rocCurve = roc(response = test.df$holc_bin,
               predictor = pi_hat,
               levels = c("0", "1"))
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# column of predicted values (1 or 0)
pi_star = coords(rocCurve, "best", ret = "threshold")$threshold
test.df<-test.df%>%
  mutate(forest_pred=ifelse(pi_hat > pi_star, "1", "0"))

# variable importance plot (most to least important in terms of out-of-sample
# prediction performance)

vi<-final_forest %>% 
  varImpPlot(type=1) %>% 
  as.data.frame()%>%
  tibble::rownames_to_column("Variable")

ggplot(data = vi) +
  geom_bar(aes(x = reorder(Variable,MeanDecreaseAccuracy),
               weight = MeanDecreaseAccuracy),
           position ="identity") +
  coord_flip() +
  labs( x = "Variable Name",y = "Importance")

#### MULTIVARIATE PLOTS ####

# histogram of pct_white vs holc grade
ggplot(data=red)+
  geom_histogram(aes(x=pct_white,fill=holc_bin), position="fill")+
  ggtitle("Proportion of White Population % and HOLC Grade")+
  scale_fill_grey()+
  theme_bw()
#histogram of pct_black vs holc_grade
ggplot(data=red)+
  geom_histogram(aes(x=pct_black,fill=holc_bin), position="fill")+
  ggtitle("Proportion of Black Population % and HOLC Grade")+
  scale_fill_grey()+
  theme_bw()

# most important variables:
# histogram of lq_white and holc_grade
ggplot(data=red)+
  geom_histogram(aes(x=lq_white,fill=holc_bin), position="fill")+
  ggtitle("Proportion of White Segregation and HOLC Grade")+
  scale_fill_grey()+
  theme_bw()
# histogram of lq_black and holc_grade
ggplot(data=red)+
  geom_histogram(aes(x=lq_black,fill=holc_bin), position="fill")+
  ggtitle("Proportion of Black Segregation and HOLC Grade")+
  scale_fill_grey()+
  theme_bw()
# histogram of lq_hisp and holc_grade
ggplot(data=red)+
  geom_histogram(aes(x=lq_hisp,fill=holc_bin), position="fill")+
  ggtitle("Proportion of Hispanic Segregation and HOLC Grade")+
  scale_fill_grey()+
  theme_bw()
#### PCA ####
# help from https://www.datacamp.com/tutorial/pca-analysis-r

num_data<-red[,3:17]

#normalize data
norm_data<-scale(num_data)

#make correlation matrix w/ normalized data
corr_matrix<-cor(norm_data)

# plot correlation matrix
ggcorrplot(corr_matrix)

# make pca variable
pca<-princomp(corr_matrix)

# check for where explanation of data (aka cumulative proportion) explains a
# lot of the data
summary(pca)
# since Comp.1, Comp.2, and Comp.3 explain nearly 83% of the data, first 3
# principal components can accurately explain the data

# scree plot
fviz_eig(pca, addlabels = TRUE)

# plot biplot and cos2 plots together
fviz_pca_var(pca, col.var="cos2", gradient.cols=c("black","orange","green"),
             repel = TRUE)
#### GLM ####

# logistic regression one variable added at a time (from variable importance)

# start w/ lq_white, lq_black
m1 = glm(holc_bin ~ lq_white+lq_black,
         data = red, family = binomial(link = "logit"))
AIC(m1) #487.2025
BIC(m1) #500.1377
# add lq_hisp
m2 = glm(holc_bin ~ lq_white+lq_black+lq_hisp,
         data = red, family = binomial(link = "logit"))
AIC(m2) #467.1908
BIC(m2) #484.4377
# add pct_white
m3 = glm(holc_bin ~ lq_white+lq_black+lq_hisp+pct_white,
         data = red, family = binomial(link = "logit"))
AIC(m3) #457.0539
BIC(m3) #478.6126
# add pct_black
m4 = glm(holc_bin ~ lq_white+lq_black+lq_hisp+pct_white+pct_black,
         data = red, family = binomial(link = "logit"))
AIC(m4) #458.9426
BIC(m4) #484.813

# AIC and BIC both prefer m3.
summary(m3)

# no standard errors greater than 5, confident we don't have complete separation
m3%>%coef%>%exp
confint(m3)

#### MAPMAKING ####
# help from https://r-graphics.org/recipe-miscgraph-choropleth
# state data (longitude, latitude)
states_map<-map_data("state")
# merge on state name to map dataframe
red_map<-merge(states_map,red,by.x="region",by.y="region") 
# reorder dataset
red_map<-arrange(red_map,group,order)
# plot holc_bin to map
ggplot(red_map, aes(x = long, y = lat, group = group, fill = holc_bin)) +
  geom_polygon(colour = "black") +
  ggtitle("Avg HOLC Score per State (A/B [1] or C/D [0])")+
  scale_fill_grey()+
  coord_map("polyconic")
