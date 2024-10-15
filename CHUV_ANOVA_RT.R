
library(rio)
library(plyr)
library(car)
library(dbplyr)
library(dplyr)
library(dtplyr)
library(effects)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(grid)
library(lattice)
library(magrittr)
library(multcomp)
library(PairedData)
library(plyr)
library(rio)
library(rstatix)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lme4)
library(car)
library(MuMIn)
library(multimode)
library(nlme)
library(effsize)
library(lsr)

#Doing ANOVA on RTs

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim('CHUV_behavioral_filtered3.txt', header = T)

data <- data %>% 
  rename("cube" = "cubes_rt",
         "body" = "bodies_rt",
         "object" = "objects_rt")

data_men <- subset(data, Gender == 'M')
data_women <- subset(data, Gender == 'F')

summary(data_men)
data_men %>% get_summary_stats(cube, type = "mean_sd")
data_men %>% get_summary_stats(body, type = "mean_sd")
data_men %>% get_summary_stats(object, type = "mean_sd")

summary(data_women)
data_women %>% get_summary_stats(cube, type = "mean_sd")
data_women %>% get_summary_stats(body, type = "mean_sd")
data_women %>% get_summary_stats(object, type = "mean_sd")


data_rt <- data%>% gather(key = "condition", value = "rt" , "cube", "body", "object")
data_rt$rt<-as.numeric(data_rt$rt)
data_rt$Gender <- as.factor(data_rt$Gender)
data_rt %>% group_by(condition)
data_rt %>% get_summary_stats(rt, type = "mean_sd")
data_rt$condition <- factor(data_rt$condition)



MOD_rt<-lme(rt~condition*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_rt)
anova(MOD_rt, test = "F")

MOD2_rt <- lmer(rt ~ condition + Gender + condition : Gender + (1 | Sub), data = data_rt)
Anova(MOD2_rt)

mod_rt <- lm(rt ~ Gender + condition + Gender : condition, data=data_rt)
Anova(mod_rt)
eta_squared(mod_rt)

# there is an effect of condition. Let's plot it.

ggplot(data_rt,aes(condition, rt))+
  geom_boxplot() + theme_bw() +
  ggtitle("RTs according to Condition")

a <- ggplot(data_rt,aes(condition, rt, fill=condition))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                      panel.grid.minor = element_blank())+
  ggtitle("RT according to Condition")

b<- a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

b + scale_fill_manual(values=c("#EC7063", "#42A5F5", "#4CAF50"))

ggplot(data_rt,aes(condition, rt, color=Gender))+
  geom_boxplot() + theme_bw() + 
  ggtitle("RT according to Condition and Gender")

ggplot(data_rt,aes(condition, rt, color=Version))+
  geom_boxplot() + theme_bw() +
  ggtitle("RTs according to Condition")



# Let's first investigate the effect of condition
# Let's see what's significant


datacubes<- subset(data_rt,condition=="cube")
#View(datacubes)
databodies<- subset(data_rt,condition=="body")
#View(databodies)
dataobjects<- subset(data_rt,condition=="object")
#View(dataobjects)

t.test(datacubes$rt,dataobjects$rt)  # non significant

t.test(datacubes$rt,databodies$rt)    # significant
cohen.d(datacubes$rt,databodies$rt)

t.test(dataobjects$rt,databodies$rt)   # significant
cohen.d(dataobjects$rt,databodies$rt)
str(data)


# let's now investigate the effect of gender

datacubesMen <- subset(datacubes, Gender =="M")
datacubesWomen <- subset(datacubes, Gender =="F")

dataobjectsMen <- subset(dataobjects, Gender =="M")
dataobjectsWomen <- subset(dataobjects, Gender =="F")

databodieslMen <- subset(databodies, Gender =="M")
databodiesWomen <- subset(databodies, Gender =="F")

library(emmeans)

# here if we do planned comparison the effect remains. If we continue and do the t.tests (below), there is no more effect.

emmeans(MOD_rt, list(pairwise ~ condition|Gender), adjust="tukey")
emmeans(MOD_rt, list(pairwise ~ Gender|condition), adjust="tukey")

t.test(datacubesMen$rt,datacubesWomen$rt)
t.test(dataobjectsMen$rt,dataobjectsWomen$rt)
t.test(databodieslMen$rt,databodiesWomen$rt)



# there are no gender differences in RTs

#Doing ANOVA with other factors

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim('CHUV_behavioral_filtered3.txt', header = T)

data <- data%>% gather(key = "condition", value = "rt" , "cube", "body", "object")
data$rt<-as.numeric(data$rt)
data$Gender <- as.factor(data$Gender)
data %>% group_by(condition)
data %>% get_summary_stats(rt, type = "mean_sd")
data$condition <- factor(data$condition)

mod0 <- lm(rt ~ Gender + condition + OSIQV_style + CFS_score + ST_score + ST_rt + Gender : OSIQV_style + Gender : ST_score + Gender : ST_rt, data)
anova(mod0)

mod1 <- lm(rt ~ Gender + condition + Spatial.scale + Object_scale + Gender : Spatial.scale, data)
anova(mod1)

mod1.1 <- lm(rt ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score, data)
anova(mod1.1)

mod1 <- lm(rt ~ Gender + condition + Spatial.scale + Object_scale + Verbal.scale + Gender : Spatial.scale, data)
Anova(mod1)

mod1.1 <- lm(rt ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score, data)
Anova(mod1.1)

## we add interactions 

mod1.2 <- lm(rt ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data)
anova(mod1.2)

mod1.3 <- lm(rt ~ Gender + condition + Spatial.scale + ST_score+ Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data)
anova(mod1.3)

ggplot(data=data, aes(x=Spatial.scale, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=Spatial.scale, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data=data, aes(x=ST_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

## removing the outlier

data2 <- data[data$Sub != 24, ]

ggplot(data=data2, aes(x=ST_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data2, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod1.4 <- lm(rt ~ Gender + condition + Spatial.scale + ST_score+ Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data2)
anova(mod1.4)

# The interaction st_score and gender is still significant. 

# effect computes the fit for certains levels.
effect <- Effect(c('Spatial.scale'), MOD1, se=T, typical=mean)
as.data.frame(effect) -> effect


ggplot(effect, aes(Spatial.scale, fit)) +
  geom_point(
    size=7, shape= 18,position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower,ymax=upper),
                size = 1, width=.1,position=position_dodge(.4))+
  theme_bw()+   theme(text = element_text(size = 15))+
  xlab("")+
  ylab("")

ggplot(data=data, aes(x=Spatial.scale, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod2 <- lm(rt ~ Gender + condition + Spatial.scale + Gender : Spatial.scale, data)
anova(mod2)


mod3 <- lm(rt ~ Gender + condition + OSIQV_style + Gender : OSIQV_style, data)
Anova(mod3)

ggplot(data=data, aes(x=Spatial.scale, y=rt, color=Sub)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data,aes(OSIQV_style, rt))+
  geom_boxplot() + theme_bw() +
  ggtitle("Accuracy according to OSIQV Style")

ggplot(data,aes(OSIQV_style, rt, color = Gender))+
  geom_boxplot() + theme_bw() +
  ggtitle("Accuracy according to OSIQV Style")


data2 <- data[data$Sub != 18, ]

mod4 <- lm(rt ~ Gender + condition + CFS_score + Gender : CFS_score, data2)
anova(mod4)

ggplot(data=data2, aes(x=CFS_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

## No effect of CFS scores whatsoever

mod5 <- lm(rt ~ Gender + ST_score + ST_rt + Gender : ST_score,data)
anova(mod5)

# there is an intercation between Gender and ST_score, let's plot it

ggplot(data=data, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#removing the outlier 
data3 <- data[data$Sub != 24, ]
mod5.1 <- lm(rt ~ Gender + ST_score + ST_rt + Gender : ST_score,data3)
anova(mod5.1)
ggplot(data=data3, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

# the effect is still there

#cor(data$ST_score, data$rt)

mod6 <- lm(rt ~ ST_rt + Gender + Gender : ST_rt,data)
anova(mod6)

ggplot(data=data, aes(x=ST_rt, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


### Same analysis, separating for each condition. Let's subset. 

data <- data%>% gather(key = "condition", value = "rt" , "cube", "body", "object")
data_cubes <- subset(data, condition == 'cube')
data_bodies <- subset(data, condition == 'body')
data_objects <- subset(data, condition == 'object')


### Investigation cubes condition with spatial scale in both men and women ###

cubes_mod<-lme(rt~Spatial.scale*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod, test = "F")
anova(cubes_mod, test = "F")

ggplot(data=data_cubes, aes(x=Spatial.scale, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=Spatial.scale, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

cubes_mod2<-lme(rt~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod2, test = "F")
anova(cubes_mod2, test = "F")

ggplot(data=data_cubes, aes(x=ST_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#removing the outlier 
data_cubes2 <- data_cubes[data_cubes$Sub != 24, ]
cubes_mod2.1<-lme(rt~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes2)
Anova(cubes_mod2.1, test = "F")
anova(cubes_mod2.1, test = "F")

ggplot(data=data_cubes2, aes(x=ST_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes2, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

## women with higher (= lower scores) to the perspective taking task are worse at MR for cubes

cubes_mod3<-lme(rt~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod3, test = "F")
anova(cubes_mod3, test = "F")

ggplot(data=data_cubes, aes(x=ST_rt, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=ST_rt, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#removing the outlier 
data_cubes3 <- data_cubes[data_cubes$Sub != 34, ]
cubes_mod3.1<-lme(rt~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes3)
Anova(cubes_mod3.1, test = "F")
anova(cubes_mod3.1, test = "F")

ggplot(data=data_cubes3, aes(x=ST_rt, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes3, aes(x=ST_rt, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


### Let's see if there is a link with the cognitive flexibility score ###

cubes_mod3<-lme(rt~CFS_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod3, test = "F")
anova(cubes_mod3, test = "F")

ggplot(data=data_cubes, aes(x=CFS_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=CFS_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

### no effect ###


### Investigation bodies condition with spatial scale in both men and women ###

bodies_mod<-lme(rt~Spatial.scale*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod, test = "F")
anova(bodies_mod, test = "F")

ggplot(data=data_bodies, aes(x=Spatial.scale, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=Spatial.scale, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

bodies_mod2<-lme(rt~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod2, test = "F")
anova(bodies_mod2, test = "F")

ggplot(data=data_bodies, aes(x=ST_score, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=ST_score, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

bodies_mod3<-lme(rt~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod3, test = "F")
anova(bodies_mod3, test = "F")

ggplot(data=data_bodies, aes(x=ST_rt, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=ST_rt, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

##remonving the outlier ##
data_bodies2 <- data_bodies[data_bodies$Sub != 34, ]
bodies_mod4<-lme(rt~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies2)
Anova(bodies_mod4, test = "F")
anova(bodies_mod4, test = "F")

ggplot(data=data_bodies2, aes(x=ST_rt, y=rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies2, aes(x=ST_rt, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


### doing the same stuff but on total scores


getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim('CHUV_behavioral_filtered3.txt', header = T)

data$cube<-as.numeric(data$cube)
data$object<-as.numeric(data$object)
data$body<-as.numeric(data$body)
data$mean_rt<-as.numeric(data$mean_rt)
data$Gender <- as.factor(data$Gender)


ggplot(data=data, aes(x=Spatial.scale, y=mean_rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=Spatial.scale, y=mean_rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

dataMen <- subset(data, Gender =="M")
dataWomen <- subset(data, Gender =="F")

library(emmeans)

mod <- lm(mean_rt ~ Gender + OSIQV_style + Gender : OSIQV_style, data)
anova(mod)

#emmeans(mod, list(pairwise ~ Spatial.scale|total_score), adjust="tukey")
#emmeans(mod, list(pairwise ~ Gender|condition), adjust="tukey")


ggplot(data=data, aes(x=Spatial.scale, y=cube)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=Spatial.scale, y=cube, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod1 <- lm(cube ~ Gender + Spatial.scale + Gender : Spatial.scale, data)
Anova(mod1)

ggplot(data=data, aes(x=Spatial.scale, y=body)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=Spatial.scale, y=body, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod2 <- lm(body ~ Gender + Spatial.scale + Gender : Spatial.scale, data)
Anova(mod2)

ggplot(data=data, aes(x=Spatial.scale, y=object)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=Spatial.scale, y=object, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


### Now let's check the score on the ST task, starting with accuracy

ggplot(data=data, aes(x=ST_score, y=mean_rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=ST_score, y=mean_rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data2 <- data[data$Sub != 24, ]

ggplot(data=data2, aes(x=ST_score, y=mean_rt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data2, aes(x=ST_score, y=mean_rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod3 <- lm(mean_rt ~ Gender + ST_score + Gender : ST_score, data2)
Anova(mod3)

ggplot(data=data2, aes(x=ST_score, y=cube)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data2, aes(x=ST_score, y=cube, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod4 <- lm(cube ~ Gender + ST_score + Gender : ST_score, data2)
Anova(mod4)

ggplot(data=data2, aes(x=ST_score, y=body)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data2, aes(x=ST_score, y=body, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod5 <- lm(body ~ Gender + ST_score + Gender : ST_score, data2)
Anova(mod5)

ggplot(data=data2, aes(x=ST_score, y=object)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data2, aes(x=ST_score, y=object, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod6 <- lm(object ~ Gender + ST_score + Gender : ST_score, data2)
Anova(mod6)


ggplot(data=data, aes(x=ST_rt, y=total_score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=ST_rt, y=total_score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data3 <- data[data$Sub != 34, ]

ggplot(data=data3, aes(x=ST_rt, y=total_score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data3, aes(x=ST_rt, y=total_score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod5 <- lm(total_score ~ Gender + ST_rt + Gender : ST_rt, data2)
Anova(mod5)

ggplot(data=data3, aes(x=ST_rt, y=cubes_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data3, aes(x=ST_rt, y=cubes_acc, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod6 <- lm(cubes_acc ~ Gender + ST_rt + Gender : ST_rt, data3)
Anova(mod6)

ggplot(data=data3, aes(x=ST_rt, y=bodies_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data3, aes(x=ST_rt, y=bodies_acc, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod7 <- lm(bodies_acc ~ Gender + ST_rt + Gender : ST_rt, data3)
Anova(mod7)


ggplot(data=data3, aes(x=ST_rt, y=objects_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data3, aes(x=ST_rt, y=objects_acc, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod8 <- lm(objects_acc ~ Gender + ST_rt + Gender : ST_rt, data3)
Anova(mod8)


