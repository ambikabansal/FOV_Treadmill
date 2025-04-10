require(dplyr)
require(lme4)
require(ggplot2)
require(cowplot)
require(openxlsx)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#reading in the data
k = 0
flist = list.files("Data", full.names = TRUE)
for (j in flist){
  if (k == 0){
    Data = read.csv(j, header = FALSE)
    Data$ID = j
    k = 1
  } else{
    Data_Temp = read.csv(j, header = FALSE)
    Data_Temp$ID = j
    Data = rbind(Data,Data_Temp)
  }
}

colnames(Data) = c("FOV","Condition","Data","Distance_Real", "Task", "Participant")


####outlier removal########

Dataframe_NoOutliers = Data %>% 
  mutate (Miss_Trials = case_when(           
    Data/FOV < 0.5 ~ "Outlier", 
    TRUE ~ "No Outlier")) %>%
  filter(Miss_Trials == "No Outlier" & Data !=0)
Dataframe_NoOutliers = Dataframe_NoOutliers %>% 
  group_by(Distance_Real, Task, Condition, FOV) %>%
  mutate(IQR = IQR(Data),
         LowerQuartile = quantile(Data, prob=c(.25)),
         UpperQuartile = quantile(Data, prob=c(.75)),
         Outlier = case_when(
           Data > UpperQuartile+1.5*IQR | Data < LowerQuartile-1.5*IQR ~ "Outlier",
           TRUE ~ "No Outlier")) %>%
  filter(Outlier == "No Outlier") %>%
  ungroup()

#write.csv(Dataframe_NoOutliers, "data_nooutliers.csv", row.names = FALSE)


require(tidyr)
Data_Temp = Dataframe_NoOutliers %>% filter(Outlier == "No Outlier") %>%
          select(FOV, Condition, Distance_Real, Participant, Data, Task)

####OR####

##keep data without the outlier analysis##
Data_Temp = Data %>%
  select(FOV, Condition, Distance_Real, Participant, Data, Task)

#####################################################

Data2 <- spread(Data_Temp, Task, Data)

Data2 = Data2 %>%   na.omit()

#invert the thing:
Data_Temp = Data_Temp %>%
  mutate(Actual_Travel_Distance = case_when(
           Task == 1 ~ Data,
           Task == 2 ~ Distance_Real),
         Perceived_Travel_Distance = case_when(
           Task == 1 ~ Distance_Real,
           Task == 2 ~ Data),
         Gain = Perceived_Travel_Distance/Actual_Travel_Distance)

# Data_Temp <- Data_Temp[is.finite(Data_Temp$Gain), ]

Data_Temp = Data_Temp %>% filter(Gain < 15) 


Data_Temp = Data_Temp %>%
  group_by(Participant, FOV, Condition, Task) %>%
  mutate(MeanPerformance = mean(Gain),
         DiffToMean = abs(Gain - MeanPerformance))

# ggplot(Data_Temp, aes(as.factor(Condition), DiffToMean, color = as.factor(FOV))) +
#   geom_boxplot() +
#   facet_wrap(Task~.)
  
library(lmerTest)


###Testing Differences Between Tasks###
#######################################

# Model_Tasks = lmer(Gain ~ as.factor(Task) + (as.factor(FOV) + as.factor(Condition) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                           data = Data_Temp,
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
#  confint(Model_Tasks, method = "boot", level = 0.95)
#  summary(Model_Tasks)
# 
# Model_Tasks_FOV = lmer(Gain ~ as.factor(Task) + (as.factor(Condition) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_FOV)
# 
# Model_Tasks_Condition = lmer(Gain ~ as.factor(Task) + (as.factor(FOV) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Condition)
# 
# Model_Tasks_Distance = lmer(Gain ~ as.factor(Task) + (as.factor(FOV) + as.factor(Condition) + as.factor(Task)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Distance)
# 
# 
# Model_Tasks_Tasks = lmer(Gain ~ as.factor(Task) + (as.factor(FOV) + as.factor(Condition) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Tasks)
# 
#  anova(Model_Tasks, Model_Tasks_Tasks)



###########Figure 3#############

####using full data set (Without outliers) cutting off gains above 5###

############FOV and Sensory Condition#################

# Cap for display
cap_value <- 5.0

# Count outliers
outliers_by_task <- Data_Temp %>%
  group_by(Task) %>%
  summarise(num_outliers = sum(Gain > cap_value))

print(outliers_by_task)

label_positions <- data.frame(
  Task = c(1, 2),
  x = 0.75,  # adjust as needed for horizontal placement
  y = c(5.7, 5.7),  # top and bottom y positions
  label = paste0("↑ ", outliers_by_task$num_outliers, " values > ", cap_value)
)

Data_Temp$Gain_clipped <- ifelse(Data_Temp$Gain > cap_value, cap_value, Data_Temp$Gain)

# Labels for the facet grid
tasklabels <- c('1' = "Move-To-Target", '2' = "Adjust-Target")

# Create boxplot with clipped values
p <- ggplot(Data_Temp, aes(x = as.factor(Condition), y = Gain_clipped, color = as.factor(FOV))) +
  geom_boxplot() +
  facet_grid(Task ~ ., labeller = labeller(Task = tasklabels)) +
  labs(x = "Sensory Condition") +
  scale_x_discrete(labels = c("Visual + Treadmill", "Visual Only", "Treadmill Only")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "FOV",
    labels = c("Full Field", "Central", "Peripheral"),
    values = c("red", "blue", "forestgreen")
  )


# Add arrow-like label to indicate outliers, near the top of the y-axis
if (num_outliers > 0) {
  p <- p +
    geom_text(data = label_positions,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 4, fontface = "bold")
}

# Print plot
print(p)

############Sensory Condition#################

# Cap for display
cap_value <- 5.0

# Count outliers per Task and FOV
outliers_by_task <- Data_Temp %>%
  group_by(Task, FOV) %>%
  summarise(num_outliers = sum(Gain > cap_value), .groups = "drop")

print(outliers_by_task)

# Create label data, one label per Task (summarizing across FOVs)
label_positions <- outliers_by_task %>%
  group_by(Task) %>%
  summarise(num_outliers = sum(num_outliers), .groups = "drop") %>%
  filter(num_outliers > 0) %>%
  mutate(
    x = 0.8,
    y = 5.7,
    label = paste0("↑ ", num_outliers, " values > ", cap_value)
  )

# Clip gains for plotting
Data_Temp$Gain_clipped <- ifelse(Data_Temp$Gain > cap_value, cap_value, Data_Temp$Gain)

# Labels for the facet grid
tasklabels <- c('1' = "Move-To-Target", '2' = "Adjust-Target")

# Create boxplot with clipped values
f <- ggplot(Data_Temp,aes(as.factor(Condition), Gain_clipped, color=as.factor(Condition))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Condition") +
  scale_x_discrete(labels = c("Visual+Treadmill", "Visual Only", "Treadmill Only")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position="none") +
  scale_color_manual(values = c("#008080", "#CC0066", "#CC6666"))
  
# Add arrow-like label to indicate outliers, near the top of the y-axis
if (nrow(label_positions) > 0) {
  f <- f +
    geom_text(data = label_positions,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 4, fontface = "bold")
}

# Print plot
print(f)


############FOV#################

# Cap for display
cap_value <- 5.0

# Count outliers per Task and FOV
outliers_by_task <- Data_Temp %>%
  group_by(Task, Condition) %>%
  summarise(num_outliers = sum(Gain > cap_value), .groups = "drop")

print(outliers_by_task)

# Create label data, one label per Task (summarizing across FOVs)
label_positions <- outliers_by_task %>%
  group_by(Task) %>%
  summarise(num_outliers = sum(num_outliers), .groups = "drop") %>%
  filter(num_outliers > 0) %>%
  mutate(
    x = 0.8,
    y = 5.7,
    label = paste0("↑ ", num_outliers, " values > ", cap_value)
  )

# Clip gains for plotting
Data_Temp$Gain_clipped <- ifelse(Data_Temp$Gain > cap_value, cap_value, Data_Temp$Gain)

# Labels for the facet grid
tasklabels <- c('1' = "Move-To-Target", '2' = "Adjust-Target")

# Create boxplot with clipped values
s <- ggplot(Data_Temp,aes(as.factor(FOV), Gain_clipped, color=as.factor(FOV))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "FOV (m/s)") +
  scale_x_discrete(labels = c("Full Field", "Central", "Peripheral")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(text = element_text(size = 18)) +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen"))

# Add arrow-like label to indicate outliers, near the top of the y-axis
if (nrow(label_positions) > 0) {
  s <- s +
    geom_text(data = label_positions,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 4, fontface = "bold")
}

# Print plot
print(s)

####plots done without cutting off gains####

tasklabels <- c('1' = "Move-To-Target", '2' = "Adjust-Target")

ggplot(Data_Temp,aes(as.factor(FOV), Gain, color=as.factor(Condition))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "FOV (m/s)") +
  scale_x_discrete(labels = c("Full Field", "Central", "Peripheral")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(name = "Sensory Condition", labels = c("Visual + Treadmill", "Visual Only", "Treadmill Only"), values = c("red", "blue", "forestgreen"))


ggplot(Data_Temp,aes(as.factor(Condition), Gain, color=as.factor(FOV))) +
   geom_boxplot() +
   facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
   labs (x = "Sensory Condition") +
   scale_x_discrete(labels = c("Visual + Treadmill", "Visual Only", "Treadmill Only")) +
   ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
   theme(text = element_text(size = 18)) +
   geom_hline(yintercept = 1, linetype = "dashed") +
   scale_color_manual(name = "FOV", labels = c("Full Field", "Central", "Peripheral"), values = c("red", "blue", "forestgreen"))


ggplot(Data_Temp,aes(as.factor(Condition), Gain, color=as.factor(Condition))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Condition") +
  scale_x_discrete(labels = c("Visual+Treadmill", "Visual Only", "Treadmill Only")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position="none") +
  scale_color_manual(values = c("#008080", "#CC0066", "#CC6666"))


ggplot(Data_Temp,aes(as.factor(FOV), Gain, color=as.factor(FOV))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "FOV (m/s)") +
  scale_x_discrete(labels = c("Full Field", "Central", "Peripheral")) +
  ylab("Gain (Perceived Travel Distance / Actual Travel Distance)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(text = element_text(size = 18)) +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen"))

##find means, SDs for raw gains
Data_Temp %>%
  group_by(Condition, Task) %>%
  summarise_at(vars(Gain), list(name = mean, sd))

df = Data_Temp %>% filter(Task==2)
gainsANOVA <- aov(Gain ~ Condition*FOV, data = df)
summary(gainsANOVA)

tukey <- TukeyHSD(gainsANOVA)
summary(tukey)
plot(tukey)



###linear mixed models comparisons###
#####################################

##checking for Task 1

# Model1_Everything = lmer(Gain ~ as.factor(Condition) + as.factor(FOV) + (as.factor(FOV) + as.factor(Condition)| Participant),
#                          data = Data_Temp %>% filter(Task==1 & Gain !=Inf),
#                          #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                          control = lmerControl(optimizer = "bobyqa"))
# confint(Model1_Everything, method = "boot", level = 0.95)
# summary(Model1_Everything)

Model1_FOV = lmer(Gain ~ as.factor(Condition) + FOV_Cont + (FOV_Cont + as.factor(Condition)| Participant),
                         data = Data_Temp %>% filter(Task==1 & Gain !=Inf) %>% mutate(FOV_Cont = FOV-1),
                         #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                         control = lmerControl(optimizer = "bobyqa"))
summary(Model1_FOV)
confint(Model1_FOV, method = "boot", level = 0.95)

Model1_Condition = lmer(Gain ~ Condition_Cont + as.factor(FOV) + (as.factor(FOV) + Condition_Cont| Participant),
                         data = Data_Temp %>% filter(Task==1 & Gain !=Inf) %>% mutate(Condition_Cont = Condition-1),
                         #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                         control = lmerControl(optimizer = "bobyqa"))
summary(Model1_Condition)
confint(Model1_Condition, method = "boot", level = 0.95)


# Model1_Everything_Condition = lmer(Gain ~ as.factor(Condition) + as.factor(FOV) + (as.factor(Condition) + as.factor(FOV)| Participant),
#                          data = Data_Temp %>% filter(Task==1 & Gain !=Inf),
#                          #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                          control = lmerControl(optimizer = "bobyqa"))
# confint(Model1_Everything_Condition, method = "boot", level = 0.95)
# summary(Model1_Everything_Condition)
# 
# mean(ranef(Model1_Everything_Condition)$Participant$'(Intercept)')
# 
# Model1_Everything_Fov = lmer(Gain ~ as.factor(FOV) + (as.factor(FOV)| Participant),
#                          data = Data_Temp %>% filter(Task==1 & Gain !=Inf),
#                          #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                          control = lmerControl(optimizer = "bobyqa"))
# confint(Model1_Everything_Fov, method = "boot", level = 0.95)
# mean(ranef(Model1_Everything_Fov)$Participant$'(Intercept)')


# Model1_Interaction = lmer(Gain ~ as.factor(Condition) * as.factor(FOV) + (1 + as.factor(FOV) + as.factor(Condition)| Participant),
#                           data = Data_Temp %>% filter(Task==1 & Gain !=Inf),
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
# # #confint(Model1_Interaction, method = "boot", level = 0.95)
# summary(Model1_Interaction)


# #checking for interaction
# anova(Model1_Interaction, Model1_Everything)

### No interaction found ###



##checking for Task 2

# Model1_Everything_Task2 = lmer(Gain ~ as.factor(Condition) + as.factor(FOV) + (as.factor(FOV) + as.factor(Condition)| Participant),
#                          data = Data_Temp %>% filter(Task==2),
#                          #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                          control = lmerControl(optimizer = "bobyqa"))
# confint(Model1_Everything, method = "boot", level = 0.95)
# summary(Model1_Everything_Task2)

Model2_FOV = lmer(Gain ~ as.factor(Condition) + FOV_Cont + (FOV_Cont + as.factor(Condition)| Participant),
                  data = Data_Temp %>% filter(Task==2 & Gain !=Inf) %>% mutate(FOV_Cont = FOV-1),
                  #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                  control = lmerControl(optimizer = "bobyqa"))
summary(Model2_FOV)
confint(Model2_FOV, method = "boot", level = 0.95)

Model2_Condition = lmer(Gain ~ Condition_Cont + as.factor(FOV) + (as.factor(FOV) + Condition_Cont| Participant),
                        data = Data_Temp %>% filter(Task==2 & Gain !=Inf) %>% mutate(Condition_Cont = Condition-1),
                        #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                        control = lmerControl(optimizer = "bobyqa"))
summary(Model2_Condition)
confint(Model2_Condition, method = "boot", level = 0.95)


# Model1_FOV_Task2 = lmer(Gain ~ as.factor(Condition) + as.factor(FOV) + as.factor(Distance_Real) + (as.factor(FOV)| Participant),
#                     data = Data_Temp %>% filter(Task==2),
#                     control = lmerControl(optimizer = "bobyqa"))
# #confint(Model1_FOV, method = "boot", level = 0.95)
# summary(Model1_FOV_Task2)
# 
# Model1_Condition_Task2 = lmer(Gain ~ as.factor(Condition) + as.factor(FOV) + as.factor(Distance_Real) + (as.factor(Condition)| Participant),
#                         data = Data_Temp %>% filter(Task==2),
#                         #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                         control = lmerControl(optimizer = "bobyqa"))
# #confint(Model1_Condition, method =  "boot", level = 0.95)
# summary(Model1_Condition_Task2)


# Model1_Interaction_Task2 = lmer(Gain ~ as.factor(Condition) * as.factor(FOV) + (1 + as.factor(FOV) + as.factor(Condition)| Participant),
#                           data = Data_Temp %>% filter(Task==2),
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
# #confint(Model1_Interaction, method = "boot", level = 0.95)
# summary(Model1_Interaction)


# #checking if the simpler model can be used (it can't)
# anova(Model1_FOV_Task2, Model1_Everything_Task2)
# anova(Model1_Condition_Task2, Model1_Everything_Task2)


# #checking for interaction
# anova(Model1_Interaction_Task2, Model1_Everything_Task2)



##############RESULTS################

###MTT Condition
require(lmerTest)
Model1_FOV = lmer(Gain ~ as.factor(Condition2) + FOV_Cont + (FOV_Cont + as.factor(Condition2)| Participant),
                  data = Data_Temp %>% filter(Task==1 & Gain !=Inf) %>% mutate(FOV_Cont = FOV-1)  %>%
                      mutate(Condition2 = 
                               case_when(Condition == 2 ~ "1Treadmill Only",
                                         Condition == 1 ~ "4Visual Only",
                                         Condition == 0 ~ "3Visual + Treadmill")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model1_FOV, method = "boot", level = 0.95)
summary(Model1_FOV)



###AT Condition
require(lmerTest)
Model2_FOV = lmer(Gain ~ as.factor(Condition2) + FOV_Cont + (FOV_Cont + as.factor(Condition2)| Participant),
                  data = Data_Temp %>% filter(Task==2 & Gain !=Inf) %>% mutate(FOV_Cont = FOV-1)  %>%
                      mutate(Condition2 = 
                               case_when(Condition == 2 ~ "1Treadmill Only",
                                         Condition == 1 ~ "4Visual Only",
                                         Condition == 0 ~ "3Visual + Treadmill")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_FOV, method = "boot", level = 0.95)
summary(Model2_FOV)



###MTT FOV
Model1_Condition = lmer(Gain ~ Condition_Cont + as.factor(FOV2) + (as.factor(FOV2) + Condition_Cont| Participant),
                        data = Data_Temp %>% filter(Task==1 & Gain !=Inf) %>% mutate(Condition_Cont = Condition-1) %>%
                          mutate(FOV2 = 
                               case_when(FOV == 2 ~ "1peripheral",
                                         FOV == 1 ~ "3central",
                                         FOV == 0 ~ "5fullfield")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model1_Condition, method = "boot", level = 0.95)
summary(Model1_Condition)



###AT FOV
Model2_Condition = lmer(Gain ~ Condition_Cont + as.factor(FOV2) + (as.factor(FOV2) + Condition_Cont| Participant),
                        data = Data_Temp %>% filter(Task==2 & Gain !=Inf) %>% mutate(Condition_Cont = Condition-1) %>%
                      mutate(FOV2 = 
                               case_when(FOV == 2 ~ "1peripheral",
                                         FOV == 1 ~ "3central",
                                         FOV == 0 ~ "5fullfield")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Condition, method = "boot", level = 0.95)
summary(Model2_Condition)




#############MODELING##############
###################################


#Fitting slopes
initalParam_Simple = c(0,6)
StraightLine = function(VectorRealDistances, Slope){
  Slope*VectorRealDistances
}

errfn_StraightLine = function(Slope,VectorRealDistances,VectorResponseDistances){
  mean((VectorResponseDistances[!is.na(VectorResponseDistances)]-StraightLine(VectorRealDistances, Slope))^2)
}


colnames(Data2) = c("FOV","Condition","Distance_Real","Participant", "Data_MTT","Data_AT")


############################################################
####Lappe model
############################################################

errfn_Lappe = function(p,VectorRealDistances,VectorResponseDistancesMTT, VectorResponseDistancesAT){
  MSE1 = mean((VectorResponseDistancesMTT[!is.na(VectorResponseDistancesMTT)]-lappe(VectorRealDistances, TypeOfTask = "MTT", p[1], p[2]))^2) #p(1)= gain, p(2)= decay
  MSE2 = mean((VectorResponseDistancesAT[!is.na(VectorResponseDistancesAT)]-lappe(VectorRealDistances, TypeOfTask = "AT", p[1], p[2]))^2) #p(1)= gain, p(2)= decay
  MSEtotal = MSE1 + MSE2
  MSEtotal
}

errfn_Lappe_NoAlpha = function(p,VectorRealDistances,VectorResponseDistancesMTT, VectorResponseDistancesAT){
  MSE1 = mean((VectorResponseDistancesMTT[!is.na(VectorResponseDistancesMTT)]-lappe(VectorRealDistances, TypeOfTask = "MTT", gain = p, alpha = 0.00001))^2) #p(1)= gain, p(2)= decay
  MSE2 = mean((VectorResponseDistancesAT[!is.na(VectorResponseDistancesAT)]-lappe(VectorRealDistances, TypeOfTask = "AT", gain = p, alpha = 0.00001))^2) #p(1)= gain, p(2)= decay
  MSEtotal = MSE1 + MSE2
  MSEtotal
}

initalParam_Lappe <- c( 1, 0.01)

lappe = function(d0, gain, alpha, TypeOfTask){
  if(TypeOfTask == "MTT"){
    # PredictedDistance = -(1/alpha)*log(gain/(d0*alpha+gain))
    PredictedDistance = (log(d0+gain/alpha)-log(gain/alpha))/alpha}
  else if (TypeOfTask == "AT"){
    PredictedDistance = (gain/alpha) * ( 1 - exp(-d0*alpha))}
  PredictedDistance}


AIC_Self <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}
relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}

Data4 = Data2[complete.cases(Data2),]

set.seed(3)
Data5 = rbind(Data4 %>% filter(Condition == 2),
              (Data4 %>% filter(Condition == 0))[sample(1:length((Data4 %>% filter(Condition == 0))$Participant), length((Data4 %>% filter(Condition == 2))$Participant)),],
              (Data4 %>% filter(Condition == 1))[sample(1:length((Data4 %>% filter(Condition == 1))$Participant), length((Data4 %>% filter(Condition == 2))$Participant)),])

# Data5 = rbind(Data4 %>% filter(Condition == 4),
#               (Data4 %>% filter(Condition == 1))[sample(1:length((Data4 %>% filter(Condition == 1))$Participant), length((Data4 %>% filter(Condition == 4))$Participant)),],
#               (Data4 %>% filter(Condition == 2))[sample(1:length((Data4 %>% filter(Condition == 2))$Participant), length((Data4 %>% filter(Condition == 4))$Participant)),],
#               (Data4 %>% filter(Condition == 3))[sample(1:length((Data4 %>% filter(Condition == 3))$Participant), length((Data4 %>% filter(Condition == 4))$Participant)),])

Data5 = Data5 %>%
  group_by(FOV,Condition,Participant) %>%
  mutate(LappeGain = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                           VectorResponseDistancesMTT = Data_MTT,
                           VectorResponseDistancesAT = Data_AT)$par[1],
         
         LappeDecay = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                            VectorResponseDistancesMTT = Data_MTT,
                            VectorResponseDistancesAT = Data_AT)$par[2],
         
         Prediction_Lappe_MTT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "MTT"),
         Prediction_Lappe_AT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "AT"),
         
         MSE_Lappe = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                           VectorResponseDistancesMTT = Data_MTT,
                           VectorResponseDistancesAT = Data_AT)$value[1],
         
         AIC_Lappe = AIC_Self(MSE_Lappe, 2, length(LappeGain))
  )

Data5 = Data5 %>%
  group_by(FOV,Condition,Participant) %>%
  mutate(#Slope = optimize(errfn_StraightLine,
    # interval=initalParam_Simple, 
    # VectorRealDistances=Perceived_Travel_Distance, 
    # VectorResponseDistances=Actual_Travel_Distance)$minimum,
    
    #Prediction_Slope = Slope*Actual_Travel_Distance,
    
    LappeGain = optim(1, errfn_Lappe_NoAlpha, 
                      VectorRealDistances=Distance_Real,
                      VectorResponseDistancesMTT = Data_MTT,
                      VectorResponseDistancesAT = Data_AT, 
                      method = "Brent",
                      lower = 0,
                      upper = 15)$par,
    
    PredictionLappe_NoAlpha_MTT = lappe(Distance_Real, LappeGain, alpha = 0.000001, "MTT"),
    PredictionLappe_NoAlpha_AT = lappe(Distance_Real, LappeGain, alpha = 0.000001, "AT"),
    
    MSE_LappeNoAlpha = optim(1, errfn_Lappe_NoAlpha, 
                             VectorRealDistances=Distance_Real,
                             VectorResponseDistancesMTT = Data_MTT,
                             VectorResponseDistancesAT = Data_AT, 
                             method = "Brent",
                             lower = 0,
                             upper = 15)$value,
    
    AIC_Slope = AIC_Self(MSE_LappeNoAlpha, 1, length(Data3$LappeGain))
  )



ggplot(Data5, aes(as.factor(Condition), MSE_Lappe, color = as.factor(FOV))) +
  geom_boxplot()


Data5 = Data5 %>%
  mutate(Error_AT = abs(Data_AT  - Prediction_Lappe_AT),
         Error_MTT = abs(Data_MTT  - Prediction_Lappe_MTT))

ggplot(Data5, aes(as.factor(Condition), Error_AT, color = as.factor(FOV))) +
  geom_boxplot()

ggplot(Data5, aes(as.factor(Condition), Error_MTT, color = as.factor(FOV))) +
  geom_boxplot()


###Figure 4

Data3 = Data2 %>%
  group_by(FOV,Condition) %>%
  mutate(LappeGain = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                           VectorResponseDistancesMTT = Data_MTT,
                           VectorResponseDistancesAT = Data_AT)$par[1],

         LappeDecay = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                            VectorResponseDistancesMTT = Data_MTT,
                            VectorResponseDistancesAT = Data_AT)$par[2],

         Prediction_Lappe_MTT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "MTT"),
         Prediction_Lappe_AT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "AT"))
save(Data3, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Data3.RData"))

load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Data3.RData"))


Data3 = Data3 %>% 
  group_by(Distance_Real, Condition, FOV) %>%
  mutate(Mean_Performance_AT = mean(Data_AT, na.rm = TRUE),
         SD_Performance_AT = sd(Data_AT, na.rm = TRUE),
         Mean_Performance_MTT = mean(Data_MTT, na.rm = TRUE),
         SD_Performance_MTT = sd(Data_MTT, na.rm = TRUE),
         FOV2 = case_when(
           FOV == 0 ~ "Full Field",
           FOV == 1 ~ "Central Field",
           FOV == 2 ~ "Peripheral Field"),
         Condition2 = case_when(
           Condition == 0 ~ "Visual + Treadmill",
           Condition == 1 ~ "Visual Only",
           Condition == 2 ~ "Treadmill Only"))

#actual / perceived travel distance
ggplot(Data3,
       aes(Mean_Performance_AT,Distance_Real)) +
  geom_point(color = "red", size = 2) +
  geom_ribbon(aes(xmin = Mean_Performance_AT - SD_Performance_AT, xmax = Mean_Performance_AT + SD_Performance_AT), 
              fill = "red",
              alpha = 0.2) +
  geom_line(color = "red", linetype = 2) +
  facet_grid(FOV2~Condition2) +
  geom_point(aes(Distance_Real,Mean_Performance_MTT), color = "blue", size = 2, shape = 15, alpha = 0.5) +
  geom_line(aes(Distance_Real,Mean_Performance_MTT), color = "blue", linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(x = Distance_Real,
                  ymin = Mean_Performance_MTT - SD_Performance_MTT, 
                  ymax = Mean_Performance_MTT + SD_Performance_MTT), 
              fill = "blue",
              alpha = 0.2) +
  geom_line(aes(Prediction_Lappe_AT,Distance_Real),color = "darkred", size = 2) +
  geom_line(aes(Distance_Real,Prediction_Lappe_MTT), color = "darkblue", size = 2) +
  ylab("Actual Traveled Distance (m)") +
  xlab("Perceived Traveled Distance (m)") +
  coord_cartesian(xlim = c(0,40), ylim = c(0,35)) +
  theme(text = element_text(size = 20)) 
ggsave("Figure2 Lappe fits Confidence Bands (SDs).jpg", w = 12, h = 8)


#perceived / actual travel distance
LabelData <- Data3 %>%
  group_by(FOV2, Condition2) %>%
  summarise(
    Gain = mean(LappeGain, na.rm = TRUE),
    Alpha = mean(LappeDecay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x = 7,  
    y = 37,   
    label = paste0("LSI Gain: ", round(Gain, 2), "\nAlpha: ", round(Alpha, 3))
  )

a <- ggplot(Data3,
       aes(Distance_Real, Mean_Performance_AT)) +
  geom_point(color = "red", size = 2) +
  geom_ribbon(aes(ymin = Mean_Performance_AT - SD_Performance_AT, ymax = Mean_Performance_AT + SD_Performance_AT), 
              fill = "red",
              alpha = 0.2) +
  geom_line(color = "red", linetype = 2) +
  facet_grid(FOV2~Condition2) +
  geom_point(aes(Mean_Performance_MTT, Distance_Real), color = "blue", size = 2, shape = 15, alpha = 0.5) +
  geom_line(aes(Mean_Performance_MTT, Distance_Real), color = "blue", linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(y = Distance_Real,
                  xmin = Mean_Performance_MTT - SD_Performance_MTT, 
                  xmax = Mean_Performance_MTT + SD_Performance_MTT), 
              fill = "blue",
              alpha = 0.2) +
  geom_line(aes(Distance_Real, Prediction_Lappe_AT),color = "darkred", size = 2) +
  geom_line(aes(Prediction_Lappe_MTT, Distance_Real), color = "darkblue", size = 2) +
  xlab("Actual Traveled Distance (m)") +
  ylab("Perceived Traveled Distance (m)") +
  coord_cartesian(xlim = c(0,40), ylim = c(0,40)) +
  theme(text = element_text(size = 20)) 

a <- a +
  geom_text(data = LabelData,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 3.5, fontface = "bold")

print(a)


####Testing Differences between Lappe and Slope for the Different Conditions

##Condition 0
round(mean((Data5 %>% filter(Condition == 0))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Condition == 0))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Condition == 0) %>%
                               group_by(Condition,FOV,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Visual_Treadmills = AIC_Self(sum((Data5 %>% filter(Condition == 0))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Condition == 0))$MSE_Lappe))
AIC_Slope_All_Visual_Treadmills = AIC_Self(sum((Data5 %>% filter(Condition == 0))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Condition == 0))$MSE_Lappe))
RelLikelihood_Lappe_Visual_Treadmills = exp( (AIC_Lappe_All_Visual_Treadmills - AIC_Slope_All_Visual_Treadmills) / 2 )
RelLikelihood_Lappe_Visual_Treadmills


##Condition 1
round(mean((Data5 %>% filter(Condition == 1))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Condition == 1))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Condition == 1) %>%
                               group_by(Condition,FOV,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Visual = AIC_Self(sum((Data5 %>% filter(Condition == 1))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Condition == 1))$MSE_Lappe))
AIC_Slope_All_Visual = AIC_Self(sum((Data5 %>% filter(Condition == 1))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Condition == 1))$MSE_Lappe))
RelLikelihood_Lappe_Visual = exp( (AIC_Lappe_All_Visual - AIC_Slope_All_Visual) / 2 )
RelLikelihood_Lappe_Visual


##Condition 2
round(mean((Data5 %>% filter(Condition == 2))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Condition == 2))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Condition == 2) %>%
                               group_by(Condition,FOV,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Treadmill = AIC_Self(sum((Data5 %>% filter(Condition == 2))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Condition == 2))$MSE_Lappe))
AIC_Slope_All_Treadmill = AIC_Self(sum((Data5 %>% filter(Condition == 2))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Condition == 2))$MSE_Lappe))
RelLikelihood_Lappe_Treadmill = exp( (AIC_Lappe_All_Treadmill - AIC_Slope_All_Treadmill) / 2 )
RelLikelihood_Lappe_Treadmill



#Is Lappe a better fit for Visual + Treadmills or other Conditions?
exp((AIC_Lappe_All_Visual_Treadmills - AIC_Lappe_All_Treadmill) / 2)
exp((AIC_Lappe_All_Visual_Treadmills - AIC_Lappe_All_Visual) / 2)
exp((AIC_Lappe_All_Visual - AIC_Lappe_All_Treadmill) / 2)


write.csv(Data5, "Lappe_Linear.csv")



##plotting
ggplot(Data_Temp, aes(Actual_Travel_Distance,Perceived_Travel_Distance, color = Task, legend.position="none")) +
  geom_point()
ggplot(Data_Temp,aes(Distance_Real, Perceived_Travel_Distance - Prediction_Slope)) +
  geom_point()
ggplot(Data2,aes(Distance_Real, Data_MTT - Prediction_Lappe_MTT)) +
  geom_point() +
  ylim (-40,20)
ggplot(Data_Temp,aes(Distance_Real, Perceived_Travel_Distance - Prediction_Slope)) +
  geom_point()
  #ylim (-25,60)
ggplot(Data2,aes(Distance_Real, Data_AT - Prediction_Lappe_AT)) +
  geom_point() +
  ylim (-25,60)

##Figure4
ggplot(Data5,aes(as.factor(Condition), MSE_Lappe, color = as.factor(Condition))) +
  geom_boxplot() +
  labs (x = "Condition", y = "Mean Squared Error (m)") +
  scale_x_discrete(labels = c("Visual + Treadmill", "Visual", "Treadmill")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0,175)) +
  scale_color_manual(values = c("red", "blue", "forestgreen")) 

long_Data5 <- pivot_longer(Data5, cols = starts_with("MSE"), names_to = "Model", 
                        values_to = "MSE")

ggplot(long_Data5,aes(as.factor(Condition), MSE, color = as.factor(Model))) +
  geom_boxplot() +
  labs (x = "Condition", y = "Mean Squared Error (m)") +
  scale_x_discrete(labels = c("Visual + Treadmill", "Visual", "Treadmill")) +
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim = c(0,150)) +
  scale_color_manual(name = "Models", labels = c("LSI", "Linear"), values = c("orange", "purple")) 


##Lappe Gains
Data7 = Data5%>% 
  group_by(Condition) %>%
  summarise_at(vars(LappeGain), list(LappeGain_mean = mean, SD = sd))

ggplot(Data5,aes(as.factor(Condition), LappeGain, color = as.factor(Condition))) +
  geom_boxplot() +
  labs (x = "Condition", y = "Mean Lappe Gain") +
  scale_x_discrete(labels = c("Visual + Treadmill", "Visual", "Treadmill")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen"))

##Lappe Decays
Data8 = Data5%>% 
  group_by(Condition) %>%
  summarise_at(vars(LappeDecay), list(LappeDecay_mean = mean, SD = sd))

ggplot(Data5,aes(as.factor(Condition), LappeDecay, color = as.factor(Condition))) +
  geom_boxplot() +
  labs (x = "Condition", y = "Mean Lappe Decay") +
  scale_x_discrete(labels = c("Visual + Treadmill", "Visual", "Treadmill")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen"))


Data6 = Data5%>%
  group_by(Distance_Real, Condition) %>%
  summarise_at(vars(MSE_Lappe), list(MSE_Lappe_mean = mean, SD = sd))

ggplot(Data6,aes(Distance_Real, MSE_Lappe_mean, color=as.factor(Condition))) +
  geom_point(size=2) +
  geom_line(size=1) +
  # geom_ribbon(aes(x = Distance_Real,
  #                 ymin = MSE_Lappe_mean - SD,
  #                 ymax = MSE_Lappe_mean + SD),
  #             fill = ,
  #             alpha = 0.2) +
  labs (x = "Distance (m)", y = "Squared Error (m)") +
  theme(text = element_text(size = 20)) +
  scale_color_manual(name = "Condition", labels = c("Visual + Treadmill", "Visual", "Treadmill"), values = c("red", "blue", "forestgreen"))
