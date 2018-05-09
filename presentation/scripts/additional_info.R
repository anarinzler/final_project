
library(tidyverse)
library(xaringan)
library(ggfortify)
library(broom)
library(kableExtra)

lang_df <- read.csv("/Users/anarinzler/Desktop/cantonese_final/raw data/raw_data.csv")



tidy_df <- lang_df %>%
  select(., Participant, TrainingGroup, Word, VoicedUnvoiced, Position, Duration)


tidy_df %>% 
  ggplot(., aes(x = (VoicedUnvoiced), y = Dur, color = TrainingGroup)) +
  geom_point() +
  geom_jitter()+
  geom_smooth(method = 'lm')
  labs(x = "Training", y = "Vowel Duration(ms)", title = "Effects of Training on Vowel Duration", 
color = "Voiced or Unvoiced")
sum_df <- tidy_df %>%
  mutate(., trainingSum = if_else(TrainingGroup == "Yes", true = 1, false = -1)) %>%
  mutate(., voicingSum = if_else(VoicedUnvoiced == "voiced", true = 1, false = -1))

```{r, message=FALSE, include=FALSE, echo=FALSE}
anova(mod_inc, mod_add) # test int
anova(mod_add, mod_voice) # test training
anova(mod_add, mod_train) # test voicing
```


tidy_df %>% 
  ggplot(., aes(x = TrainingGroup, y = Duration, fill = VoicedUnvoiced)) +
  geom_boxplot() +
  labs(x = "Training", y = "Vowel Duration", title = "Effects of Training on Vowel Duration", 
       fill = "Voiced or Unvoiced") 


# explain data -- looks like training group has lower than no trained to begin with 

tidy_df %>% 
  ggplot(., aes(x = Word, y = Dur, color = TrainingGroup)) +
  geom_boxplot() +
  geom_smooth(method = 'lm')
  facet_grid(. ~ VoicedUnvoiced)
  labs(x = "Training", y = "Vowel Duration(ms)", title = 
         "Effects of Training on Vowel Duration", fill = 
         "Training Group")

?spread
?gather
  # Model Residuals
  - Should be normally distributed
  - Absolute values of 1Q and 3Q should be similar
  - Mean = 0, median close to 0
  
  ```{r, message=FALSE}
  summary(mod_inc$residuals)
  
  ---
    # Testing Model Assumptions
    
    --
    ### Autocorrelation of Residuals
    
    ```{r, message=FALSE, echo=FALSE, fig.height = 7}
  acf(mod_full$residuals)
  
#This is a measure of the variability
  for each random effect that you added to the model. You can see that scenario
  (“item”) has much less variability than subject. Based on our boxplots from above,
  where we saw more idiosyncratic differences between subjects than between
  items, this is to be expected. Then, you see “Residual” which stands for the
  variability that’s not due to either scenario or subject. This is our “ε” again, the
  “random” deviations from the predicted values that are not due to subjects and
  items. Here, this reflects the fact that each and every utterance has some factors
  that affect pitch that are outside of the purview of our experiment.

  Likelihood Ratio Test as a means to attain p-values.
  Likelihood is the probability of seeing the data you collected given your model.
  The logic of the likelihood ratio test is to compare the likelihood of two models
  with each other
  
  tidy(.) %>% 
    kable(., format = 'html', digits = 3) %>% 
    kable_styling(., full_width = FALSE, font_size = 16)
  
  library(Matrix)
  library(MuMIn)
  library(likelihood)
  library(lmerTest)
  library(doBy)
  
  # Boxplot with POA
  ```{r, boxplot with POA, message=FALSE,fig.retina=2, fig.align='center', fig.width=9,fig.height= 6, echo=FALSE, warning=FALSE}
  tidy_df %>% 
    ggplot(., aes(x = POA, y = Dur, fill = TrainingGroup)) +
    geom_boxplot() +
    facet_grid(. ~ VoicedUnvoiced)
  labs(x = "Training", y = "Vowel Duration(ms)", title = 
         "Effects of Training on Vowel Duration", fill = 
         "Training Group")
  ```
  
dum_df[!complete.cases(dum_df),]

group_by(., dur) %>% 
  summarize(voice_mean = mean(VoicedUnvoiced), voic_sd = sd(VoicedUnvoiced), 
            train_mean = mean(TraingingGroup), train_sd = sd(TraingingGroup)) %>% 
  knitr::kable(., format = 'markdown')
?mutate

lm(Dur ~ trainingSum, data = sum_df)
lm(Dur ~ voicingSum, data = sum_df)
lm(Dur ~ voicingSum + trainingSum, data = sum_df)

```{r, boxplot with POA, message=FALSE,fig.retina=2, fig.align='center', fig.width=8,fig.height= 4, echo=FALSE}
tidy_df %>% 
  ggplot(., aes(x = POA, y = Dur, fill = TrainingGroup)) +
  geom_boxplot() +
  facet_grid(. ~ VoicedUnvoiced)
labs(x = "Training", y = "Vowel Duration(ms)", title = 
       "Effects of Training on Vowel Duration", fill = 
       "Training Group")
```

qqnorm(residuals(mod_inc))
qqline(residuals(mod_inc))

autoplot(mod_inc, which = 2)

summary(mod_inc$residuals)

```{r, message=FALSE, warning=FALSE, fig.retina=2, fig.align='center', fig.width=8,fig.height= 4, echo=FALSE}
tidy_df %>% 
  ggplot(., aes(x = Participant, y = Dur, color = TrainingGroup)) +
  geom_point() +
  geom_smooth(method = 'lm')
labs(x = "Participant", y = "Vowel Duration(ms)", color = 
       "Training Group")
```

#mixed effects modeling
#lmer() or glmer() from the lme4 package 
#lmer(criterion ~ fixed_effect + (1|random_effect), data = my_data)

#lmer(response ~ time + (1|subjects), data = my_df)


anova(my_null, my_int) # test int
anova(my_null, my_voice) # test training
anova(my_null, my_train)


mod_full <- lmer(Dur ~ voicingSum * trainingSum + (1 + trainingSum | Participant) + (1 + trainingSum | Word), data = sum_df)

#centering
  mutate(., Dur - mean(Dur))
save(sum_df,file="sum_df.Rda")

```{r, message=FALSE, warning=FALSE, include=FALSE}
mod_inc <- lm(Dur ~  voicingSum * trainingSum, data = sum_df)  # inclusive model, R2 = .05347, .04906
summary(mod_inc)

mod_add <- lm(Dur ~ voicingSum + trainingSum, data = sum_df)  # nested model, add, R2 = .04918, .04623
summary(mod_add)
#no voicing
mod_voice <- lm(Dur ~ voicingSum, data = sum_df)       
# nested model, no training, R2 = .04918, .04623
summary(mod_voice)
# R2 = .04588, .04441
mod_train <- lm(Dur ~ trainingSum, data = sum_df)                    # nested model, no voicing, R = .00337, .001827
summary(mod_train)

mod_nul <- lm(Dur ~ 1, data = sum_df)
summary(mod_nul)


why find this?
  evidence on vowel length being difficult
exposure could have reduced group differences 

my_null <- lmer(Dur ~ 1 + (1|Participant), data = sum_df)
summary(my_null)
my_train <- lmer(Dur ~ trainingSum + (1|Participant), data = sum_df)
summary(my_train)
my_voice <- lmer(Dur ~ voicingSum + (1|Participant), data = sum_df)
summary(my_voice)
my_int <- lmer(Dur ~ voicingSum * trainingSum + (1|Participant), data = sum_df)
summary(my_int)

sum_df %>% 
  ggplot(., aes(x = VoicedUnvoiced, y = durNorm, fill = as.factor(Participant)))+
  geom_boxplot()+
  facet_grid(. ~ TrainingGroup) +
  labs(x = "Voiced or Unvoiced", y = "Vowel Duration(ms)", title = 
         "Effects of Training on Vowel Duration", 
       fill = "Participant")