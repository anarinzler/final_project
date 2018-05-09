
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
  ggplot(., aes(x = Participant, y = Dur, color = TrainingGroup)) +
  geom_point() +
  geom_smooth(method = 'lm')
  facet_grid(. ~ VoicedUnvoiced)
  labs(x = "Training", y = "Vowel Duration(ms)", title = 
         "Effects of Training on Vowel Duration", fill = 
         "Training Group")

?spread
?gather
  
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