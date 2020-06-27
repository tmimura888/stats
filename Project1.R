install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(ggplot2)


read_excel("basketball.xlsx")
basketball <- read_excel("basketball.xlsx")

head(basketball)
str(basketball)
names(basketball)
summary(basketball)



mod1 <- lm(Salary~Age, data=basketball)
summary(mod1)
  



ggplot(data=basketball, aes(x=Salary, y=Guaranteed))+
  geom_jitter()+
  geom_smooth()

summary(basketball)

hist(basketball$Salary)

pairs(~Salary + Guaranteed + Age + Player_Efficiency_Rating + 
        True_Shooting_Percentage + Three_Point_Field_Goal_Percentage + 
        basketball)

pairs(~Salary + Free_Throw_Percentage + Offensive_Rebound_Percentage + Defensive_Rebound_Percentage + 
        Total_Rebound_Percentage + Assist_Percentage, basketball)


pairs(~Salary + Steal_Percentage + Block_Percentage +   Turnover_Percentage + Usage_Percentage + 
        Offensive_Win_Shares, basketball)

pairs(~Salary + Defensive_Win_Shares  +  Win_Shares + Win_Shares_Per_48_Minutes 
      + Offense_Box_Plus_Minus, basketball)

pairs(~Salary + Defense_Box_Plus_Minus + Box_Plus_Minus + Value_Over_Replacement_Player, basketball)




