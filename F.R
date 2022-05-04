packages <- c("tidyverse","stargazer","AER","asbio","tigerstats","readxl","foreign","wooldridge","moderndive","gridExtra","haven","car") ## This is how you define an object (which is a vector here)
install.packages(packages, repos='http://cran.us.r-project.org') # Installing packages at once
lapply(packages, library, character.only = T) # Loading the packages
terrorism_data <- read_csv("C:/Users/Matthew/Desktop/terrorism_data.csv")

terrorism_data<-terrorism_data%>%
  filter(!is.na(gdppc)&ftmpop>0)

#Part 1
#a
ggplot(data=terrorism_data,aes(x=gdppc,y=ftmpop))+geom_point()

#b
terrorism_data_1<-terrorism_data%>%
  mutate(lnftmpop=log(ftmpop),lngdppc=log(gdppc))
ggplot(terrorism_data_1,aes(x=lngdppc,y=lnftmpop))+geom_point()

#c
ggplot(terrorism_data_1,aes(x=lngdppc,y=lnftmpop))+geom_point()


#Part 2

terrorism_data_1=terrorism_data_1%>%
  mutate(lngdppc2=(lngdppc)^2,lackpf2=(lackpf)^2)

#Regression

F_1 <- lm(data = terrorism_data_1, lnftmpop ~ lngdppc)
F_2 <- lm(data = terrorism_data_1, lnftmpop ~ lngdppc +lackpf)
F_3 <- lm(data = terrorism_data_1, lnftmpop ~ lngdppc+lngdppc2+lackpf+lackpf2)
F_4 <- lm(data = terrorism_data_1, lnftmpop ~ lngdppc+lngdppc2+lackpf+lackpf2+ethnic+religion)
F_5 <- lm(data = terrorism_data_1, lnftmpop ~ lngdppc+lngdppc2+lackpf+lackpf2+ethnic+religion+mideast+latinam+easteurope+eastasia+africa)

stargazer(F_1, F_2,F_3,F_4,F_5,type = "text",dep.var.labels = c("ftmpop"), title = "Terrorism Factors", style = "qje",notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))

#Part 3

linearHypothesis(F_3,c("lngdppc=0","lngdppc2=0"))
linearHypothesis(F_3,c("lngdppc=0","lngdppc2=0"),white.adjust="hc1")

linearHypothesis(F_5,c("latinam+0","easteurope=0","africa=0","eastasia=0"))
linearHypothesis(F_5,c("latinam+0","easteurope=0","africa=0","eastasia=0"),white.adjust="hc1")
