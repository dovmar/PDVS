## ---- include=FALSE-----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE,message=FALSE)
options("digits" = 5)


## -----------------------------------------------------------------------------------------------------------------------------
library(tidyverse)

# Duomenų įvesties klaidos (sutvarkysiu pačiame duomenų faile)
#lines <-readLines("Future-500-7.csv")
#lines[69]<- str_replace(lines[69],'\"',"")
#lines[79]<- str_replace(lines[79],'\"',"")

#writeLines(lines,"modified_csv.csv")
x <- read_csv("modified_csv.csv")


## -----------------------------------------------------------------------------------------------------------------------------
# Pasivertimas į skaitinius kintamuosius
x_1 <- x %>%
  mutate(Revenue = as.numeric(str_replace_all(Revenue,"\\$|\\,","")),
        Expenses = as.numeric(str_replace_all(Expenses," Dollars|\\,","")),
        Growth = as.numeric(str_replace_all(Growth,"%","")),
        Profit = as.numeric(str_match(Profit,"\\d+")))


## -----------------------------------------------------------------------------------------------------------------------------
library(psych)
x_1 %>% select(where(is.numeric),-"ID") %>% describe()



x_grouped <- x_1 %>% group_by(Industry)
names <- x_grouped %>% group_keys() %>% pull(Industry)

summary_list <- x_grouped %>% select(where(is.numeric)) %>% select(-"ID") %>% 
  group_split() %>% 
  purrr::map(~select(.x,-"Industry")) %>%
  purrr::map(describe) %>%
  purrr::map(~rownames_to_column(as.data.frame(.x)))

names(summary_list) <- names
summary_list


## -----------------------------------------------------------------------------------------------------------------------------
# išveda į failus, siekiant nukopijuoti į word lentelę
x %>% describe %>% select(c("sd","mean","median","min","max")) %>% round(2) %>% write.csv("out.csv",quote=FALSE)


temp <- summary_list %>% enframe() %>% unnest_longer("value") 

cbind(temp$name,temp$value) %>% select(c("rowname","temp$name","sd","mean","median","min","max")) %>% mutate(across(where(is.numeric),round,2)) %>% write.csv("out_2.csv",quote=FALSE,row.names = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------
# turimi vienų metų duomenys. esant praeitų metų duomenims NA reikšmes būtų galima pakeisti praeitomis
x_1 %>% group_by(Name) %>% count() %>% arrange(desc(n))

x_1 %>% ungroup() %>% summarize(across(everything(),~sum(is.na(.x)))) # pradiniai kiekiai praleistų reikšmių


replace_with_group_median<- function(x,y) {
  group_median <- median(x,na.rm = TRUE)
  if_else(is.na(x),group_median,x)
}


library(maps)
cities <-  us.cities$country.etc
names(cities) <- str_replace(us.cities$name,paste("",us.cities$country.etc),"")


x_2 <- x_1 %>%
  # faktinis užpildymas
  mutate(State = if_else(is.na(State),cities[City],State)) %>%
  # išvestinės reikšmės 
  mutate(Expenses = if_else(is.na(Expenses) & !is.na(Profit),Revenue - Profit,Expenses),
        Revenue = if_else(is.na(Revenue) & !is.na(Profit),Expenses + Profit,Revenue)) %>%
  group_by(Industry) %>%
  filter(!(is.na(Revenue) & is.na(Expenses) & !is.na(Profit))) %>%
  mutate(Expenses = replace_with_group_median(Expenses),
         Revenue = replace_with_group_median(Revenue),
         Profit = Revenue - Expenses) %>%
  rbind(x_1  %>% filter((is.na(Revenue) & is.na(Expenses) & !is.na(Profit)))) %>%
  mutate(Employees = replace_with_group_median(Employees),
         Growth = replace_with_group_median(Growth)) %>%
  ungroup()




## -----------------------------------------------------------------------------------------------------------------------------
# likusios praleistos reikšmės paliekamos duomenyse (daugiausia reikšmės nominaliuose kintamuosiouse)
x_2 %>% summarize(across(everything(),~sum(is.na(.x))))


## -----------------------------------------------------------------------------------------------------------------------------
names <- c("Employees","Revenue","Expenses","Profit","Growth")

x_2 %>%  select(names) %>% purrr::map(~boxplot.stats(.x,coef = 1.5)$out) # sąlyginės išskirtys ("mild" outliers)

(outliers <- x_2 %>% select(names) %>% purrr::map(~boxplot.stats(.x,3)$out)) # išskirtys ("extreme" outliers)


## -----------------------------------------------------------------------------------------------------------------------------
ggplot(x_2,aes(Employees)) + geom_histogram() + theme_minimal() # įmonės darbuotojų skaičiaus pasiskirstymas yra stiprios dešininės asimetrijos (right skewed)
# toliau pašalinsiu šias išskirtis


x_2 %>% filter(Employees %in% outliers$Employees)
# išsiskiriančios įmonės t.y. tyrimo objektai. kai kurios iš šių įmonių turi ne tik didelius darbuotojų kiekis, bet ir didelius Expenses/Revenue

x_2 %>% filter(Employees %in% outliers$Employees) %>% count(Industry) 

x_3 <- x_2 %>% filter(!Employees %in% outliers$Employees)


## -----------------------------------------------------------------------------------------------------------------------------
x_3 %>% select(names) %>% purrr::map(~boxplot.stats(.x,3)$out) # daugiau išskirčių pagal dominančius stulpelius nerasta


## -----------------------------------------------------------------------------------------------------------------------------
# Kaip skiriasi imties statistiniai duomenys pašalinus išskirtis
summary_1 <- x_2 %>% select(where(is.numeric),-"ID") %>% describe()

summary_2 <- x_3 %>% select(where(is.numeric),-"ID") %>% describe()

(summary_2 - summary_1) / summary_1 * 100 # procentinis imties statistinių duomenų pokytis pašalinus išskirtis


## -----------------------------------------------------------------------------------------------------------------------------
# normalizavimas
normalized <- x_3 %>% select(where(is.numeric),-c("ID","Inception")) %>% drop_na() %>% map_df(~((.x-min(.x))/(max(.x)-min(.x)))) 
# standartizavimas
standartized <- x_3 %>% select(where(is.numeric),-c("ID","Inception")) %>% drop_na() %>% map_df(~(.x-mean(.x))/sd(.x)) 


## -----------------------------------------------------------------------------------------------------------------------------
normalized  %>% pivot_longer(1:5) %>% ggplot(aes(value,color=name)) + geom_boxplot() + coord_flip() + theme_minimal() + scale_color_viridis_d()

standartized  %>% pivot_longer(1:5) %>% ggplot(aes(value,color=name)) + geom_boxplot() + coord_flip() + theme_minimal() + scale_color_viridis_d()

x_3 %>% select(where(is.numeric),-c("ID","Inception")) %>% pivot_longer(1:5) %>% ggplot(aes(value,color=name)) + geom_boxplot() + coord_flip() + theme_minimal() + scale_color_viridis_d()


## -----------------------------------------------------------------------------------------------------------------------------
library(corrplot)
x_corr <- x_3[,-1] %>% drop_na()

numerical <- unlist(lapply(x_corr, is.numeric))  
correlation_matrix <- cor(as.matrix(x_corr[,numerical]))
correlation_matrix


corrplot(correlation_matrix, order = "FPC", method = "color",type="upper",diag=FALSE,tl.col = "black", addCoef.col = "black")


## -----------------------------------------------------------------------------------------------------------------------------
length(unique(x_3$Industry)) # 7 industrijos

x_industry <- x_3 %>% drop_na()


x_industry %>% ggplot(aes(x=Profit,fill=Industry)) + geom_histogram(aes(y=after_stat(density)),bins = 12) + facet_wrap(vars(Industry)) + scale_fill_viridis_d() + theme_minimal() + scale_y_continuous(n.breaks = 5)

x_industry %>% ggplot(aes(x=Industry,y=Employees,fill=Industry)) + stat_summary(fun=mean,geom="bar") + scale_fill_viridis_d() + theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
x_industry %>% ggplot(aes(Revenue,Expenses,color=Industry)) + geom_point(aes(size=Profit),alpha=0.7) + 
  scale_color_viridis_d() + geom_abline(slope=1,intercept=0) + theme_minimal()

min(x_3$Profit)


## -----------------------------------------------------------------------------------------------------------------------------
library(datasets)
states <-state.region
names(states) <- state.abb

x_regions <- x_3 %>% mutate(Region = states[State]) %>% drop_na()


## -----------------------------------------------------------------------------------------------------------------------------
x_regions %>% ggplot(aes(Region,Growth,fill=Region)) + geom_violin(draw_quantiles = 0.5) + theme_minimal() + scale_fill_brewer(palette = "Set2")

x_regions %>% ggplot(aes(Region,Profit,fill=Region)) + geom_violin(draw_quantiles = 0.5) + theme_minimal() + scale_fill_brewer(palette = "Set2")



x_regions %>% ggplot(aes(Region,fill=Region))+ geom_bar() + scale_fill_brewer(palette = "Set2") + theme_minimal()

x_regions %>% ggplot(aes(Region,fill=Industry))+ geom_bar(position="fill")+ coord_flip() +
  scale_y_continuous(labels=scales::label_percent()) + scale_fill_viridis_d() + theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
x_regions %>% ggplot(aes(Revenue,Expenses,color=Region)) + geom_point(aes(size=Profit),alpha=0.8) + 
  scale_color_brewer(palette="Set2") + geom_abline(slope=1,intercept=0) + theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
x_regions %>% drop_na() %>% ggplot(aes(Inception,color=Industry)) + stat_ecdf() + facet_wrap(vars(Industry)) + theme_minimal() + scale_color_viridis_d()

