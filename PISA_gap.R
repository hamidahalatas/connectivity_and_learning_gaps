library(haven)
library(intsvy)
library(expss)
library(tidyverse)

library(wesanderson)   # for web applications

setwd("/Users/hamidah/Documents/School/DA4SS/Data_Analysis_Social_Science/01_data/")
mydata <- read_dta("PISA2018_merge_All.dta")
pisadata <- mydata

pisadata$icts_computer <- factor(pisadata$icts_computer,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))
pisadata$icts_tablet <- factor(pisadata$icts_tablet,
                                 levels = c(0,1),
                               labels = c("No", "Yes"))
pisadata$icts_ebook_reader <- factor(pisadata$icts_ebook_reader,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))
pisadata$icts_internet <- factor(pisadata$icts_internet,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))
pisadata$icts_computer_school <- factor(pisadata$icts_computer_school,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))
pisadata$icts_smartphone <- factor(pisadata$icts_smartphone,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))


  collapse_1 <- pisadata %>% 
    group_by(cnt,icts_smartphone) %>%
    select(starts_with('pv')) %>%
    summarise_all(funs('mean' = mean),na.rm = TRUE) %>%
    mutate(var = "icts_smartphone", value = icts_smartphone) %>%
    select(cnt, value, var, starts_with("pv"))

  collapse_2 <- pisadata %>% 
    group_by(cnt,icts_internet) %>%
    select(starts_with('pv')) %>%
    summarise_all(funs('mean' = mean),na.rm = TRUE) %>%
    mutate(var = "icts_internet", value = icts_internet) %>%
    select(cnt, value, var, starts_with("pv"))

  collapse_3 <- pisadata %>% 
    group_by(cnt,icts_computer_school) %>%
    select(starts_with('pv')) %>%
    summarise_all(funs('mean' = mean),na.rm = TRUE) %>%
    mutate(var = "icts_computer_school", value = icts_computer_school) %>%
    select(cnt, value, var, starts_with("pv"))
  
  collapse_4 <- pisadata %>% 
    group_by(cnt,icts_tablet) %>%
    select(starts_with('pv')) %>%
    summarise_all(funs('mean' = mean),na.rm = TRUE) %>%
    mutate(var = "icts_tablet", value = icts_tablet) %>%
    select(cnt, value, var, starts_with("pv"))
  
  collapse_5 <- pisadata %>% 
    group_by(cnt,icts_ebook_reader) %>%
    select(starts_with('pv')) %>%
    summarise_all(funs('mean' = mean),na.rm = TRUE) %>%
    mutate(var = "icts_ebook_reader", value = icts_ebook_reader) %>%
    select(cnt, value, var, starts_with("pv"))
  
all <- rbind(collapse_1, collapse_2, collapse_3,
             collapse_4, collapse_5)
all <- all %>% filter(!is.na(value))
write_csv(all,"rbind.csv")

rbind_clean <- read_csv2("rbind_clean.csv")
rbind_clean <- rename(rbind_clean, c("value" = "group",
                                     "math" = "Math",
                                     "read" = "Reading",
                                     "science" = "Science"))
rbind_clean <- rbind_clean %>%
  mutate(var = ifelse(var=="icts_computer_school","Has Computer",
                      ifelse(var=="icts_smartphone","Has Smartphone",
                             ifelse(var=="icts_internet","Link to Internet",
                                    ifelse(var=="icts_tablet","Has Tablet","Has Ebook Reader")))))
rbind_clean$Math <- as.numeric(rbind_clean$Math)
rbind_clean$Reading <- as.numeric(rbind_clean$Reading)
rbind_clean$Science <- as.numeric(rbind_clean$Science)

rbind_clean_ind <- filter(rbind_clean,cnt=="IDN") %>% select(-cnt)
rbind_clean_sg <- filter(rbind_clean,cnt=="SGP") %>% select(-cnt)
rbind_clean_ita <- filter(rbind_clean,cnt=="ITA") %>% select(-cnt)

library(reshape2)
rbind_clean_ind_l <- melt(rbind_clean_ind, id.vars = c("var", "group"))
rbind_clean_ind_l <- rbind_clean_ind_l %>% na.omit()

ggplot(rbind_clean_ind_l, aes(y=value, x=group, fill=as.factor(group))) + 
  geom_bar(stat="identity")  +
  theme(text = element_text(size=10)) +
  facet_grid(variable ~ var) + 
  ggtitle("Indonesia") +
  ylab("PISA score") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values=rev(wes_palette("Zissou1",3)))






rbind_clean_ita_l <- melt(rbind_clean_ita, id.vars = c("var", "group"))
rbind_clean_ita_l <- rbind_clean_ita_l %>% na.omit()

ggplot(rbind_clean_ita_l, aes(y=value, x=group, fill=as.factor(group))) + 
  geom_bar(stat="identity")  +
  theme(text = element_text(size=10)) +
  facet_grid(variable ~ var) + 
  ggtitle("Italy") +
  ylab("PISA score") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values=rev(wes_palette("Zissou1",3)))




rbind_clean_sg_l <- melt(rbind_clean_sg, id.vars = c("var", "group"))
rbind_clean_sg_l <- rbind_clean_sg_l %>% na.omit()

ggplot(rbind_clean_sg_l, aes(y=value, x=group, fill=as.factor(group))) + 
  geom_bar(stat="identity")  +
  theme(text = element_text(size=10)) +
  facet_grid(variable ~ var) + 
  ggtitle("Singapore") +
  ylab("PISA score") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values=rev(wes_palette("Zissou1",3)))

rbind_clean_sg %>% select(-Reading,-Science) %>%
  spread(group, Math) %>%
  mutate(diff = (Yes-No)/Yes*100) # 4.1,15.7
rbind_clean_sg %>% select(-Math,-Science) %>%
  spread(group, Reading) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.17 16.1
rbind_clean_sg %>% select(-Math,-Reading) %>%
  spread(group, Science) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.16 16.4



rbind_clean_ind %>% select(-Reading,-Science) %>%
  spread(group, Math) %>%
  mutate(diff = (Yes-No)/Yes*100) # 4.1,15.7
rbind_clean_ind %>% select(-Math,-Science) %>%
  spread(group, Reading) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.17 16.1
rbind_clean_ind %>% select(-Math,-Reading) %>%
  spread(group, Science) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.16 16.4



rbind_clean_ita %>% select(-Reading,-Science) %>%
  spread(group, Math) %>%
  mutate(diff = (Yes-No)/Yes*100) # 4.1,15.7
rbind_clean_ita %>% select(-Math,-Science) %>%
  spread(group, Reading) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.17 16.1
rbind_clean_ita %>% select(-Math,-Reading) %>%
  spread(group, Science) %>%
  mutate(diff = (Yes-No)/Yes*100) #4.16 16.4

pisadata2 <- pisadata %>% filter(cnt!="ESP")
a <- table(pisadata2$cnt)
b <- names(a)

mat_coef <- matrix(c(NA),length(b),10)
read_coef <- matrix(c(NA),length(b),10)
sci_coef <- matrix(c(NA),length(b),10)

i <- 1
for (var in b){
data_ind <- pisadata2 %>%
  filter(cnt==var) %>% 
  select(icts_internet, irt_escs, school_location, lesson_minute,
         contains("math"), contains("read"), contains("sci")) %>%
  select(icts_internet, irt_escs, school_location, lesson_minute, starts_with("pv"))

subs <- data_ind %>% 
  select(starts_with("pv"), irt_escs, lesson_minute, school_location, icts_internet)
subs <- sapply(subs,as.numeric)
data_ind <- data_ind %>% select(-starts_with("pv"),-irt_escs, -lesson_minute, -school_location, -icts_internet)
data_ind <- cbind(data_ind,subs) 
data_ind <- data_ind %>% mutate(log_minute = log(lesson_minute))
log <- mean(data_ind$log_minute,na.rm = TRUE)
if(is.na(log) == FALSE){ 
  for(j in 1:10){
    reg_m <- lm(data_ind[[paste0("pv",j,"math")]] ~ 
                data_ind[["icts_internet"]] +
                data_ind[["irt_escs"]] +
                data_ind[["log_minute"]])
    mat_coef[i,j] <- coef(reg_m)['data_ind[["icts_internet"]]'] 
    
    reg_r <- lm(data_ind[[paste0("pv",j,"read")]] ~ 
                  data_ind[["icts_internet"]] +
                  data_ind[["irt_escs"]] +
                  data_ind[["log_minute"]])
    read_coef[i,j] <- coef(reg_r)['data_ind[["icts_internet"]]'] 
    
    reg_s <- lm(data_ind[[paste0("pv",j,"scie")]] ~ 
                  data_ind[["icts_internet"]] +
                  data_ind[["irt_escs"]] +
                  data_ind[["log_minute"]])
    sci_coef[i,j] <- coef(reg_s)['data_ind[["icts_internet"]]'] 
  }
} else {
  for(j in 1:10){
    mat_coef[i,j] <- NA
    read_coef[i,j] <- NA
    sci_coef[i,j] <- NA
  }
  }
i <- i + 1
}


math <- rowMeans(mat_coef, na.rm = TRUE)
read <- rowMeans(read_coef, na.rm = TRUE)
sci <- rowMeans(sci_coef, na.rm = TRUE)

final_mat <- cbind(math,read,sci)
rownames(final_mat) <- b

final_mat <- as.data.frame(final_mat)
final_mat$math <- round(final_mat$math,1)
final_mat$read <- round(final_mat$read,1)
final_mat$sci <- round(final_mat$sci,1)

write.csv(final_mat,"final_mat.csv")
final_mat$cnt <- rownames(final_mat)


library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(countrycode)   # load data 

library(tmap)    # for static and interactive maps

final_mat$iso_a2   <- countrycode(final_mat$cnt, origin = "iso3c", destination = "iso2c")
final_mat$iso_a2[final_mat$cnt=="KSV"] <- "XK"
final_mat$iso_a2[final_mat$cnt=="QAZ"] <- "AZ"
final_mat$iso_a2[final_mat$cnt=="QCI"] <- "CN"
final_mat$iso_a2[final_mat$cnt=="QMR"] <- "RU"
final_mat$iso_a2[final_mat$cnt=="QRT"] <- "RU"
final_mat$iso_a2[final_mat$cnt=="TAP"] <- "RU"

world2 = world %>% dplyr::select(iso_a2, name_long, geom)

final_mat_join = left_join(world2, final_mat)
final_mat_join = final_mat_join %>% filter(!is.na(math))

legend_title = expression("PISA Math")
pal <- wes_palette("Zissou1", 100, type = "continuous")
tm_shape(final_mat_join) + 
  tm_polygons(col = "math", palette = pal, n = 6, title = legend_title) +
  tm_layout(main.title = "Source: PISA, 2018", main.title.size = 0.7, main.title.position = "right")

