# Falco and Zamparelli (2024), Discourse-Linked DPs as Covert Partitives

dir.create("../figures/regenerated", showWarnings = FALSE, recursive = TRUE)

# libraries 

library(tidyverse)
library(patchwork)
library(ggpubr)
library(rstatix)
library("viridis")

#library(reprex)

my_colors <- c(viridis(3))# ("#00AFBB", "#E7B800", "#FC4E07")

# Chosing font size and family for all plots
theme_update(text = element_text(size=14, family="Times"))

# Preprocessing English ---------------------------------------------------------------------

# Run from scripts/. The input files contain study-local participant labels.
dfe <- as_tibble(read_csv("../data/dfe.csv", show_col_types = FALSE))

# Gather data into 3 column format: "linking", "DPType" and "judgment"

dfe %>%
  pivot_longer(!participant, names_pattern = c("([a-zA-Z]{2,3}:)([a-zA-Z]+_*\\.*[a-zA-Z]*_*\\.*[a-zA-Z]*)"), 
               names_to = c("linking", "DPType"),
               values_to = "Judgment") %>%
  mutate(linking = str_replace(linking, ":", "")) %>% 
  rename(Linking = linking)  -> melt.dfe2

# Create 2 new variables -------------------------------------------------------------------

melt.dfe2 %>% 
  mutate(Det = case_when(DPType == "Num_V" | DPType == "V_num.obj" ~ "Numeral",
                         DPType == "A.quarter_V" | DPType == "V_a.quarter" | DPType == "" ~ "A_quarter",
                         DPType == "One.quarter_V" | DPType == "V_one.quarter" ~ "One_quarter",
                         DPType == "Half_V" | DPType == "V_half.obj" ~ "Half",
                         TRUE ~ "Other_Half")) %>%
   mutate(Order = case_when(str_detect(DPType, "^V") ~ "Subject",
                           TRUE ~ "Object")) %>%
   mutate(Det = factor(Det, levels = c("Numeral", "Half", "One_quarter", "A_quarter", "Other_Half")))  %>%
   mutate(Order = factor(Order, levels=c("Subject", "Object"))) -> melt.dfe3

# NUMERALS IN ENGLISH - PART OF FIG. 1 -----------------------------------------------------

# Extract only Num_V and and V_num.obj DPTypes

melt.dfe3 %>%
  filter(Det == "Numeral") -> melt.dfe3.order.num 

melt.dfe3.order.num %>%
  ggplot(aes(x=Order, y=Judgment, fill=Order))+
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5) + 
  scale_fill_manual(values = my_colors)+
  scale_x_discrete("Numeral position")+
  scale_y_continuous("Judgement / Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
  facet_wrap(~Linking)+
  theme(legend.position = "none")+
  ggtitle("English")  -> p.melt.dfe3.num # will used below 

# nDL IN ENGLISH - fig. 2 ------------------------------------------------------------------------             
              
# Extract nDL linking 

melt.dfe3 %>%
  filter(Det == "Numeral" | Det == "Half" |  Det == "One_quarter", Linking == "nDL") -> melt.dfe3.ndl

# Plot English nDL 

melt.dfe3.ndl %>%
  ggplot(aes(Order, Judgment, fill=Order))+ 
  stat_summary(fun = "mean",
               geom = "bar")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5) + 
  scale_fill_manual(values = my_colors)+
  scale_x_discrete("DP position")+
  scale_y_continuous("Acceptabilty",  expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
  theme(legend.position="none")+
  facet_grid(~Det, scales = 'free') -> p.melt.dfe3.ndl
# ggtitle("Mean ratings by DP type and linking for English")  

# Saving 

ggsave("../figures/regenerated/fig2.png", plot = p.melt.dfe3.ndl, height = 5.5, width = 8.5)

# HALF nDL IN ENGLISH - PART OF FIG. 4 -------------------------------------------------------------

# Extract Half nDL  

melt.dfe3 %>%
  filter(Det == "Numeral" | Det == "Half", Linking == "nDL") -> melt.dfe3.half.ndl

# Plot English Half nDL 

melt.dfe3.half.ndl %>%
  ggplot(aes(Order, Judgment, fill = Order))+ 
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5)+ 
  scale_fill_manual(values = my_colors)+
  scale_x_discrete("Half")+
  scale_y_continuous("Acceptabilty",  expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
  facet_wrap(~Det, scales = "free_x")+
  ggtitle("English")+
  theme(legend.position="none") -> p.melt.dfe3.half.ndl # will used below 

# Preprocessing Italian  --------------------------------------------------------------------

dfi <- as_tibble(read_csv("../data/dfi.csv", show_col_types = FALSE))

# gather data into 3 column format: "linking", "DPType" and "judgment" (longer regex for Italian data)

dfi %>%
  pivot_longer(!participant, names_pattern = c("([a-zA-Z]{2,3}:)([a-zA-Z]+_*\\.*[a-zA-Z]*_*\\.*[a-zA-Z]*_*\\.*[a-zA-Z]*)"), 
               names_to = c("linking", "DPType"),
               values_to = "Judgment") %>%
  mutate(linking = str_replace(linking, ":", "")) %>% rename(Linking = linking) -> melt.dfi2

# Create three new variables: Det, Agreement, Order ----------------------------------------------------------

melt.dfi2 %>% 
  mutate(Det = case_when(DPType == "Num_Vplur" | DPType == "Vplur_num" | DPType == "Vplur_num.obj" ~ "Numeral",
                         DPType == "la.meta_Vplur" | DPType == "la.meta_Vsing" | DPType == "Vplur_la.meta.obj" | DPType == "Vsing_la.meta.obj"| DPType == "Vplur_la.meta" | DPType == "Vsing_la.meta"  ~ "La metà",
                         TRUE ~ "Metà"
                         )) %>%
  mutate(Agreement = case_when(DPType == "meta_Vplur" | DPType == "Num_Vplur" | DPType == "la.meta_Vplur" | DPType == "Vplur_meta" | DPType == "Vplur_num" | DPType == "Vplur_la.meta" | DPType == "Vplur_meta.obj" | DPType == "Vplur_num.obj"  | DPType == "Vplur_la.meta.obj" ~ "V.plur",
                         TRUE ~ "V.sing"  
                         )) %>%
  mutate(Order = case_when(str_detect(DPType, "obj") ~ "Object",
                           str_detect(DPType, "^V") ~ "Post-V subj.",
                           TRUE ~ "Subject")) -> melt.dfi3

# NUMERALS IN ITALIAN - PART OF FIG. 1 -----------------------------------------------------------------

# Extract numerals

melt.dfi3 %>%
  filter(Det == "Numeral")  %>% mutate(Order = factor(Order, levels=c("Subject", "Object", "Post-V subj."))) ->  melt.dfi3.num

# Plot nuemrals 

melt.dfi3.num %>%
  ggplot(aes(Order, Judgment, fill = Order))+
  stat_summary(fun = "mean",
               geom = "bar")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5)+ 
  scale_x_discrete("Numeral position")+
  scale_y_continuous("Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
  facet_wrap(~Linking) +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position="none")+
  scale_fill_manual(values = my_colors)+
  ggtitle("Italian") -> p.melt.dfi3.num  

# Putting together numerals in English and in Italian with patchwork

p.melt.dfe3.num + p.melt.dfi3.num + plot_layout(widths = c(2,3)) -> p.num 
p.num #+ plot_annotation(title = 'Numerals in English and in Italian: mean ratings by DP type and linking')

# Saving 

ggsave("../figures/regenerated/fig1.png", plot = p.num, height = 5.5, width = 8.5)

# nDL IN ITALIN FIG, 3 ----------------------------------------------------------------------------------

# Extracting nDL linking

melt.dfi3 %>% filter(Linking == "nDL" ) %>% 
  mutate(Det = factor(Det, levels = c("Numeral", "La metà", "Metà"))) -> melt.dfi3.ndl

factor(melt.dfi3.ndl$Order, levels=c("Subject", "Object", "Post-V subj.")) -> melt.dfi3.ndl$Order

melt.dfi3.ndl %>% 
  mutate(Fill = Order) -> melt.dfi3.ndl 

#melt.dfi3.ndl %>%
#  group_by(Det) %>%
#  t_test(Judgment ~ Order) %>%
#  adjust_pvalue(method = "bonferroni") %>%
#  add_significance() %>% 
#  add_xy_position(fun = "mean_se", x = "Order", scales = "free") -> stat.test

  melt.dfi3.ndl  %>%
  ggplot(aes(Order, Judgment, fill = Order))+ 
  stat_summary(fun = "mean",
               geom = "bar")+
  stat_summary(fun.data = mean_se,
                             geom = "errorbar",
                              width = .5)+
  scale_x_discrete("DP position")+
  scale_y_continuous("Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5.1))+
  facet_wrap(~Det, scales = "free_x")+
  theme(legend.position="none")+ 
  scale_fill_manual(values = my_colors) -> p.melt.dfi3.ndl 
  
  p.italian.ndl.annotated <- p.melt.dfi3.ndl +
    geom_pwc(
      method = "t_test", label = "p.adj.signif",
      p.adjust.method = "bonferroni",
      tip.length = 0.01, y.position = c(5.1, 4.7, 4.3, 3.9),
      hide.ns = TRUE
    )
  
# p.melt.dfi3.ndl + stat_pvalue_manual(stat.test, hide.ns = TRUE, tip.length = 0.01) 
  
# Saving 

ggsave("../figures/regenerated/fig3.png", plot = p.italian.ndl.annotated, height = 5.5, width = 8.5)

# NUMERAL and LA METÀ + nDL - PART OF FIG. 4 -------------------------------------------------------

melt.dfi3 %>% filter(Det == "Numeral" | DPType == "la.meta_Vplur" | DPType == "Vplur_la.meta" | DPType == "Vplur_la.meta.obj", Linking == "nDL") -> melt.dfi3.lameta.ndl 
  
factor(melt.dfi3.lameta.ndl$Order, levels=c("Subject", "Object", "Post-V subj.")) -> melt.dfi3.lameta.ndl$Order 
factor(melt.dfi3.lameta.ndl$Det, levels=c("Numeral", "La metà")) -> melt.dfi3.lameta.ndl$Det

melt.dfi3.lameta.ndl   %>%
   ggplot(aes(Order, Judgment, fill = Order))+ 
   stat_summary(fun = "mean",
               geom = "bar")+
   stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5)+ 
   scale_x_discrete("Numerals vs. La metà")+
   scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
   facet_wrap(~Det, scales = "free_x")+
   ggtitle("Italian")+
  scale_fill_manual(values = my_colors)+
   theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none") -> p.melt.dfi3.lameta.ndl 

# HALF + LA METÀ- FIG. 4  -------------------------------------------------------
  
p.melt.dfe3.half.ndl + p.melt.dfi3.lameta.ndl + plot_layout(widths = c(2,3)) -> p.half.all.ndl

# Saving 

ggsave("../figures/regenerated/fig4.png", plot = p.half.all.ndl, height = 5.5, width = 8.5)

# ITALIAN AGREEMENT EFFECTS  ------------------------------------------------------=

filter(melt.dfi3, Det == "La metà", Order == "Subject" | Order == "Post-V subj.")  -> melt.dfi3.agree


#p.melt.dfi3.agree <- ggbarplot(
#  melt.dfi3.agree, x = "DPType", y = "Judgment", fill = "Order",
#  palette = "npg", add = "mean_sd",
#  position = position_dodge(0.8)
#)

#melt.dfi3.agree %>%
  #group_by(Det) %>%
#  t_test(Judgment ~ DPType) %>%
#  adjust_pvalue(method = "bonferroni") %>%
#  add_significance()  %>% 
#  add_xy_position(fun = "mean_se", x = "DPType") -> stat.test.melt.dfi3.agree
#scales = "free") 

melt.dfi3.agree %>%
  ggplot(aes(DPType, Judgment, fill = Order))+
  stat_summary(fun = "mean",
               geom = "bar")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .5)+ 
  #  facet_grid(~Det, scales = 'free')+
  scale_fill_manual(values = my_colors)+
  scale_y_continuous("Judgement / Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5.3))+
  ggtitle("Agreement effects in Italian: 'La metà'") -> p.melt.dfi3.agree

p.italian.agree.annotated <- p.melt.dfi3.agree +
  geom_pwc(
    method = "t_test", label = "p.adj.signif",
    p.adjust.method = "bonferroni",
    tip.length = 0.01, y.position = c(4.7, 4.9, 5.1, 5.3),
    hide.ns = TRUE
  ) 

# p.melt.dfi3.agree + stat_pvalue_manual(stat.test.melt.dfi3.agree, hide.ns = TRUE, tip.length = 0.01, y.position = c(4.7, 4.9, 5.1, 5.3)) 

# Saving 

ggsave("../figures/regenerated/agreeita.png", plot = p.italian.agree.annotated, height = 5.5, width = 8.5)


# EFFECTS OF VERB NUM IN ITALIAN nDL AND DL (Fig. 5) ---------------------------------------

melt.dfi3 %>%
  filter(!(Order == "Object"), Linking == "nDL") -> dfi.det.nDL


#dfi.det.nDL   %>%
#  t_test(Judgment ~ DPType) %>%
#  adjust_pvalue(method = "bonferroni") %>%
#  add_significance()  %>% 
#  add_xy_position(fun = "mean_se", x = "DPType") -> stat.test.dfi.det.nDL 

dfi.det.nDL %>%
  ggplot(aes(DPType, Judgment, fill = Order))+ 
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .6) + 
  scale_x_discrete("DP position and V agreement", 
                   labels = c(
                     "meta_Vplur" = "Vplur",   "Num_Vplur" = "Vplur",   "meta_Vsing"  = "Vsing",  "la.meta_Vplur" = "Vplur",
                      "la.meta_Vsing" = "Vsing", "Vplur_meta"  = "Vplur",  "Vplur_num" = "Vplur",   
                      "Vsing_meta" = "Vsing",   "Vplur_la.meta" = "Vplur", "Vsing_la.meta"= "Vsing"
                   ))+
  scale_y_continuous("Judgement / Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5),  breaks = c(1, 2, 3, 4, 5))+
  #scale_fill_discrete(name = "DP position") +
  facet_grid(~Det, scales = 'free')+
  theme(legend.position="none", axis.title.x = element_blank())+
  scale_fill_manual(name = "DP position", values = my_colors)+
  ggtitle("Non D-Linked readings") -> p.dfi.det.nDL 

p.dfi.det.nDL  +
  geom_pwc(
    method = "t_test", label = "p.adj.signif",
    p.adjust.method = "bonferroni",
    tip.length = 0.01, #y.position = c(4.7, 4.9, 5.1, 5.3),
    hide.ns = TRUE
  ) 


#p.dfi.det.nDL + stat_pvalue_manual(stat.test.dfi.det.nDL, hide.ns = TRUE, tip.length = 0.01) 

melt.dfi3 %>%
  filter(!(Order == "Object"), Linking == "DL") -> dfi.det.DL 

#dfi.det.DL   %>%
# t_test(Judgment ~ DPType) %>%
#  adjust_pvalue(method = "bonferroni") %>%
#  add_significance()  %>% 
#  add_xy_position(fun = "mean_se", x = "DPType") -> stat.test.dfi.det.DL 

dfi.det.DL    %>%
#  ggplot(aes(Agreement, Judgment, fill = Order)) +
  ggplot(aes(DPType, Judgment, fill = Order))+ 
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .6) + 
  scale_x_discrete("DP position and V agreement", 
                   labels = c(
                     "meta_Vplur" = "Vplur",   "Num_Vplur" = "Vplur",   "meta_Vsing"  = "Vsing",  "la.meta_Vplur" = "Vplur",
                     "la.meta_Vsing" = "Vsing", "Vplur_meta"  = "Vplur",  "Vplur_num" = "Vplur",   
                     "Vsing_meta" = "Vsing",   "Vplur_la.meta" = "Vplur", "Vsing_la.meta"= "Vsing"
                   ))+
  scale_y_continuous("Judgement / Acceptabilty", expand = expansion(mult = c(0, 0.1)), limits = c(0,5),  breaks = c(1, 2, 3, 4, 5))+
  facet_grid(~Det, scales = 'free')+
  scale_fill_manual(name = "DP position", values = my_colors)+
 # theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  ggtitle("D-Linked readings") -> p.dfi.det.DL

# p.dfi.det.DL + stat_pvalue_manual(stat.test.dfi.det.DL, hide.ns = TRUE, tip.length = 0.01) -> p.dfi.det.DL

p.dfi.det.nDL / p.dfi.det.DL + plot_layout(guides="collect") -> p.dfi.det

ggsave("../figures/regenerated/fig5.png", plot = p.dfi.det, height = 8.5, width = 8.5)

# QUANTIFIERS IN ENGLISH (Fig. 6) --------------------------------------------------------------------

dfeq <- as_tibble(read_csv("../data/dfeq.csv", show_col_types = FALSE))

dfeq %>% 
  rename(c("Participant" = "participant", "None" =  contains("none"), "Nobody" =  contains("nobody"), "No one" =  contains("No one"), "Nothing" =  contains("nothing"),
           "Some" =  contains("some"), "Something" =  contains("something"), "Everything" =  contains("everything"), "Everybody" =  contains("everybody"), "Every one" =  contains("every one"))) %>% 
  pivot_longer(!Participant, 
               names_to = "Q",
               values_to = "Judgment") -> melted.dfeq

melted.dfeq %>% 
  mutate(QType = case_when(Q == "Some1" | Q == "Some2" | Q == "Some3"  | Q == "Some4" | Q == "None1" | Q == "None2" ~ "Partitive",
                         TRUE ~ "Non-partitive")) -> melted.dfeq
  
melted.dfeq   %>%
  ggplot(aes(QType, Judgment, fill = QType)) +
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .6) + 
  scale_x_discrete("Type of quantifier")+
  scale_y_continuous("Judgement", expand = expansion(mult = c(0, 0.1)), limits = c(0,5),  breaks = c(1, 2, 3, 4, 5))+
  theme(legend.position="none")+
  scale_fill_manual(values = my_colors)+
  ggtitle("English") -> p.melted.dfeq


# QUANTIFIERS IN  ITALIAN --------------------------------------------------------------------

dfiq <- as_tibble(read_csv("../data/dfiq.csv", show_col_types = FALSE))

dfiq %>% 
  rename(c("Participant" = "participant", "Partitive" = "Nessuno sembrava fosse stato toccato.", 
               "Non-partitive" = "Nulla sembrava fosse stato toccato.")) %>% 
  pivot_longer(!Participant, #names_pattern = c("([a-zA-Z]{2,3}:)([a-zA-Z]+_*\\.*[a-zA-Z]*_*\\.*[a-zA-Z]*)"), 
             names_to = "QType",
             values_to = "Judgment") -> melted.dfiq

melted.dfiq   %>%
  ggplot(aes(QType, Judgment, fill = QType)) +
  stat_summary(fun = "mean",
               geom = "bar") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .6) + 
  scale_x_discrete("Type of quantifier")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,5),  breaks = c(1, 2, 3, 4, 5))+
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")+
  scale_fill_manual(values = my_colors)+
  ggtitle("Italian") -> p.melted.dfiq


# QUANTIFIERS IN ENGLISH + ITALIAN  -------------------------------------------------------

p.melted.dfeq + p.melted.dfiq -> p.melted.q

# Saving 

ggsave("../figures/regenerated/fig6.png", plot = p.melted.q, height = 5.5, width = 8.5)

#############
