library(tidyverse)
library(ggsci)

a <- read.csv("intervalMetrics5years.csv")

b <- read.csv("MetricsTab5years.csv")

a$Scen <- relevel(a$Scen, "W2")

a_plot <- ggplot(a, aes(width=.75))+
  geom_line(aes(x=rep(1:(52*4), 180/4), y=interval_score, colour=CRN))+
  xlab("Week")+
  ylab("Interval Score")+
  scale_fill_lancet(name="Country")+
  facet_wrap(vars(Scen))

ggsave("a_ts.png", a_plot)

a2 <- a %>% group_by(CRN, Scen) %>% summarise(theMis = mean(interval_score), sharpness=mean(sharpness)) %>% ungroup()

a_plot <- ggplot(a2, aes(width=.75))+
  geom_col(aes(x=Scen, y=sharpness, fill=CRN), position = "dodge")+
  xlab("")+
  ylab("Sharpness")+
  scale_fill_lancet(name="Country")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))

ggsave("a.png", a_plot)

a_plot <- ggplot(a2, aes(width=.75))+
  geom_col(aes(x=Scen, y=theMis, fill=CRN), position = "dodge")+
  xlab("")+
  ylab("Mean Interval Score")+
  scale_fill_lancet(name="Country")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))

scale_fill_lancet()

ggsave("a_b.png", a_plot, height = 4)

a2_plot <- ggplot(a2)+
  theme_classic()+
  geom_line(aes(x=Scen, y=theMis, group=CRN), alpha=.5)+
   geom_point(aes(x=Scen, y=theMis, colour=CRN), size=4)+
  xlab("")+
  ylab("Mean Interval Score")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))+
  scale_colour_lancet()

ggsave("mis_chart.png", a2_plot, height = 4)

levels(b$Dataset) <- c("Testing", "Training")

b_plot <- ggplot(b, aes(width=.75))+
  theme_classic()+
  geom_col(aes(x=Scen, y=R2, fill=Country), position = "dodge")+
  xlab("")+
  ylab("R-squared")+
  scale_fill_lancet(name="Country")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))+
  facet_wrap(vars(Dataset))+
  scale_y_continuous(labels = scales::label_percent())

ggsave("b.png", b_plot, height = 4)

c_plot <- ggplot(b, aes(width=.75))+
  geom_boxplot(aes(x=Scen, y=R2, fill=Scen))+
  xlab("")+
  ylab("R-squared")+
  scale_fill_lancet(name="")+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))+
  facet_wrap(vars(Dataset))+
  scale_y_continuous(labels = scales::label_percent())

ggsave("c.png", c_plot)

d_plot <- ggplot(b, aes(width=.75))+
  geom_boxplot(aes(x=Scen, y=RMSPE, fill=Scen))+
  xlab("")+
  ylab("Root mean square percentage error (RMSPE)")+
  scale_fill_lancet(name="")+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))+
  facet_wrap(vars(Dataset))+
  scale_y_continuous(labels = scales::label_percent())

ggsave("d.png", d_plot)

e_plot <- ggplot(b, aes(width=.75))+
  geom_boxplot(aes(x=Scen, y=MAPE, fill=Scen))+
  xlab("")+
  ylab("Mean absolute percentage error (MAPE)")+
  scale_fill_lancet(name="")+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("W2", "M1", "M2", "M3"))+
  facet_wrap(vars(Dataset))+
  scale_y_continuous(labels = scales::label_percent())

ggsave("e.png", e_plot)

