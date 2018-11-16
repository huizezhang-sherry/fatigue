library(ggplot2)
library(visdat)
# First vs. Second serve speed
ggplot(Federer,aes(x=Count,y=Speed_KMH,col= match_num)) +
  ylim(min(Federer$Speed_KMH),max(Federer$Speed_KMH))+
  geom_point() +
  facet_grid(~ServeNumber,scales = "free_x") + 
  scale_y_continuous("Speed (KPH)", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous("Serve Count", breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "right", text = element_text(size = 16)) + 
  ggtitle("Roger Federer Serving Speeds (Aus Open 2017)")

# Distribution of First vs. Second serve speed
ggplot(Federer,aes(x=Speed_KMH,col = Round)) +
  ylim(min(Federer$Speed_KMH),max(Federer$Speed_KMH))+
  geom_density() +
  facet_wrap(~ServeNumber, ncol = 1, scales = "free_x") + 
  scale_y_continuous("Frequency", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous("Speed (KPH)", breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "right", text = element_text(size = 16)) + 
  ggtitle("Roger Federer Serving Speeds (Aus Open 2017)")

# density shifting towards left as the progression of the Round#
ggplot(subset(Federer,(ServeNumber == 2)&(Round == c(2,4,6))),
       aes(x=Speed_KMH,col = Round)) +
  ylim(min(Federer$Speed_KMH),max(Federer$Speed_KMH))+
  geom_density(aes(fill = Round), alpha = 0.25) +
  scale_y_continuous("Frequency", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous("Speed (KPH)", breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "right", text = element_text(size = 16)) + 
  ggtitle("Roger Federer Second Serving Speeds (Aus Open 2017)")



