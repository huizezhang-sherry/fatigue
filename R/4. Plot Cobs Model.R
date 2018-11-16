# plot the COBS model by match_num
ggplot(subset(Federer,(ServeNumber == 2)),aes(x=Count,col = Round)) +
  geom_point(aes(y=Speed_KMH),size = 2) +
  geom_line(aes(y=fit_cbs_match), size = 2)+
  scale_y_continuous("Speed (KPH)", breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous("Count", breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "right", text = element_text(size = 16)) + 
  ggtitle("Roger Federer Second Serving Speeds (Aus Open 2017)")

# plot the COBS model by SetNo
ggplot(subset(Federer,(ServeNumber == 2)),aes(x=Count, y=fit_cbs_set,col = SetNo)) +
  geom_line()+
  facet_wrap(~SetNo)+
  scale_y_continuous("Speed (KPH)", breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous("Count", breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "right", text = element_text(size = 16)) + 
  ggtitle("Roger Federer Second Serving Speeds (Aus Open 2017)")

#geom_smooth(se = FALSE)+
#geom_smooth(method = "lm", formula = y~bs(x, dt= 3))+
#geom_smooth(method = "gam", formula = y~s(x, m=2)) + 
#geom_smooth(method = "lm", formula = y~ns(x, dt= 2)) +