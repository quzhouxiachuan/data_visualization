require(lattice)
setwd('R:/IPHAM/CHIP/Data_Team/Projects/Geocoding/Yu')
x = read.csv('R:/IPHAM/CHIP/Data_Team/Projects/Geocoding/top50_overall_poverty_med_071118.csv')
colnames(x)[6] = 'Condition'
colnames(x)[1] ='Group'
colnames(x)[4] ='LowerLimit'
colnames(x)[5] = 'UpperLimit'
colnames(x)[3] = 'RiskRatio'
x$Group = as.factor(x$Group)
i = 1 

for (i in 1:10)
{
  pdf(paste('OR_forestplot', i,'.pdf'))
s = (10*i-9)
e = 10*i
RR_data = x[s:e,]
p = ggplot(data=RR_data,
           aes(x = Group,y = RiskRatio, ymin = LowerLimit, ymax = UpperLimit ))+
  geom_pointrange(aes(col=Group))+
  geom_hline(aes(fill=Group),yintercept =1, linetype=2)+
  xlab('Group')+ ylab("Risk Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=LowerLimit, ymax=UpperLimit,col=Group),width=0.5,cex=0.4)+ 
  facet_wrap(~Condition,strip.position="left",nrow=9,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
print(p)
dev.off()

}
