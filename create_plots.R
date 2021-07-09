# Some setup, loads packages and set theme files
source("core.R")

# Load relevant data
empdata<-getTABLE("36100480")
pop<-getTABLE("17100005")

# Remove the Industry Codes
empdata<-empdata %>%
  mutate(industry_code=str_match(Industry,"\\[(.*?)\\]")[,2],
         Industry=ifelse(regexpr(" \\[",Industry)>1,
                         substr(Industry,1,regexpr(" \\[",Industry)-1),Industry))

# Employment by Level of Government
plotdata<-empdata %>%
  filter(GEO=="Alberta" & Labour.productivity.and.related.measures=="Total number of jobs" &
           Industry %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services")) %>%
  mutate(Industry=replace(Industry,Industry=="Government educational services","Education"),
         Industry=replace(Industry,Industry=="Government health services","Health"),
         Industry=replace(Industry,Industry=="Federal government services (excluding defence)","Feds, Non-Defense"),
         Industry=replace(Industry,Industry=="Defence services","Feds, Defense"),
         Industry=replace(Industry,Industry=="Provincial and territorial government services","Provincial Govt"),
         Industry=replace(Industry,Industry=="Municipal government services","Municipal Govt"),
         Industry=replace(Industry,Industry=="Aboriginal government services","Aboriginal Govt")) %>%
  filter(Ref_Date>=2014) %>%
  group_by(Industry) %>%
  mutate(change=Value-Value[1])
ggplot(plotdata,aes(Ref_Date,change,group=Industry,color=Industry))+
  geom_line(size=2) +
  geom_hline(yintercept = 0,size=1)+
  geom_point(data=filter(plotdata,Ref_Date==max(Ref_Date)),size=2.5,stroke=2.5,
             shape=21,fill='white',show.legend = F)+
  mytheme+
  scale_colour_brewer(name="",palette="Set1") +
  scale_y_continuous(breaks=pretty_breaks(n=6),label=comma) +
  scale_x_continuous(breaks=seq(2014,max(plotdata$Ref_Date))) +
  labs(x="",y="Number of Jobs",
       title="Change in Alberta Public Employment, by Govt Sector",
       subtitle="Source: Own calculations from Statistics Canada data table 36-10-0480.",
       caption="Graph by @trevortombe")
ggsave("plot.png",width=8,height=4,dpi=200)

## As above, but share of population in 2016
plotdata<-empdata %>%
  filter(Labour.productivity.and.related.measures=="Total number of jobs" &
           Industry %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services")) %>%
  mutate(Industry=replace(Industry,Industry=="Government educational services","Education"),
         Industry=replace(Industry,Industry=="Government health services","Health"),
         Industry=replace(Industry,Industry=="Federal government services (excluding defence)","Feds, Non-Defense"),
         Industry=replace(Industry,Industry=="Defence services","Feds, Defense"),
         Industry=replace(Industry,Industry=="Provincial and territorial government services","Provincial Govt"),
         Industry=replace(Industry,Industry=="Municipal government services","Municipal Govt"),
         Industry=replace(Industry,Industry=="Aboriginal government services","Aboriginal Govt")) %>%
  filter(Ref_Date>=2014) %>%
  group_by(Industry) %>%
  mutate(change=Value-Value[1])
pop2<-pop %>%
  filter(Sex=="Both sexes" & Age.group=="All ages" & Ref_Date==max(Ref_Date)) %>%
  rename(population=Value) %>%
  select(Ref_Date,GEO,population)
plotdata2<-plotdata %>%
  filter(Ref_Date==max(Ref_Date)) %>%
  left_join(pop2,by=c("Ref_Date","GEO")) %>%
  mutate(share=Value/population) %>%
  filter(short!="NA" & short!="CAN" & !short %in% c("YT","NT","NU")) %>%
  select(Ref_Date,short,Industry,Value,population,share)
ggplot(plotdata2,aes(short,share,group=Industry,fill=Industry))+
  geom_col(position="stack",color="white")+
  geom_hline(yintercept=0,size=1)+
  mythemebar+
  scale_fill_brewer(name="",palette = "Set1")+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent) +
  labs(x="",y="Per Cent",
       title=paste0("Public Employment as % of Population, by Govt Sector (",
                    max(plotdata2$Ref_Date),")"),
       subtitle="Source: Own calculations from Statistics Canada data table CANSIM 36-10-0480 and 17-10-0005.",
       caption="Graph by @trevortombe")
ggsave("plot2.png",width=8,height=4,dpi=200)

## AS ABOVE, BUT SHARE OF COMPENSATION
plotdata<-empdata %>%
  filter(Ref_Date==max(Ref_Date),
         Labour.productivity.and.related.measures=="Total compensation for all jobs" &
           Industry %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services",
                        "All industries")) %>%
  mutate(Industry=replace(Industry,Industry=="Government educational services","Education"),
         Industry=replace(Industry,Industry=="Government health services","Health"),
         Industry=replace(Industry,Industry=="Federal government services (excluding defence)","Feds, Non-Defense"),
         Industry=replace(Industry,Industry=="Defence services","Feds, Defense"),
         Industry=replace(Industry,Industry=="Provincial and territorial government services","Provincial Govt"),
         Industry=replace(Industry,Industry=="Municipal government services","Municipal Govt"),
         Industry=replace(Industry,Industry=="Aboriginal government services","Aboriginal Govt")) %>%
  group_by(GEO) %>%
  mutate(share=Value/max(Value*(Industry=="All industries"))) %>%
  filter(Industry!="All industries" & short!="NA" & short!="CAN"& !short %in% c("YT","NT","NU"))
ggplot(plotdata,aes(short,share,group=Industry,fill=Industry))+
  geom_col(position="stack",color="white")+
  geom_hline(yintercept=0,size=1)+
  mythemebar+
  scale_fill_brewer(name="",palette = "Set1")+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent) +
  labs(x="",y="Per Cent",
       title=paste0("Public Sector Share of Total Labour Compensation (",
       max(plotdata2$Ref_Date),")"),
       subtitle="Source: Own calculations from Statistics Canada data table CANSIM 36-10-0480",
       caption="Graph by @trevortombe")
ggsave("plot3.png",width=8,height=4,dpi=200)

# Average Hourly Pay in Selected Government Sectors
plotdata<-empdata %>%
  filter(Ref_Date==max(Ref_Date),GEO %in% c("Alberta","British Columbia","Ontario","Quebec"),
         Labour.productivity.and.related.measures=="Total compensation per hour worked" &
           Industry %in% c("Government educational services",
                           "Government health services",
                           "Provincial and territorial government services",
                           "All industries"))
ggplot(plotdata,aes(reorder(Industry,Value),Value,group=GEO,fill=GEO))+
  geom_col(position='dodge')+
  coord_flip()+
  geom_hline(yintercept=0,size=1)+
  mythemebarflip+
  labs(y="Dollars per Hour",
       x="",
       title=paste0("Average Hourly Labour Compensation, Selected Government Sectors (",
                    max(plotdata$Ref_Date),")"),
       subtitle="Source: Own calculations from Statistics Canada data table CANSIM 36-10-0480",
       caption="Graph by @trevortombe")
ggsave("plot4.png",width=10,height=5,dpi=200)

# Average Hourly Pay in Selected Government Sectors over time
plotdata<-empdata %>%
  filter(GEO %in% c("Alberta","Canada"),
         Labour.productivity.and.related.measures=="Total compensation per hour worked" &
           Industry %in% c("Government educational services",
                           "Government health services",
                           "All industries"))
ggplot(plotdata,aes(Ref_Date,Value,group=Industry,color=Industry))+
  geom_line(size=2)+
  facet_wrap(~GEO)+
  geom_hline(yintercept=0,size=1)+
  mytheme+
  labs(y="Dollars per Hour",
       x="",
       title="Average Hourly Labour Compensation, Selected Government Sectors",
       subtitle="Source: Own calculations from Statistics Canada data table CANSIM 36-10-0480",
       caption="Graph by @trevortombe")
ggsave("plot5.png",width=9,height=4.5,dpi=200)

