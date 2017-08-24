# Some setup, loads packages and set theme files
source("setup.R")

# Load relevant data
empdata<-getCANSIM("3830033")
pop<-getCANSIM("0510001")

# Employment by Level of Government
plotdata<-empdata %>%
  filter(GEO=="Alberta" & ESTIMATES=="Total number of jobs" &
           NAICS %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services")) %>%
  mutate(NAICS=replace(NAICS,NAICS=="Government educational services","Education"),
         NAICS=replace(NAICS,NAICS=="Government health services","Health"),
         NAICS=replace(NAICS,NAICS=="Federal government services (excluding defence)","Feds, Non-Defense"),
         NAICS=replace(NAICS,NAICS=="Defence services","Feds, Defense"),
         NAICS=replace(NAICS,NAICS=="Provincial and territorial government services","Provincial Govt")
         ,NAICS=replace(NAICS,NAICS=="Municipal government services","Municipal Govt"),
         NAICS=replace(NAICS,NAICS=="Aboriginal government services","Aboriginal Govt")) %>%
  filter(Ref_Date>=2014) %>%
  group_by(NAICS) %>%
  mutate(change=Value-Value[1])
ggplot(plotdata,aes(Ref_Date,change,group=NAICS,color=NAICS))+
  geom_line(size=2) +
  geom_hline(yintercept = 0,size=1)+
  geom_point(data=filter(plotdata,Ref_Date==2016),size=3,show.legend = F)+
  mytheme+
  scale_colour_brewer(name="",palette="Set1") +
  scale_y_continuous(breaks=pretty_breaks(n=6),label=comma) +
  scale_x_continuous(breaks=seq(2014,2016)) +
  labs(x="",y="Number of Jobs",
       title="Change in Alberta Public Employment, by Govt Sector",
       subtitle="",
       caption="Source: CANSIM 383-0033. Graph by @trevortombe")
ggsave("plot.png",width=7,height=4,dpi=200)

## As above, but share of population in 2016
plotdata<-empdata %>%
  filter(ESTIMATES=="Total number of jobs" &
           NAICS %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services")) %>%
  mutate(NAICS=replace(NAICS,NAICS=="Government educational services","Education"),
         NAICS=replace(NAICS,NAICS=="Government health services","Health"),
         NAICS=replace(NAICS,NAICS=="Federal government services (excluding defence)","Feds, Non-Defense"),
         NAICS=replace(NAICS,NAICS=="Defence services","Feds, Defense"),
         NAICS=replace(NAICS,NAICS=="Provincial and territorial government services","Provincial Govt")
         ,NAICS=replace(NAICS,NAICS=="Municipal government services","Municipal Govt"),
         NAICS=replace(NAICS,NAICS=="Aboriginal government services","Aboriginal Govt")) %>%
  filter(Ref_Date>=2014) %>%
  group_by(NAICS) %>%
  mutate(change=Value-Value[1])
pop2<-pop %>%
  filter(SEX=="Both sexes" & AGE=="All ages" & Ref_Date==2016) %>%
  rename(population=Value) %>%
  select(Ref_Date,GEO,population)
plotdata2<-plotdata %>%
  filter(Ref_Date==2016) %>%
  left_join(pop2,by=c("Ref_Date","GEO")) %>%
  mutate(share=Value/population) %>%
  left_join(provnames,by="GEO") %>%
  filter(short!="NA" & short!="CAN") %>%
  select(Ref_Date,short,NAICS,Value,population,share)
ggplot(plotdata2,aes(short,share,group=NAICS,fill=NAICS))+
  geom_bar(stat="identity",position="stack",color="black",alpha=0.7)+
  geom_hline(yintercept=0,size=1)+
  mythemebar+
  scale_fill_brewer(name="",palette = "Set1")+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent) +
  labs(x="",y="Per Cent",
       title="Public Employment as % of Population, by Govt Sector (2016)",
       subtitle="",
       caption="Source: CANSIM 383-0033 and 051-0001. Graph by @trevortombe")
ggsave("plot2.png",width=7,height=4,dpi=200)

## AS ABOVE, BUT SHARE OF COMPENSATION
plotdata<-empdata %>%
  filter(Ref_Date==2016,
         ESTIMATES=="Total compensation for all jobs (thousands of dollars)" &
           NAICS %in% c("Government educational services",
                        "Government health services",
                        "Federal government services (excluding defence)",
                        "Defence services",
                        "Provincial and territorial government services",
                        "Municipal government services",
                        "Aboriginal government services",
                        "All industries")) %>%
  mutate(NAICS=replace(NAICS,NAICS=="Government educational services","Education"),
         NAICS=replace(NAICS,NAICS=="Government health services","Health"),
         NAICS=replace(NAICS,NAICS=="Federal government services (excluding defence)","Feds, Non-Defense"),
         NAICS=replace(NAICS,NAICS=="Defence services","Feds, Defense"),
         NAICS=replace(NAICS,NAICS=="Provincial and territorial government services","Provincial Govt"),
         NAICS=replace(NAICS,NAICS=="Municipal government services","Municipal Govt"),
         NAICS=replace(NAICS,NAICS=="Aboriginal government services","Aboriginal Govt")) %>%
  group_by(GEO) %>%
  mutate(share=Value/max(Value*(NAICS=="All industries"))) %>%
  left_join(provnames,by="GEO") %>%
  filter(NAICS!="All industries" & short!="NA" & short!="CAN")
ggplot(plotdata,aes(short,share,group=NAICS,fill=NAICS))+
  geom_bar(stat="identity",position="stack",color="black",alpha=0.7)+
  geom_hline(yintercept=0,size=1)+
  mythemebar+
  scale_fill_brewer(name="",palette = "Set1")+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent) +
  labs(x="",y="Per Cent",
       title="Public Sector Share of Labour Compensation, by Govt Sector (2016)",
       subtitle="",
       caption="Source: CANSIM 383-0033. Graph by @trevortombe")
ggsave("plot3.png",width=7,height=4,dpi=200)
