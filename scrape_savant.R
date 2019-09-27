#install these packages if you have not already.
library(baseballr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(MASS)
library(data.table)
library(stringr)
library(ggrepel)
#This downloads the data from baseball savant into a data frame
data<-data.frame()
f<-c(seq(as.Date("2018-03-29"), by = "day", length.out = 109),seq(as.Date("2018-07-19"), by = "day", length.out = 74))
options(timeout=100)
for(i in c(1:length(f))){
  data<-rbind(data,scrape_statcast_savant(start_date = f[i], end_date = f[i],
                       player_type = "pitcher"))
}
#creates a new column for a unique id for every ab
df<-data%>%unite(unique_ab,game_pk,at_bat_number)%>%arrange(unique_ab,pitch_number)
#table that appears in the piece
woba_by_count<-df%>%filter(woba_value!="NA")%>%group_by(balls,strikes)%>%summarise(woba=mean(woba_value))

#this reshapes the data so that consecutative pitches can be grouped together, unforunately it loops over 700000 rows, so it takes a few hours to run. If you have a better solution, it would be pretty cool to see it.
row<-list()
for(i in c(1:nrow(df))){
  if(df$pitch_number[i]!=1&&!is.na(df$events[i])){ #if not first pitch and end of at bat there is a useful woba
    state<-data.frame("stand"=df$stand[i],"throws"=df$p_throws[i],"pre_pitch_name"=df$pitch_name[i+1],"pitch_name"=df$pitch_name[i],"pre_pitch_zone"=df$zone[i+1],"pitch_zone"=df$zone[i],"balls"=df$balls[i],"strikes"=df$strikes[i],"woba.y"=df$woba_value[i])
    w1state<-state%>%left_join(woba_by_count,c("balls", "strikes"))%>%mutate(woba=woba.y-woba)
    row[[i]]<-w1state[,-9]
  }else if(df$pitch_number[i]!=1&&is.na(df$events[i])){ #if not end of at bat we track the difference in count and a previous pitch exists
    state<-data.frame("stand"=df$stand[i],"throws"=df$p_throws[i],"pre_pitch_name"=df$pitch_name[i+1],"pitch_name"=df$pitch_name[i],"pre_pitch_zone"=df$zone[i+1],"pitch_zone"=df$zone[i],"balls1"=df$balls[i],"strikes1"=df$strikes[i],"balls"=df$balls[i-1],"strikes"=df$strikes[i-1])
    w1state<-state%>%left_join(woba_by_count,by = c("balls", "strikes"))
    names(w1state)[c(7:10)]<-c("balls","strikes","balls1","strikes1")
    w2state<-w1state%>%left_join(woba_by_count,by = c("balls", "strikes"))%>%mutate(woba=woba.x-woba.y)
    row[[i]]<-w2state[,c(1:8,13)]
  }
}
row2= row[-which(sapply(row, is.null))]
two_pitch<-as.data.frame(do.call(rbind,row2))

#ready for analysis
table<-two_pitch%>%group_by_at(2:9)%>%summarise(mwoba=mean(woba),n=n())%>%arrange(mwoba)

#function to group baseball savant pitch locations by high, medium and low.
highlow<-function(x){
  if(x%in%c(1,2,3,11,12)){
    x<-1
  }else if(x%in%c(4,5,6)){
    x<-2
  }else{
    x<-3
  }
}
#function to group baseball savant pitch types into: fastball, breaking and offspeed
pitchtype<-function(x){
  if(x%in%c("4-Seam Fastball","2-Seam Fastball","Cutter","Sinker")){
    x<-"Fastball"
  }else if(x%in%c("Split Finger","Changeup","Forkball","Screwball")){
    x<-"Offspeed"
  }else{
    x<-"Breaking"
  }
}
#apply the highlow function for the two pitches. In retrospect, lapply() would be an easy fix for the loop here, but it still doesn't take too long to run.
for(i in c(1:nrow(table))){
    table$pitch_zone[i]<-highlow(table$pitch_zone[i])
    table$pre_pitch_zone[i]<-highlow(table$pre_pitch_zone[i])
}
levels(table$pitch_name)<-c(levels(table$pitch_name),"Fastball","Offspeed","Breaking")
levels(table$pre_pitch_name)<-c(levels(table$pre_pitch_name),"Fastball","Offspeed","Breaking")

#reshape the data for analysis
highlowtable<-table #this step may be broken
htable3<-highlowtable%>%unite(pitch,c(pitch_name,pitch_zone))%>%spread(pitch,nmwoba)
htable3[is.na(htable3)]<-0
htable4<-htable3%>%unite(pre_pitch,c(pre_pitch_name,pre_pitch_zone))%>%group_by(pre_pitch)%>%summarise_each(funs(.[which.max(abs(.))]))%>%column_to_rownames('pre_pitch')

#similar process for pitch name groupings
for(i in c(1:nrow(table))){
  table$pitch_name[i]<-pitchtype(table$pitch_name[i])
  table$pre_pitch_name[i]<-pitchtype(table$pre_pitch_name[i])
}
table2<-table[,c(3:10)]%>%group_by_at(1:4)%>%summarise(nmwoba=mean(mwoba*n)/sum(n),n=sum(n))%>%filter(n>=30)%>%arrange(pre_pitch_name,pre_pitch_zone,desc(nmwoba))
table3<-table2%>%unite(pitch,c(pitch_name,pitch_zone))%>%spread(pitch,nmwoba)
table3[is.na(table3)]<-0
table4<-table3%>%unite(pre_pitch,c(pre_pitch_name,pre_pitch_zone))%>%group_by(pre_pitch)%>%summarise_each(funs(.[which.max(abs(.))]))%>%column_to_rownames('pre_pitch')
matrix<-as.matrix(table4[,-1])

heatmap.2(matrix,scale="row",col=rev(colorRampPalette(brewer.pal(9, "RdBu"))(256)),main="Best Pitch by Type and Inside Outside",xlab = "Suggested Pitch",ylab = "Previous Pitch",margins = c(10,10),cellnote = round(matrix,7),notecol="black")
#instead of high low, inside and outside.
insideout<-function(x,y){
  if(x%in%c(2,5,8)){
    x<-2
  }else if(x%in%c(1,4,7,11,13)&y=="R"){
    x<-1
  }else if(x%in%c(1,4,7,11,13)&y=="L"){
    x<-3
  }else if(x%in%c(3,6,9,12,14)&y=="L"){
    x<-1
  }else{
    x<-3
  }
}

for(i in c(1:nrow(table))){
  table$pitch_zone[i]<-insideout(table$pitch_zone[i],table$stand[i])
  table$pre_pitch_zone[i]<-insideout(table$pre_pitch_zone[i],table$stand[i])
}
#add a variable for same and opposite handedness.
for(i in c(1:nrow(table))){
  if(table$stand[i]==table$throws[i])
    table$same_hand[i]<-1
  else{
    table$same_hand[i]<-0
  }
}

stable2<-table[,c(3:11)]%>%group_by_at(c(1:4,9))%>%summarise(nmwoba=mean(mwoba*n)/sum(n),n=sum(n))%>%filter(n>=30&same_hand==1)%>%arrange(pre_pitch_name,pre_pitch_zone,desc(nmwoba))
stable3<-stable2%>%unite(pitch,c(pitch_name,pitch_zone))%>%spread(pitch,nmwoba)
stable3[is.na(stable3)]<-0
stable4<-stable3%>%unite(pre_pitch,c(pre_pitch_name,pre_pitch_zone))%>%group_by(pre_pitch)%>%summarise_each(funs(.[which.max(abs(.))]))%>%column_to_rownames('pre_pitch')
smatrix<-as.matrix(stable4[,c(3:11)])

heatmap.2(smatrix,scale="row",col=rev(colorRampPalette(brewer.pal(9, "RdBu"))(256)),main="Best Pitch by Type and Inside/Outside Same Hand",xlab = "Suggested Pitch",ylab = "Previous Pitch",margins = c(10,10),cellnote = round(matrix,7),notecol="black")

ntable2<-table[,c(3:11)]%>%group_by_at(c(1:4,9))%>%summarise(nmwoba=mean(mwoba*n)/sum(n),n=sum(n))%>%filter(n>=30&same_hand==0)%>%arrange(pre_pitch_name,pre_pitch_zone,desc(nmwoba))
ntable3<-ntable2%>%unite(pitch,c(pitch_name,pitch_zone))%>%spread(pitch,nmwoba)
ntable3[is.na(ntable3)]<-0
ntable4<-ntable3%>%unite(pre_pitch,c(pre_pitch_name,pre_pitch_zone))%>%group_by(pre_pitch)%>%summarise_each(funs(.[which.max(abs(.))]))%>%column_to_rownames('pre_pitch')
nmatrix<-as.matrix(ntable4[,c(3:11)])

heatmap.2(nmatrix,scale="row",col=rev(colorRampPalette(brewer.pal(9, "RdBu"))(256)),main="Best Pitch by Type and Inside/Outside Opposite Hand",xlab = "Suggested Pitch",ylab = "Previous Pitch",margins = c(10,10),cellnote = round(matrix,7),notecol="black")

#===============================================================#
#clustering pitchers. I don't really have time to clean this section up but if you have questions, let me know and I'll try to get back to you... sorry.
data<-read.csv('C:/Users/Peter/Documents/baseball-databases/previous_pitch/base.csv')
pitchers<-data%>%group_by(pitcher,pitch_name,p_throws)%>%summarise(mvelo=mean(release_speed,na.rm = T),mext=mean(release_extension,na.rm = T),my=mean(release_pos_y,na.rm = T),mz=mean(release_pos_z,na.rm = T),mx=mean(release_pos_x,na.rm = T),mspin=mean(release_spin_rate,na.rm = T),vvelo=var(release_speed,na.rm = T),vext=var(release_extension,na.rm = T),vy=var(release_pos_y,na.rm = T),vz=var(release_pos_z,na.rm = T),vx=var(release_pos_x,na.rm = T),vspin=var(release_spin_rate,na.rm = T),mpfx_x=mean(pfx_x,na.rm = T),mpfx_z=mean(pfx_z,na.rm = T),mplate_x=mean(plate_x,na.rm = T),mplate_z=mean(plate_z,na.rm = T),vpfx_x=var(pfx_x,na.rm = T),vpfx_z=var(pfx_z,na.rm = T),vplate_x=var(plate_x,na.rm = T),vplate_z=var(plate_z,na.rm = T),mvx0=mean(vx0,na.rm = T),mvy0=mean(vy0,na.rm = T),mvz0=mean(vz0,na.rm = T),max=mean(ax,na.rm = T),may=mean(ay,na.rm = T),maz=mean(az,na.rm = T),vvx0=var(vx0,na.rm = T),vvy0=var(vy0,na.rm = T),vvz0=var(vz0,na.rm = T),vax=var(ax,na.rm = T),vay=var(ay,na.rm = T),vaz=var(az,na.rm = T))
cpitchers<-na.omit(pitchers)
s<-cpitchers %>% 
  gather(variable, value, -(pitcher:p_throws)) %>%
  unite(temp, pitch_name, variable) %>%
  spread(temp, value)
s[is.na(s)]<-0

reduc_pitch<-as.data.frame(scale(s[,c(3:ncol(s))]))
dim_pitch<-prcomp(reduc_pitch)

#cumulative proportion PC72=.95091
reduc_dim_pitch<-dim_pitch$rotation[,c(1:72)]
f_red_dim_pitch<-as.data.frame(as.matrix(reduc_pitch)%*%reduc_dim_pitch)
row.names(f_red_dim_pitch)<-as.vector(unite(s,"pid",c("pitcher","p_throws"))[[1]])

m<-c()
for(i in c(2:15)){
  clust_pitch<-kmeans(f_red_dim_pitch,i,100)
  m<-c(m,Davies.Bouldin(clust_pitch$centers,clust_pitch$withinss,clust_pitch$size))
}
plot(c(2:15),m,main="Selection of Number of Centers",xlab = "Number of Centers",ylab="Davies-Boldin Index")
lines(c(2:15),m) 
#consistently drop in DB index between 5-7
fclust_pitch<-kmeans(f_red_dim_pitch,6,100)

df<-data%>%unite("pid",c("pitcher","p_throws"))

joiner<-setDT(as.data.frame(fclust_pitch$cluster), keep.rownames = TRUE)[]
colnames(joiner)<-c("pid","cluster")

ddf<-left_join(cpitchers%>%unite("pid",c("pitcher","p_throws")),joiner,by="pid")
ddf<-ddf%>%group_by(cluster,pitch_name)%>%summarise_if(is.numeric,mean)

fdf<-left_join(df,joiner,by="pid")
fgf<-na.omit(fdf)
ffdf<-fgf%>%group_by(cluster,pitch_name)%>%summarise_if(is.numeric,mean)

#clustering batters
batters<-data%>%group_by(batter,pitch_name,stand)%>%summarise(mhc_x=mean(hc_x,na.rm = T),mhc_y=mean(hc_y,na.rm = T),vhc_x=var(hc_x,na.rm = T),vhc_y=var(hc_y,na.rm = T),mhit_distance=mean(hit_distance_sc,na.rm = T),mlaunch_speed=mean(launch_angle,na.rm = T),mlaunch_angle=mean(launch_angle,na.rm=T),mhit_location=mean(hit_location,na.rm = T),mbarrel=mean(barrel,na.rm = T),mpitch_number=mean(pitch_number,na.rm = T),vpitch_number=var(pitch_number,na.rm = T),vbarrel=var(barrel,na.rm = T),vhit_location=var(hit_location,na.rm = T),vhit_distance=var(hit_distance_sc,na.rm = T),vlaunch_speed=var(launch_angle,na.rm = T),vlaunch_angle=var(launch_angle,na.rm=T),mplate_x=mean(plate_x,na.rm = T),mplate_z=mean(plate_z,na.rm = T),vplate_x=var(plate_x,na.rm = T),vplate_z=var(plate_z,na.rm = T),total=n())
add<-data%>%group_by(batter,pitch_name,stand,description)%>%summarise(n=n())%>%spread(description,n)%>%mutate(whiff=sum(swinging_strike,swinging_strike_blocked,swinging_pitchout,missed_bunt,na.rm=T),take=sum(ball,blocked_ball,called_strike,hit_by_pitch,pitchout,na.rm=T),contact=sum(hit_into_play,hit_into_play_score,hit_into_play_no_out,na.rm=T))
batters2<-(left_join(batters,add,by=c("batter","pitch_name","stand"))%>%mutate(whiffp=whiff/total,takep=take/total,contactp=contact/total))[,c(1:23,44:46)]
cbatters<-na.omit(batters2)

t<-cbatters %>% 
  gather(variable, value, -(batter:stand)) %>%
  unite(temp, pitch_name, variable) %>%
  spread(temp, value)
t[is.na(t)]<-0

reduc_batt<-as.data.frame(scale(t[,c(3:ncol(t))]))
reduc_batt[is.na(reduc_batt)]<-0
dim_batt<-prcomp(reduc_batt)

#PC80 cumu proportion .95004
reduc_dim_batt<-dim_batt$rotation[,c(1:80)]
f_red_dim_batt<-as.data.frame(as.matrix(reduc_batt)%*%reduc_dim_batt)
row.names(f_red_dim_batt)<-as.vector(unite(t,"bid",c("batter","stand"))[[1]])

m<-c()
for(i in c(2:15)){
  clust_batt<-kmeans(f_red_dim_batt,i,100)
  m<-c(m,Davies.Bouldin(clust_batt$centers,clust_batt$withinss,clust_batt$size))
}
plot(c(2:15),m,main="Selection of Number of Centers",xlab = "Number of Centers",ylab="Davies-Boldin Index")
lines(c(2:15),m) 

#consistently between 5-6
fclust_batt<-kmeans(f_red_dim_batt,6,100)

df<-data%>%unite("bid",c("batter","stand"))

bjoiner<-setDT(as.data.frame(fclust_batt$cluster), keep.rownames = TRUE)[]
colnames(bjoiner)<-c("bid","bcluster")

bdf<-left_join(cbatters%>%unite("bid",c("batter","stand")),bjoiner,by="bid")
bdf<-bdf%>%group_by(bcluster,pitch_name)%>%summarise_if(is.numeric,mean)

bfdf<-left_join(df,bjoiner,by="bid")
bfgf<-na.omit(bfdf)
bffdf<-bfgf%>%group_by(cluster,pitch_name)%>%summarise_if(is.numeric,mean)

ifdf<-bfdf%>%unite("pid",c("pitcher","p_throws"))

finaldf<-left_join(ifdf,joiner,by="pid")

sort<-finaldf%>%filter(!is.na(woba_value))%>%group_by(cluster,bcluster,pitch_name)%>%summarise(woba=mean(woba_value),n=n())%>%filter(n>30)

#========================#
#testing
data2<-data.frame()
f2<-seq(as.Date("2019-03-28"), by = "day", length.out = 102)
options(timeout=100)
memory.limit(size=56000)
for(i in c(1:length(f2))){
  data2<-rbind(data2,scrape_statcast_savant(start_date = f2[i], end_date = f2[i],
                                          player_type = "pitcher"))
}
write.csv(data2,'C:/Users/Peter/Documents/baseball-databases/previous_pitch/test.csv')
testdf<-data2%>%unite("pid",c("pitcher","p_throws"))
ftestdf<-testdf%>%unite("bid",c("batter","stand"))

testclustdf<-left_join(ftestdf,joiner,by="pid")
ftestclustdf<-left_join(testclustdf,bjoiner,by="bid")

test<-ftestclustdf%>%filter(!is.na(woba_value))%>%group_by(cluster,bcluster,pitch_name)%>%summarise(woba=mean(woba_value),n=n())%>%filter(n>30)
comp<-left_join(sort,test,by=c("cluster","bcluster","pitch_name"))
comp<-na.omit(comp)
comp2<-comp%>%filter(n.y>30)%>%mutate(error=woba.x-woba.y)
mae<-mean(abs(comp2$error))


pitchers_by_clust<-na.omit(unique(ftestclustdf[,c("player_name", "cluster")]))%>%arrange(cluster)
batters_by_clust<-na.omit(unique((ftestclustdf%>%separate('bid',c('id','hand'),sep='_'))[,c("id", "cluster")]))%>%arrange(cluster)
master<-read.csv('C:/Users/Peter/Documents/baseball-databases/master.csv')
master<-master%>%mutate_if(is.numeric,as.character)
batters_by_clust<-left_join(batters_by_clust,master,by=c('id'='mlb_id'))

finaldf$cluster<-as.factor(finaldf$cluster)

fastball<-finaldf%>%filter(pitch_name=='4-Seam Fastball')
ggplot(fastball,aes(x=release_speed,y=release_spin_rate,col=cluster,label=player_name))+geom_point()+theme(panel.background=element_blank(),legend.position = 'none')+labs(title='2 Dimensional Plot of Clusters',x='Fastball Velocity',y='Fastball Spin Rate')

library(scatterplot3d)
my_col <- as.numeric(fastball$cluster)
frames <- 360

rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

#loop through plots
for(i in 1:frames){
  name <- rename(i)
  
  #saves the plot as a .png file in the working directory
  png(name)
  scatterplot3d(fastball[,c('release_speed','release_spin_rate','release_extension')],
                main=paste("Angle", i),
                angle=i,
                pch=19,
                cex.symbols=0.5,
                color=my_col)
  dev.off()
}

ptrank<-comp2%>%group_by(cluster,pitch_name)%>%arrange(cluster,pitch_name,woba.x)%>%mutate(rank1=row_number())
pterank<-ptrank%>%group_by(cluster,pitch_name)%>%arrange(cluster,pitch_name,woba.y)%>%mutate(rank2=row_number())
prank_order<-pterank%>%mutate(rank_error=abs(rank1-rank2))
pmae<-mean(prank_order$rank_error)
rand<-data.frame(matrix(c(1,2,3,4,5,6,0, 0.5, 0.8888889 ,1.25, 1.6, 1.9444444),6,2))
names(rand)<-c("p","odds")
podds<-left_join(prank_order%>%group_by(cluster,pitch_name)%>%summarise(p=max(row_number())),rand,by='p')
possiblep<-sum(podds$p*podds$odds)/sum(podds$p)

btrank<-comp2%>%group_by(bcluster,pitch_name)%>%arrange(bcluster,pitch_name,woba.x)%>%mutate(rank1=row_number())
bterank<-btrank%>%group_by(bcluster,pitch_name)%>%arrange(bcluster,pitch_name,woba.y)%>%mutate(rank2=row_number())
brank_order<-bterank%>%mutate(rank_error=abs(rank1-rank2))
bmae<-mean(brank_order$rank_error)
bodds<-left_join(brank_order%>%group_by(bcluster,pitch_name)%>%summarise(p=max(row_number())),rand,by='p')
possibleb<-sum(bodds$p*bodds$odds)/sum(bodds$p)

mae<-mean(abs(brank_order$error))


pitcher_ranks<-prank_order%>%group_by(bcluster,pitch_name)%>%summarize(mtrain=mean(rank1),vtrain=var(rank1),mtest=mean(rank2),vtest=var(rank2))
pitcher_ranks<-na.omit(pitcher_ranks)
batter_ranks<-brank_order%>%group_by(cluster,pitch_name)%>%summarize(mtrain=mean(rank1),vtrain=var(rank1),mtest=mean(rank2),vtest=var(rank2))
batter_ranks<-na.omit(batter_ranks)
