library(yaml)
library(httr)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)
library(ggrepel)
library(jsonlite)

credentials <- read_yaml('credentials.yaml')

app <- oauth_app("strava", credentials$client_id, credentials$secret)

endpoint <- oauth_endpoint(
    request = NULL,
    authorize = "https://www.strava.com/oauth/authorize",
    access = "https://www.strava.com/oauth/token"
)

token <- oauth2.0_token(endpoint, app, as_header = FALSE,
                        scope = "activity:read_all")

df_list <- list()
i <- 1
done <- FALSE
while (!done) {
    req <- GET(
        url = "https://www.strava.com/api/v3/athlete/activities",
        config = token,
        query = list(per_page = 200, page = i)
    )
    df_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
    if (length(content(req)) < 200) {
        done <- TRUE
    } else {
        i <- i + 1
    }
}
df <- rbind_pages(df_list) %>%
    mutate(year =as.numeric(substr(start_date_local,1,4)),
           month=as.numeric(substr(start_date_local,6,7)),
           day  =as.numeric(substr(start_date_local,9,10)),
           year_dec =year + (month-1)/12+(day-1)/30/12,
           date_dec = (month-1)/12+(day-1)/30/12) %>%
    filter(type=='Run')

if(file.exists('activities.rda')){
    load('activities.rda')
}else{
    activities <- NULL
}
for(i in df %>% filter(!id%in%activities$id) %>% select(id) %>% pull){
    req <- GET(
        url = paste("https://www.strava.com/api/v3/activities/",i,sep=''),
        config = token
    )
    if(req$status_code==429){
        print('Waiting 15 minutes...')
        Sys.sleep(5*60)
        print('Waiting 10 minutes...')
        Sys.sleep(5*60)
        print('Waiting 5 minutes...')
        Sys.sleep(5*60)
    }
    activity <- fromJSON(req %>% content(as='text'),flatten=TRUE)
    if('splits_metric'%in%names(activity)){
        details <- data.frame(activity[names(activity)%in%c('distance','id','start_date_local','average_speed','average_cadence','average_heartrate')])
        splits <- data.frame(activity[names(activity)%in%c('splits_metric')]) %>% select(splits_metric.distance,splits_metric.moving_time,splits_metric.split,splits_metric.average_speed)
        activity_detail <- data.frame(details,splits)
        activities <- activities %>% bind_rows(activity_detail)
    }
}
save(activities,file='activities.rda')

mss <- function(x,y) (x*60+y)/60
mst <- function(x,y){
    y <- round(y)
    if(y<10) output <- paste(x,y,sep=':0')
    if(y>=10) output <- paste(x,y,sep=':')
    if(x==0 & y<0 & y>-10) output <- paste("-",x,':0',abs(y),sep='')
    if(x==0 & y<0 & y< -10) output <- paste("-",x,':',abs(y),sep='')
    return(output)
}
pace_speed <- function(pace) 1000/pace/60
speed_pace <- function(speed) 1000/speed/60

label_ms <- list(c(3,0),c(3,15),c(3,30),c(3,45),
                 c(4,0),c(4,15),c(4,30),c(4,45),
                 c(5,0),c(5,15),c(5,30),c(5,45),
                 c(6,0),c(6,15),c(6,30),c(6,45),
                 c(7,0),c(7,30),c(8,00),c(8,30),
                 c(9,0),c(10,0),c(11,0),c(12,0))
label_pace <- unlist(lapply(label_ms,function(z) mss(z[1],z[2])))
label_speed <- sapply(label_pace,'pace_speed')
label_text <- unlist(lapply(label_ms,function(z) mst(z[1],z[2])))

historical_plot <- function(dist=3000,dist_to,act_type=c('Run','Ride','Swin'),year_start=2010){
    if(missing(dist_to)) dist_to <- dist+500

    temp <- df %>%
        filter(distance>=dist & distance<dist_to & type==act_type[1] & year>=year_start) %>%
        arrange(year_dec) %>%
        mutate(pdate = lag(year_dec),
               break_dec = year_dec - pdate,
               streak_broken = as.numeric(break_dec>=(1/24)),
               streak_broken = if_else(is.na(streak_broken),0,streak_broken),
               streak_no = factor(cumsum(streak_broken))) %>%
        select(id,distance,moving_time,elapsed_time,streak_no,start_date_local,year,year_dec,average_speed)

    ggplot(temp,aes(x=year_dec,y=average_speed,colour=factor(year))) +
        geom_point(size=3) +
        geom_line(size=0.5,linetype=2,colour='black') +
        scale_y_continuous(breaks=label_speed,labels=label_text) +
        scale_x_continuous(breaks=(2000:2050)) +
        xlab('\nDate') +
        ylab('Pace\n') +
        theme(panel.grid.minor.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text=element_text(size=18),
              axis.title=element_text(size=22),
              legend.position='none')
}
historical_plot(dist=5000,year_start=2019)

split_plot <- function(dist=3000,dist_to,act_type=c('Run','Ride','Swin')){
    if(missing(dist_to)) dist_to <- dist+500

    activity_splits <- activities %>%
        filter(distance>=dist & distance<dist_to) %>%
        filter(splits_metric.split<=floor(dist/1000)) %>%
        group_by(splits_metric.split) %>%
        arrange(desc(start_date_local)) %>%
        mutate(N=row_number()) %>%
        ungroup() %>%
        arrange(desc(average_speed)) %>%
        mutate(FR=row_number()) %>%
        group_by(id) %>%
        mutate(ORD=min(N),
               FORD=min(FR),
               Latest=as.numeric(ORD==1),
               Previous=as.numeric(ORD==2),
               Fastest=as.numeric(FORD==1),
               Class=case_when(Latest==1~'L',
                               Previous==1~'P',
                               Fastest==1~'F',
                               TRUE~'X'),
               Label=if_else(Class!='X' & splits_metric.split==1,paste(substr(start_date_local,1,10),' (',mst(floor(speed_pace(average_speed)),60*(speed_pace(average_speed)-floor(speed_pace(average_speed)))),')',sep=''),NA_character_))

    MEDS <- activity_splits %>%
        group_by(splits_metric.split) %>%
        summarise(splits_metric.average_speed=median(splits_metric.average_speed)) %>%
        mutate(Class='X',id=0)

    p <- ggplot(activity_splits %>% arrange(start_date_local),
           aes(x=factor(splits_metric.split),y=splits_metric.average_speed,group=id
               ,colour=Class,size=Class,alpha=Class)) +
        geom_line(size=1.2) +
        geom_point() +
        scale_y_continuous(breaks=label_speed,labels=label_text) +
        xlab('\nSplit') +
        ylab('Pace\n') +
        scale_colour_manual(values=c('X'='grey','L'='#D55E00','F'='#009E73','P'='#0072B2'),guide='none') +
        scale_size_manual(values=c('X'=1,'L'=3,'F'=3,'P'=2),guide='none') +
        scale_alpha_manual(values=c('X'=0.6,'L'=1,'F'=1,'P'=1),guide='none') +
        labs(
            title = paste('Split Time Comparisons (',round(dist/1000,1),'-',round(dist_to/1000,1),'km)',"
    <p><span style='font-size:16pt'>Median Pace,
    <span style='color:#D55E00;'>Latest</span>,
    <span style='color:#0072B2;'>Previous</span>, and
    <span style='color:#009E73;'>Fastest</span>
    </span>",sep='')) +
        theme(panel.grid.minor=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text=element_text(size=18),
              axis.title=element_text(size=22),
              plot.title = element_markdown(lineheight = 1.1)) +
        geom_line(data=MEDS,colour='black',linetype=2) +
        geom_point(data=MEDS,colour='black',size=2) +
        geom_text_repel(mapping=aes(label=Label,x=1),hjust=0,size=4)
    return(p)
}
r3 <- split_plot(dist=3000)
r5 <- split_plot(dist=5000); r5
r10 <- split_plot(dist=10000)

comparison_plot <- function(dist=3000,dist_to,act_type=c('Run','Ride','Swin')){
    if(missing(dist_to)) dist_to <- dist+500

    activity_splits <- activities %>%
        filter(distance>=dist & distance<dist_to) %>%
        filter(splits_metric.split<=floor(dist/1000)) %>%
        group_by(splits_metric.split) %>%
        arrange(desc(start_date_local)) %>%
        mutate(N=row_number()) %>%
        ungroup() %>%
        arrange(desc(average_speed)) %>%
        mutate(FR=row_number()) %>%
        group_by(id) %>%
        mutate(ORD=min(N),
               FORD=min(FR),
               Latest=as.numeric(ORD==1),
               Previous=as.numeric(ORD==2),
               Fastest=as.numeric(FORD==1),
               Class=case_when(Latest==1~'L',
                               Previous==1~'P',
                               Fastest==1~'F',
                               TRUE~'X'),
               Label=if_else(Class!='X' & splits_metric.split==1,paste(substr(start_date_local,1,10),' (',mst(floor(speed_pace(average_speed)),60*(speed_pace(average_speed)-floor(speed_pace(average_speed)))),')',sep=''),NA_character_)) %>%
        filter(Class!='X')

    MEDS <- activity_splits %>%
        group_by(splits_metric.split) %>%
        summarise(splits_metric.average_speed=median(splits_metric.average_speed)) %>%
        mutate(Class='X',id=0)

    Latest <- activity_splits %>% filter(Class=='L')
    Previous <- activity_splits %>% filter(Class=='P')
    Fastest <- activity_splits %>% filter(Class=='F')

    label_ms_d <- list(c(-2,0),c(-1,15),c(-1,30),c(-1,45),
                     c(-1,0),c(0,-15),c(0,-30),c(0,-45),
                     c(0,0),c(0,15),c(0,30),c(0,45),
                     c(1,0),c(1,15),c(1,30),c(1,45),
                     c(2,0))
    label_pace_d <- unlist(lapply(label_ms_d,function(z) mss(z[1],z[2])))
    label_speed_d <- sapply(label_pace_d,'pace_speed')
    label_text_d <- unlist(lapply(label_ms_d,function(z) mst(z[1],z[2])))

    PC <- Latest %>%
        left_join(Previous,by='splits_metric.split') %>%
        select(start_date_local.x,splits_metric.split,average_speed.x,average_speed.y,splits_metric.average_speed.x,splits_metric.average_speed.y,start_date_local.y) %>%
        mutate(Comparison='Previous')
    FC <- Latest %>%
        left_join(Fastest,by='splits_metric.split') %>%
        select(start_date_local.x,splits_metric.split,average_speed.x,average_speed.y,splits_metric.average_speed.x,splits_metric.average_speed.y,start_date_local.y) %>%
        mutate(Comparison='Fastest')
    MC <- Latest %>%
        left_join(MEDS,by='splits_metric.split') %>%
        select(start_date_local.x=start_date_local,splits_metric.split,average_speed.x=average_speed,splits_metric.average_speed.x,splits_metric.average_speed.y) %>%
        mutate(Comparison='Median',average_speed.y=mean(splits_metric.average_speed.y))
    PC %>% bind_rows(FC) %>% bind_rows(MC) %>%
        mutate(Time.x = 1000/splits_metric.average_speed.x,
               Time.y = 1000/splits_metric.average_speed.y,
               Speed_Diff = splits_metric.average_speed.x - splits_metric.average_speed.y,
               Time_Diff = Time.y - Time.x,
               Sign = factor(sign(Time_Diff))) %>%
        mutate(Comparison=factor(Comparison,c('Previous','Median','Fastest'))) %>%
        ggplot(aes(x=splits_metric.split,y=Time_Diff,fill=Sign)) +
        geom_col() +
        facet_wrap(~Comparison) +
        scale_fill_manual(values=c('1'='darkgreen','0'='grey40','-1'='darkred'),guide='none') +
        xlab('\nSplit') +
        ylab('Time Difference (sec)\n') +
        scale_y_continuous(breaks=seq(-60,60,by=10)) +
        theme(panel.grid.minor.x=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.minor.y=element_line(colour='white',linetype=2),
              axis.text=element_text(size=18),
              axis.title=element_text(size=22),
              strip.text=element_text(size=22),
              plot.title=element_text(size=22),
              plot.subtitle=element_text(size=18)) +
        ggtitle(paste('Split Time Comparisons (',round(dist/1000,1),'-',round(dist_to/1000,1),'km)',sep=''),subtitle='Latest run compared to:')
}
