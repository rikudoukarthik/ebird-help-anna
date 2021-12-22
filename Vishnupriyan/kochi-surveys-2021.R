library(tidyverse)
library(lubridate)
library(patchwork)

theme_set(theme_bw())

### Salim Ali ----------


SArawdata <- read_csv("Vishnupriyan/Salim Ali Bird Count Data.csv")

SArawdata <- SArawdata %>% 
  select(-c(`Taxonomic Order`, `Area Covered (ha)`, `ML Catalog Numbers`))

### changing names to EBD format
names(SArawdata) <- c("SAMPLING.EVENT.IDENTIFIER","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                      "STATE.CODE","COUNTY","LOCALITY.ID","LOCALITY","LATITUDE","LONGITUDE",
                      "OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","PROTOCOL.TYPE",
                      "DURATION.MINUTES","ALL.SPECIES.REPORTED","EFFORT.DISTANCE.KM",
                      "NUMBER.OBSERVERS","BREEDING.CODE","OBSERVATION.COMMENTS","TRIP.COMMENTS")
SArawdata <- SArawdata %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!(any(OBSERVATION.COUNT == "X")), !(ALL.SPECIES.REPORTED == 0)) %>% 
  ungroup()
SArawdata <- SArawdata %>% 
  mutate(PROTOCOL.TYPE = case_when(grepl("Traveling", SArawdata$PROTOCOL.TYPE) ~ "Traveling",
                                   grepl("Stationary", SArawdata$PROTOCOL.TYPE) ~ "Stationary",
                                   grepl("Casual", SArawdata$PROTOCOL.TYPE) ~ "Incidental"),
         OBSERVATION.COUNT = as.integer(OBSERVATION.COUNT))


### adding useful columns 

met_week <- function(dates) {
  require(lubridate)
  normal_year <- c((0:363 %/% 7 + 1), 52)
  leap_year   <- c(normal_year[1:59], 9, normal_year[60:365])
  year_day    <- yday(dates)
  return(ifelse(leap_year(dates), leap_year[year_day], normal_year[year_day])) 
}

SAdata <- SArawdata %>% 
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE), 
         DAY.Y = yday(OBSERVATION.DATE), 
         WEEK.Y = met_week(OBSERVATION.DATE), 
         M.YEAR = if_else(DAY.Y <= 151, YEAR-1, YEAR), # from 1st June to 31st May
         WEEK.MY = if_else(WEEK.Y > 21, WEEK.Y-21, 52-(21-WEEK.Y)))



### visualising

SAdata0 <- SAdata %>% 
  filter(PROTOCOL.TYPE != "Incidental") %>% 
  filter(!(grepl("sp.", COMMON.NAME)),
         !(grepl("/", COMMON.NAME)))

# locations sampled in only one year
SAdata1 <- SAdata0 %>% 
  group_by(LOCALITY) %>% 
  filter(any(n_distinct(YEAR) < 2)) %>% ungroup() %>% 
  distinct(LOCALITY)
  
## comparing two years

# NOTE: merging all "Kidangu Road" locations into one 
SAdata2 <- SAdata0 %>% 
  mutate(LOCALITY = case_when(grepl("Kidangu", LOCALITY) ~ "Kidangu Road",
                              TRUE ~ LOCALITY)) %>% 
  group_by(LOCALITY) %>% 
  filter(!any(n_distinct(YEAR) < 2)) %>% ungroup() %>% 
  group_by(LOCALITY, YEAR) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.SP = n_distinct(COMMON.NAME)) %>% ungroup() %>% 
  arrange(YEAR, desc(NO.SP))

SAdata2a <- SAdata2 %>% filter(NO.LISTS > 2) %>% 
  mutate(L.CODE = case_when(LOCALITY == "Kalamassery--HMT Estate Area" ~ "HMT",
                            LOCALITY == "Kidangu Road" ~ "KID",
                            LOCALITY == "Mangalavanam Bird Sanctuary" ~ "MBS"))

SAfig2 <- (ggplot(filter(SAdata2a %>% 
                          pivot_longer(cols = c("NO.LISTS","NO.SP"), 
                                       names_to = "WHAT", values_to = "NUMBER") %>% 
                          mutate(WHAT = factor(WHAT, levels = c("NO.SP","NO.LISTS"))) %>% 
                          arrange(desc(NUMBER)),
                        YEAR == 2020)) +
  geom_col(aes(reorder(L.CODE, -NUMBER), NUMBER, fill = WHAT), position = "dodge") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(4, "Paired")[2],
                               RColorBrewer::brewer.pal(4, "Paired")[1]),
                    name = "Number of",
                    labels = c("species", "lists")) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Location", y = "Number", title = "2020")) /
  (ggplot(filter(SAdata2a %>% 
                  pivot_longer(cols = c("NO.LISTS","NO.SP"), 
                               names_to = "WHAT", values_to = "NUMBER") %>% 
                  mutate(WHAT = factor(WHAT, levels = c("NO.SP","NO.LISTS"))) %>% 
                  arrange(desc(NUMBER)),
                YEAR == 2021)) +
  geom_col(aes(reorder(L.CODE, -NUMBER), NUMBER, fill = WHAT), position = "dodge") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(4, "Paired")[4],
                               RColorBrewer::brewer.pal(4, "Paired")[3]),
                    name = "Number of",
                    labels = c("species", "lists")) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Location", y = "Number", title = "2021")) &
  plot_annotation(title = "Salim Ali Bird Counts",
                  subtitle = "Showing total species richness and birding effort in three locations",
                  caption = expression(italic("HMT: 'Kalamassery--HMT Estate Area'; KID: 'Kidangu Road'; MBS: 'Mangalavanam Bird Sanctuary'"))) &
  theme(plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 20)))


# total counts of bird species seen more than once per location per year
SAdata3 <- SAdata0 %>% 
  mutate(LOCALITY = case_when(grepl("Kidangu", LOCALITY) ~ "Kidangu Road",
                              TRUE ~ LOCALITY)) %>% 
  group_by(LOCALITY) %>% 
  filter(!any(n_distinct(YEAR) < 2)) %>% 
  group_by(LOCALITY, YEAR) %>% 
  filter(!any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% ungroup() %>% 
  group_by(LOCALITY, COMMON.NAME) %>% 
  filter(!any(n_distinct(YEAR) < 2)) %>% 
  group_by(LOCALITY, COMMON.NAME, YEAR) %>% 
  filter(!any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% 
  summarise(TOT.COUNT = sum(OBSERVATION.COUNT)) %>% ungroup() 


# freq of reporting per each location not possible because not many lists per location
# changes between two years seen here affected by change in effort at different locations
  

SAdata4 <- SAdata0 %>% 
  group_by(YEAR) %>% 
  mutate(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>% 
  group_by(COMMON.NAME) %>% 
  filter(!any(n_distinct(YEAR) < 2)) %>% 
  group_by(COMMON.NAME, YEAR) %>% 
  summarise(TOT.LISTS = min(TOT.LISTS),
            NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  mutate(R.FREQ = round(NO.LISTS/TOT.LISTS,2)) %>% 
  arrange(YEAR, desc(R.FREQ)) %>% ungroup()

SAdata4a <- SAdata4 %>% filter(YEAR == 2020)
SAdata4b <- SAdata4 %>% filter(YEAR == 2021)


set.seed(31)
SAfig4 <- (ggplot(rbind(SAdata4a[1:5,], 
                        filter(SAdata4a, 
                               COMMON.NAME %in% sample((SAdata4a %>% filter(R.FREQ < 0.05))$COMMON.NAME, 5))),
                  aes(reorder(COMMON.NAME, -R.FREQ), R.FREQ*100)) +
             geom_col(fill = c(rep(RColorBrewer::brewer.pal(4, "Paired")[2], 5), 
                               rep(RColorBrewer::brewer.pal(4, "Paired")[1], 5))) + 
             geom_vline(xintercept = 5.5) +
             scale_y_continuous(breaks = seq(0,100,5), 
                                limits = c(0, 60)) +
             scale_x_discrete(labels = c("BrKi","HoCr","IPHe","LiCo","BlDr",
                                         "Shik","YBBa","AsWo","BBFl","RWLa")) +
             labs(x = "Species", y = "Frequency of reporting (%)", title = "2020")) /
  (ggplot(rbind(SAdata4b[1:5,], 
                filter(SAdata4b, 
                       COMMON.NAME %in% sample((SAdata4b %>% filter(R.FREQ < 0.05))$COMMON.NAME, 5))),
          aes(reorder(COMMON.NAME, -R.FREQ), R.FREQ*100)) +
     geom_col(fill = c(rep(RColorBrewer::brewer.pal(4, "Paired")[4], 5), 
                       rep(RColorBrewer::brewer.pal(4, "Paired")[3], 5))) + 
     geom_vline(xintercept = 5.5) +
     scale_y_continuous(breaks = seq(0,100,5), 
                        limits = c(0, 80)) +
     scale_x_discrete(labels = c("HoCr","RoPi","BrKi","IPHe","BlDr",
                                 "APSw","GHSw","JuOw","WRMu","YeBi")) +
     labs(x = "Species", y = "Frequency of reporting (%)", title = "2021")) &
  plot_annotation(title = "Reporting frequency of birds in Salim Ali Bird Count",
                  subtitle = "Showing five most common and random five uncommon species",
                  caption = expression(italic("BrKi: Brahminy Kite; HoCr: House Crow; IPHe: Indian Pond-Heron; LiCo: Little Cormorant; BlDr: Black Drongo; Shik: Shikra; YBBa: Yellow-billed Babbler; AsWo: Ashy Woodswallow; BBFl: Brown-breasted Flycatcher; \nRWLa: Red-wattled Lapwing; RoPi: Rock Pigeon (Feral Pigeon); APSw: Asian Palm-Swift; GHSw: Grey-headed Swamphen; JuOw: Jungle Owlet; WRMu: White-rumped Munia; YeBi: Yellow Bittern"))) &
  theme(plot.caption = element_text(size = 6.5, hjust = 0, margin = margin(t = 20)))




ggsave("Vishnupriyan/kochi-surveys-2021_SAfig2.png", SAfig2, 
       width = 9, height = 12, units = "in", dpi = 500)

write_csv(SAdata3, "Vishnupriyan/kochi-surveys-2021_SAdata3.csv")  

ggsave("Vishnupriyan/kochi-surveys-2021_SAfig4.png", SAfig4, 
       width = 9, height = 12, units = "in", dpi = 500)




### Thattekad ----------


THrawdata <- read_csv("Vishnupriyan/Thattekkad Bird Count Data.csv")

THrawdata <- THrawdata %>% 
  select(-c(`Taxonomic Order`, `Area Covered (ha)`, `ML Catalog Numbers`))

### changing names to EBD format
names(THrawdata) <- c("SAMPLING.EVENT.IDENTIFIER","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                      "STATE.CODE","COUNTY","LOCALITY.ID","LOCALITY","LATITUDE","LONGITUDE",
                      "OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","PROTOCOL.TYPE",
                      "DURATION.MINUTES","ALL.SPECIES.REPORTED","EFFORT.DISTANCE.KM",
                      "NUMBER.OBSERVERS","BREEDING.CODE","OBSERVATION.COMMENTS","TRIP.COMMENTS")
THrawdata <- THrawdata %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!(any(OBSERVATION.COUNT == "X")), !(ALL.SPECIES.REPORTED == 0)) %>% 
  ungroup()
THrawdata <- THrawdata %>% 
  mutate(PROTOCOL.TYPE = case_when(grepl("Traveling", THrawdata$PROTOCOL.TYPE) ~ "Traveling",
                                   grepl("Stationary", THrawdata$PROTOCOL.TYPE) ~ "Stationary",
                                   grepl("Casual", THrawdata$PROTOCOL.TYPE) ~ "Incidental"),
         OBSERVATION.COUNT = as.integer(OBSERVATION.COUNT))


### adding useful columns 

met_week <- function(dates) {
  require(lubridate)
  normal_year <- c((0:363 %/% 7 + 1), 52)
  leap_year   <- c(normal_year[1:59], 9, normal_year[60:365])
  year_day    <- yday(dates)
  return(ifelse(leap_year(dates), leap_year[year_day], normal_year[year_day])) 
}

THdata <- THrawdata %>% 
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE), 
         DAY.Y = yday(OBSERVATION.DATE), 
         WEEK.Y = met_week(OBSERVATION.DATE), 
         M.YEAR = if_else(DAY.Y <= 151, YEAR-1, YEAR), # from 1st June to 31st May
         WEEK.MY = if_else(WEEK.Y > 21, WEEK.Y-21, 52-(21-WEEK.Y)))

# too many locations (way more than "five transects being monitored for years")



### visualising

THdata0 <- THdata %>% 
  filter(PROTOCOL.TYPE != "Incidental") %>% 
  filter(!(grepl("sp.", COMMON.NAME)),
         !(grepl("/", COMMON.NAME)))

# locations with only 1 list
# other locations are probably the 5 transects
THdata1 <- THdata0 %>% 
  mutate(LOCALITY = case_when(grepl("Inchathotty", LOCALITY) ~ "Inchathotty Thattekad Road",
                               TRUE ~ LOCALITY)) %>% 
  group_by(LOCALITY) %>% 
  filter(any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% ungroup() %>% 
  distinct(LOCALITY)

# comparing transects by number of species reported
# NOTE: merging all "Inchathotty Thattekad Road" locations into one 
THdata2 <- THdata0 %>% 
  mutate(LOCALITY = case_when(grepl("Inchathotty", LOCALITY) ~ "Inchathotty Thattekad Road",
                              TRUE ~ LOCALITY)) %>% 
  group_by(LOCALITY) %>% 
  filter(!any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.SP = n_distinct(COMMON.NAME)) %>% ungroup() %>% 
  arrange(desc(NO.SP)) %>% 
  mutate(L.CODE = case_when(LOCALITY == "Dr.Salim Ali Bird Sanctuary--Kallippara" ~ "SABS_K",
                            LOCALITY == "Dr. Salim Ali Bird Sanctuary" ~ "SABS_G",
                            LOCALITY == "Thattekad Bird Sanctuary--Urulanthanni" ~ "TBS_U",
                            LOCALITY == "Thattekkad Bird Sanctuary--General Area" ~ "TBS_G",
                            LOCALITY == "Inchathotty Thattekad Road" ~ "INCH",
                            LOCALITY == "Thattekkad" ~ "THAT"))

THfig2 <- ggplot(THdata2 %>% 
                    pivot_longer(cols = c("NO.LISTS","NO.SP"), 
                                 names_to = "WHAT", values_to = "NUMBER") %>% 
                    mutate(WHAT = factor(WHAT, levels = c("NO.SP","NO.LISTS"))) %>% 
                    arrange(desc(NUMBER))) +
  geom_col(aes(reorder(L.CODE, -NUMBER), NUMBER, fill = WHAT), position = "dodge") +
  scale_fill_manual(values = c("goldenrod", "dark green"),
                    name = "Number of",
                    labels = c("species", "lists")) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Location", 
       y = "Number", 
       title = "Thattekkad Bird Count 2021 transects",
       subtitle = "Showing total species richness and birding effort",
       caption = expression(italic("INCH: 'Inchathotty Thattekad Road'; SABS_G: 'Dr. Salim Ali Bird Sanctuary'; SABS_K: 'Dr.Salim Ali Bird Sanctuary--Kallippara'; TBS_G: 'Thattekkad Bird Sanctuary--General Area'; \nTBS_U: 'Thattekad Bird Sanctuary--Urulanthanni'; THAT: 'Thattekkad'"))) + 
  theme(plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 20)))

# THfig2b <- ggplot(THdata2, aes(NO.LISTS, NO.SP)) +
#   geom_point(aes(size = NO.LISTS, colour = L.CODE))


# total counts of bird species seen more than once per location
THdata3 <- THdata0 %>% 
  mutate(LOCALITY = case_when(grepl("Inchathotty", LOCALITY) ~ "Inchathotty Thattekad Road",
                              TRUE ~ LOCALITY)) %>% 
  group_by(LOCALITY) %>% 
  filter(!any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% ungroup() %>% 
  group_by(LOCALITY, COMMON.NAME) %>% 
  filter(!any(n_distinct(SAMPLING.EVENT.IDENTIFIER) < 2)) %>% 
  summarise(TOT.COUNT = sum(OBSERVATION.COUNT)) %>% ungroup() 


THdata4 <- THdata0 %>% 
  mutate(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  group_by(COMMON.NAME) %>% 
  summarise(TOT.LISTS = min(TOT.LISTS),
            NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  mutate(R.FREQ = round(NO.LISTS/TOT.LISTS,2)) %>% 
  arrange(desc(R.FREQ)) %>% ungroup()

set.seed(16)
THfig4 <- ggplot(rbind(THdata4[1:5,], 
                       filter(THdata4, 
                              COMMON.NAME %in% sample((THdata4 %>% filter(R.FREQ < 0.05))$COMMON.NAME, 5))),
                 aes(reorder(COMMON.NAME, -R.FREQ), R.FREQ*100)) +
  geom_col(fill = c(rep("goldenrod", 5), rep("dark green", 5))) + 
  geom_vline(xintercept = 5.5) +
  scale_y_continuous(breaks = seq(0,50,5), 
                     limits = c(0, 40)) +
  scale_x_discrete(labels = c("GRTD","SHMy","MaPa","YBBu","MGHo",
                              "CHEa","ISBa","MaWo","InEg","LBCr")) +
  labs(x = "Species",
       y = "Frequency of reporting (%)",
       title = "Reporting frequency of birds in Thattekkad Bird Count 2021",
       subtitle = "Showing five most common and random five uncommon species",
       caption = expression(italic("GRTD: Greater Racket-tailed Drongo; SHMy: Southern Hill Myna; MaPa: Malabar Parakeet; YBBu: Yellow-browed Bulbul; MGHo: Malabar Grey Hornbill; CHEa: Changeable Hawk-Eagle; \nISBa: Indian Scimitar-Babbler; MaWo: Malabar Woodshrike; InEg: Intermediate Egret; LBCr: Large-billed Crow"))) +
  theme(plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 20)))
  


ggsave("Vishnupriyan/kochi-surveys-2021_THfig2.png", THfig2, 
       width = 9, height = 6, units = "in", dpi = 500)

write_csv(THdata3, "Vishnupriyan/kochi-surveys-2021_THdata3.csv")  

ggsave("Vishnupriyan/kochi-surveys-2021_THfig4.png", THfig4, 
       width = 9, height = 6, units = "in", dpi = 500)
