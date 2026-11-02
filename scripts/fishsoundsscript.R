# load packages ------
library(tidyverse)
library(here)
library(skimr)
library(ggplot2)
library(gridExtra)

# read in data ------
fishsounds <- read.csv(here("Thesis", 
                            "fishsounds.csv"))

# cleaning data -----
fishsounds_clean <- fishsounds |>
  mutate(behavior_clean = case_when(
    behavior== "1" ~ "warning",
    behavior== "2" ~ "territorial",
    behavior== "3" ~ "agonistic",
    behavior== "4" ~ "startle", 
    behavior== "5" ~ "courtship", 
    behavior== "6" ~ "unknown"))

fishsounds_clean <- fishsounds_clean|>
  rename(LowFrequency = Low.Freq..Hz.,
         HighFrequency = High.Freq..Hz.,
         LowFrequency = Low.Freq..Hz.,
         Duration = Dur.90...s.,
         CenterFrequency = Center.Freq..Hz.,
         PeakFrequency = Peak.Freq..Hz.,
         Bandwidth = BW.90...Hz.)
#singling out agonistic behavior
fishsounds_clean <- fishsounds_clean |>
  mutate(behavior_agonistic = case_when(
    behavior_clean == "agonistic" ~ "agonistic",
    TRUE ~ "not agonistic"))

glimpse(fishsounds_clean)

#standardizing for later 
#fishsounds_clean <- fishsounds_clean|>
#  mutate(across(c(LowFrequency, 
 #                 HighFrequency, 
  #                Duration, 
   #               CenterFrequency, 
    #              PeakFrequency, 
     #             Bandwith, 
      #            Pulses), 
       #         ~as.vector(scale(.))))


# histograms-----
#duration
holocentrusduration_histogram <- fishsounds_clean |>
  filter(genus == "Holocentrus") |>
  ggplot(aes(x = Duration)) +
  geom_histogram()+
  labs(title = "Holocentrus Duration")
holocentrusduration_histogram

myripristisduration_histogram <- fishsounds_clean |>
  filter(genus == "Myripristis") |>
  ggplot(aes(x = Duration)) +
  geom_histogram()+
  labs(title = "Myripristis Duration")
myripristisduration_histogram

neoniphonduration_histogram <- fishsounds_clean |>
  filter(genus == "Neoniphon") |>
  ggplot(aes(x = Duration)) +
  geom_histogram()+
  labs(title = "Neoniphon Duration")
neoniphonduration_histogram

grid.arrange(holocentrusduration_histogram,
             myripristisduration_histogram,
             neoniphonduration_histogram,
             ncol=2) 
#centerfreq
holocentruscenterfreq_histogram <- fishsounds_clean |>
  filter(genus == "Holocentrus") |>
  ggplot(aes(x = CenterFrequency)) +
  geom_histogram()+
  labs(title = "Holocentrus Center Freq")
holocentruscenterfreq_histogram

myripristiscenterfreq_histogram <- fishsounds_clean |>
  filter(genus == "Myripristis") |>
  ggplot(aes(x = CenterFrequency)) +
  geom_histogram()+
  labs(title = "Myripristis Center Freq")
myripristiscenterfreq_histogram

neoniphoncenterfreq_histogram <- fishsounds_clean |>
  filter(genus == "Neoniphon") |>
  ggplot(aes(x = CenterFrequency)) +
  geom_histogram()+
  labs(title = "Neoniphon Center Freq")
neoniphoncenterfreq_histogram

grid.arrange(holocentruscenterfreq_histogram,
             myripristiscenterfreq_histogram,
             neoniphoncenterfreq_histogram,
             ncol=2)
#peakfreq
myripristispeakfreq_histogram <- fishsounds_clean |>
  filter(genus == "Myripristis") |>
  ggplot(aes(x = PeakFrequency)) +
  geom_histogram()+
  labs(title = "Myripristis Peak Freq")
myripristispeakfreq_histogram

holocentruspeakfreq_histogram <- fishsounds_clean |>
  filter(genus == "Holocentrus") |>
  ggplot(aes(x = PeakFrequency)) +
  geom_histogram()+
  labs(title = "Holocentrus Peak Freq")
holocentruspeakfreq_histogram
  
neoniphonpeakfreq_histogram <- fishsounds_clean |>
  filter(genus == "Neoniphon") |>
  ggplot(aes(x = PeakFrequency)) +
  geom_histogram()+
  labs(title = "Neoniphon Peak Freq")
neoniphonpeakfreq_histogram

grid.arrange(holocentruspeakfreq_histogram,
             myripristispeakfreq_histogram,
             neoniphonpeakfreq_histogram,
             ncol=2)
#bandwidth
myripristisbandwidth_histogram <- fishsounds_clean |>
  filter(genus == "Myripristis") |>
  ggplot(aes(x = Bandwidth)) +
  geom_histogram()+
  labs(title = "Myripristis Bandwidth")
myripristisbandwidth_histogram

holocentrusbandwidth_histogram <- fishsounds_clean |>
  filter(genus == "Holocentrus") |>
  ggplot(aes(x = Bandwidth)) +
  geom_histogram()+
  labs(title = "Holocentrus Bandwidth")
holocentrusbandwidth_histogram

neoniphonbandwidth_histogram <- fishsounds_clean |>
  filter(genus == "Neoniphon") |>
  ggplot(aes(x = Bandwidth)) +
  geom_histogram()+
  labs(title = "Neoniphon Bandwidth")
neoniphonbandwidth_histogram

grid.arrange(holocentrusbandwidth_histogram,
             myripristisbandwidth_histogram,
             neoniphonbandwidth_histogram,
             ncol=2)
#pulse
myripristispulses_histogram <- fishsounds_clean |>
  filter(genus == "Myripristis") |>
  ggplot(aes(x = Pulses)) +
  geom_histogram()+
  labs(title = "Myripristis Pulse Count")
myripristispulses_histogram

holocentruspulses_histogram <- fishsounds_clean |>
  filter(genus == "Holocentrus") |>
  ggplot(aes(x = Pulses)) +
  geom_histogram()+
  labs(title = "Holocentrus Pulse Count")
holocentruspulses_histogram

neoniphonpulses_histogram <- fishsounds_clean |>
  filter(genus == "Neoniphon") |>
  ggplot(aes(x = Pulses)) +
  geom_histogram()+
  labs(title = "Neoniphon Pulse Count")


grid.arrange(holocentruspulses_histogram,
             myripristispulses_histogram,
             neoniphonpulses_histogram,
             ncol=2)

# mean overview-----
#general
mean_table <- fishsounds_clean|>
  group_by(genus)|>
  summarise(
    mean_duration = mean(Duration),
    sd_duration = sd(Duration),
    mean_centerfreq = mean(CenterFrequency),
    sd_centerfreq = sd(CenterFrequency),
    mean_peakfreq = mean(PeakFrequency),
    sd_peakfreq = sd(PeakFrequency),
    mean_bandwidth = mean(Bandwidth),
    sd_bandwidth = sd(Bandwidth),
    mean_pulses = mean(Pulses),
    sd_pulses = sd(Pulses))

print(mean_table)

#subfamily 
subfamilymean_table <- fishsounds_clean|>
  group_by(subfamily)|>
  summarise(
    mean_duration = mean(Duration),
    sd_duration = sd(Duration),
    mean_centerfreq = mean(CenterFrequency),
    sd_centerfreq = sd(CenterFrequency),
    mean_peakfreq = mean(PeakFrequency),
    sd_peakfreq = sd(PeakFrequency),
    mean_bandwidth = mean(Bandwidth),
    sd_bandwidth = sd(Bandwidth),
    mean_pulses = mean(Pulses),
    sd_pulses = sd(Pulses))

print(subfamilymean_table)

# general distribution-----
#duration
duration_general <- ggplot(fishsounds_clean, 
                           aes(x=genus, 
                               y=Duration, 
                               color=genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#centerfreq
centerfreq_general <- ggplot(fishsounds_clean, 
                             aes(x=genus, 
                                 y=CenterFrequency, 
                                 color=genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#peakfreq
peakfreq_general <- ggplot(fishsounds_clean, 
                           aes(x=genus, 
                               y=PeakFrequency, 
                               color=genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#bandwidth
bandwidth_general<- ggplot(fishsounds_clean, 
                          aes(x=genus, 
                              y=Bandwidth, 
                              color=genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#pulse count
pulsecount_general <- ggplot(fishsounds_clean, 
                             aes(x=genus, 
                                 y=Pulses, 
                                 color=genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

grid.arrange(duration_general, 
             centerfreq_general, 
             peakfreq_general, 
             bandwidth_general, 
             pulsecount_general, 
             ncol=2) 

# distribution fill = behavior -----
duration_generalbybehavior <- ggplot(fishsounds_clean, 
                                     aes(x=genus, 
                                         y=Duration)) +
  geom_violin()+
  geom_point(aes(color = behavior_clean),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#centerfreq
centerfreq_generalbybehavior <- ggplot(fishsounds_clean, 
                                       aes(x=genus, 
                                           y=CenterFrequency)) +
  geom_violin()+
  geom_point(aes(color = behavior_clean),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#peakfreq
peakfreq_generalbybehavior <- ggplot(fishsounds_clean, 
                                     aes(x=genus, 
                                         y=PeakFrequency)) +
  geom_violin()+
  geom_point(aes(color = behavior_clean),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#bandwidth
bandwidth_generalbybehavior <- ggplot(fishsounds_clean, 
                                     aes(x=genus, 
                                         y=Bandwidth)) +
  geom_violin()+
  geom_point(aes(color = behavior_clean),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#pulse count
pulsecount_generalbybehavior <- ggplot(fishsounds_clean, 
                                       aes(x=genus, 
                                           y=Pulses)) +
  geom_violin()+
  geom_point(aes(color = behavior_clean),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

grid.arrange(duration_generalbybehavior, 
             centerfreq_generalbybehavior, 
             peakfreq_generalbybehavior, 
             bandwidth_generalbybehavior, 
             pulsecount_generalbybehavior, 
             ncol=2) 

# subfamily distribution ---- 
#duration
duration_generalsubfamily <- ggplot(fishsounds_clean, 
                           aes(x=subfamily, 
                               y=Duration, 
                               color=subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#centerfreq
centerfreq_generalsubfamily <- ggplot(fishsounds_clean, 
                             aes(x=subfamily, 
                                 y=CenterFrequency, 
                                 color=subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#peakfreq
peakfreq_generalsubfamily <- ggplot(fishsounds_clean, 
                           aes(x=subfamily, 
                               y=PeakFrequency, 
                               color=subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#bandwidth
bandwidth_generalsubfamily<- ggplot(fishsounds_clean, 
                           aes(x=subfamily, 
                               y=Bandwidth, 
                               color=subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

#pulse count
pulsecount_generalsubfamily <- ggplot(fishsounds_clean, 
                             aes(x=subfamily, 
                                 y=Pulses, 
                                 color=subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2)) +
  theme(legend.position = "none")

grid.arrange(duration_generalsubfamily, 
             centerfreq_generalsubfamily, 
             peakfreq_generalsubfamily, 
             bandwidth_generalsubfamily, 
             pulsecount_generalsubfamily, 
             ncol=2) 

# subfamily agonistic distribution ----
#mean table 
fishsounds_clean|>
  group_by(subfamily)|>
  count(behavior_agonistic)

#mean table
mean_table_agonisticsubfamily <- fishsounds_clean|>
  group_by(subfamily)|>
  filter(behavior_clean == "agonistic")|>
  summarise(
    mean_duration = mean(Duration),
    sd_duration = sd(Duration),
    mean_centerfreq = mean(CenterFrequency),
    sd_centerfreq = sd(CenterFrequency),
    mean_peakfreq = mean(PeakFrequency),
    sd_peakfreq = sd(PeakFrequency),
    mean_bandwidth = mean(Bandwidth),
    sd_bandwidth = sd(Bandwidth),
    mean_pulses = mean(Pulses),
    sd_pulses = sd(Pulses))

print(mean_table_agonisticsubfamily)
#duration
duration_agonisticsubfamily <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = subfamily, 
      y = Duration,
      color = subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#centerfreq
centerfreq_agonisticsubfamily <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = subfamily, 
      y = CenterFrequency,
      color = subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#peakfreq
peakfreq_agonisticsubfamily <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = subfamily, 
      y = PeakFrequency,
      color = subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#bandwidth
bandwidth_agonisticsubfamily <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = subfamily, 
      y = Bandwidth,
      color = subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#pulse count
pulsecount_agonisticsubfamily <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = subfamily, 
      y = Pulses,
      color = subfamily)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

grid.arrange(duration_agonisticsubfamily, 
             centerfreq_agonisticsubfamily, 
             peakfreq_agonisticsubfamily, 
             bandwidth_agonisticsubfamily, 
             pulsecount_agonisticsubfamily, 
             ncol=2)


# distribution fill = agonistic or no ----
#duration
duration_generalbyagonistic <- ggplot(fishsounds_clean, 
                                      aes(x=genus, 
                                          y=Duration)) +
  geom_violin()+
  geom_point(aes(color = behavior_agonistic),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#centerfreq
centerfreq_generalbyagonistic <- ggplot(fishsounds_clean, 
                                        aes(x=genus, 
                                            y=CenterFrequency)) +
  geom_violin()+
  geom_point(aes(color = behavior_agonistic),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#peakfreq
peakfreq_generalbyagonistic <- ggplot(fishsounds_clean, 
                                      aes(x=genus, 
                                          y=PeakFrequency)) +
  geom_violin()+
  geom_point(aes(color = behavior_agonistic),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#bandwidth
bandwidth_generalbyagonistic <- ggplot(fishsounds_clean, 
                                      aes(x=genus, 
                                          y=Bandwidth)) +
  geom_violin()+
  geom_point(aes(color = behavior_agonistic),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

#pulse count
pulsecount_generalbyagonistic <- ggplot(fishsounds_clean, 
                                        aes(x=genus, 
                                            y=Pulses)) +
  geom_violin()+
  geom_point(aes(color = behavior_agonistic),
             position = position_jitter(seed = 1, 
                                        width = 0.2))

grid.arrange(duration_generalbyagonistic, 
             centerfreq_generalbyagonistic, 
             peakfreq_generalbyagonistic, 
             bandwidth_generalbyagonistic, 
             pulsecount_generalbyagonistic, 
             ncol=2) 

# agonistic mean -----
fishsounds_clean|>
  group_by(genus)|>
  count(behavior_agonistic)

#mean table
mean_table_agonistic <- fishsounds_clean|>
  group_by(genus)|>
  filter(behavior_clean == "agonistic")|>
  summarise(
    mean_duration = mean(Duration),
    sd_duration = sd(Duration),
    mean_centerfreq = mean(CenterFrequency),
    sd_centerfreq = sd(CenterFrequency),
    mean_peakfreq = mean(PeakFrequency),
    sd_peakfreq = sd(PeakFrequency),
    mean_bandwidth = mean(Bandwidth),
    sd_bandwidth = sd(Bandwidth),
    mean_pulses = mean(Pulses),
    sd_pulses = sd(Pulses))


# agonistic distribution ----
#duration
duration_agonistic <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = genus, 
      y = Duration,
      color = genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")
            
#centerfreq
centerfreq_agonistic <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = genus, 
      y = CenterFrequency,
      color = genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#peakfreq
peakfreq_agonistic <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = genus, 
      y = PeakFrequency,
      color = genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#bandwidth
bandwidth_agonistic <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = genus, 
      y = Bandwidth,
      color = genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

#pulse count
pulsecount_agonistic <- ggplot(
  fishsounds_clean|>
    filter(behavior_clean == "agonistic"),
  aes(x = genus, 
      y = Pulses,
      color = genus)) +
  geom_violin()+
  geom_point(position = position_jitter(seed = 1, 
                                        width = 0.2))+
  theme(legend.position = "none")

grid.arrange(duration_agonistic, 
             centerfreq_agonistic, 
             peakfreq_agonistic, 
             bandwidth_agonistic, 
             pulsecount_agonistic, 
             ncol=2)


# pulse duration regression ------
#total
ggplot(fishsounds_clean,
       aes(x = Duration,
           y = Pulses))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

pulsedurationFit <- lm(Pulses ~ Duration, 
   data = fishsounds_clean)

summary(pulsedurationFit)
#filtered by genus
ggplot(fishsounds_clean,
       aes(x = Duration,
           y = Pulses,
           color = genus))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

#regressions
pulsedurationFit_holocentrus <- lm(Pulses ~ Duration,
                                   data = fishsounds_clean|>
                                     filter(genus == "Holocentrus"))
summary(pulsedurationFit_holocentrus)

pulsedurationFit_myripristis <- lm(Pulses ~ Duration,
                               data = fishsounds_clean|>
                                 filter(genus == "Myripristis"))
summary(pulsedurationFit_myripristis)

pulsedurationFit_neoniphon <- lm(Pulses ~ Duration, 
                                 data = fishsounds_clean|>
                                   filter(genus == "Neoniphon"))
summary(pulsedurationFit_neoniphon)

# bandwidth duration -----
#total
ggplot(fishsounds_clean,
       aes(x = Duration,
           y = Bandwidth))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

bandwidthdurationFit <- lm(Bandwidth ~ Duration, 
                       data = fishsounds_clean)
summary(bandwidthdurationFit)

summary(bandwidthdurationFit)
#filtered by genus
ggplot(fishsounds_clean,
       aes(x = Duration,
           y = Bandwidth,
           color = genus))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

#regressions
bandwidthdurationFit_holocentrus <- lm(Bandwidth ~ Duration,
                                   data = fishsounds_clean|>
                                     filter(genus == "Holocentrus"))
summary(bandwidthdurationFit_holocentrus)

bandwidthdurationFit_myripristis <- lm(Bandwidth ~ Duration,
                                   data = fishsounds_clean|>
                                     filter(genus == "Myripristis"))
summary(bandwidthdurationFit_myripristis)

bandwidthdurationFit_neoniphon <- lm(Bandwidth ~ Duration, 
                                 data = fishsounds_clean|>
                                   filter(genus == "Neoniphon"))
summary(bandwidthdurationFit_neoniphon)
# bandwidth pulse duration -----
#total
ggplot(fishsounds_clean,
       aes(x = Pulses,
           y = Bandwidth))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

bandwidthpulseFit <- lm(Bandwidth ~ Pulses, 
                           data = fishsounds_clean)
summary(bandwidthpulseFit)

summary(bandwidthpulseFit)
#filtered by genus
ggplot(fishsounds_clean,
       aes(x = Pulses,
           y = Bandwidth,
           color = genus))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

#regressions
bandwidthpulseFit_holocentrus <- lm(Bandwidth ~ Pulses,
                                       data = fishsounds_clean|>
                                         filter(genus == "Holocentrus"))
summary(bandwidthpulseFit_holocentrus)

bandwidthpulseFit_myripristis <- lm(Bandwidth ~ Pulses,
                                       data = fishsounds_clean|>
                                         filter(genus == "Myripristis"))
summary(bandwidthpulseFit_myripristis)

bandwidthpulseFit_neoniphon <- lm(Bandwidth ~ Pulses, 
                                     data = fishsounds_clean|>
                                       filter(genus == "Neoniphon"))
summary(bandwidthpulseFit_neoniphon)


# linear regression per genus-----
peakfreqregression <- lm(PeakFrequency ~ genus, data = fishsounds_clean)
summary(peakfreqregression)

centerfreqregression <- lm(CenterFrequency ~ genus, data = fishsounds_clean)
summary(centerfreqregression)

durationregression <- lm(Duration ~ genus, data = fishsounds_clean)
summary(durationregression)


bandwidthregression <- lm(Bandwidth ~ genus, data = fishsounds_clean)
summary(bandwidthregression)

pulseregression <- lm(Pulses ~ genus, data = fishsounds_clean)
summary(pulseregression)

# no difference in agonistic sounds ----
#duration
#not significant
oneway.test(Duration ~ behavior_agonistic, 
            data = fishsounds_clean, 
            var.equal = FALSE)

#centerfreq
#not significant
oneway.test(CenterFrequency ~ behavior_agonistic, 
            data = fishsounds_clean, 
            var.equal = FALSE)

#peakfreq
#not significant
oneway.test(PeakFrequency ~ behavior_agonistic, 
            data = fishsounds_clean, 
            var.equal = FALSE)

#bandwidth
#not significant
oneway.test(Bandwidth ~ behavior_agonistic, 
            data = fishsounds_clean, 
            var.equal = FALSE)

#pulses
#not significant
oneway.test(Pulses ~ behavior_agonistic, 
            data = fishsounds_clean, 
            var.equal = FALSE)

#posthoc tests

# anova per genus-----
#duration
#not significant
oneway.test(Duration ~ genus, 
            data = fishsounds_clean, 
            var.equal = FALSE)
#peakfreq
#significant
oneway.test(PeakFrequency ~ genus, 
            data = fishsounds_clean, 
            var.equal = FALSE)
#centerfreq
#significant
oneway.test(CenterFrequency ~ genus, 
            data = fishsounds_clean, 
            var.equal = FALSE)
#bandwidth
#significant
oneway.test(Bandwidth ~ genus, 
            data = fishsounds_clean, 
            var.equal = FALSE)
#pulse count
#not significant
oneway.test(Pulses ~ genus, 
            data = fishsounds_clean, 
            var.equal = FALSE)
#post hoc results 
#centerfreq
pairwise.t.test(fishsounds_clean$CenterFrequency, 
                fishsounds_clean$genus, 
                p.adjust.method = "bonferroni", 
                pool.sd = FALSE)

#peakfreq
pairwise.t.test(fishsounds_clean$PeakFrequency, 
                fishsounds_clean$genus, 
                p.adjust.method = "bonferroni", 
                pool.sd = FALSE)

#bandwidth
pairwise.t.test(fishsounds_clean$Bandwidth, 
                fishsounds_clean$genus, 
                p.adjust.method = "bonferroni", 
                pool.sd = FALSE)


# pca----

#this is the pca step
pr.out <- fishsounds_clean |>
  select(Duration, CenterFrequency, PeakFrequency, Bandwidth, Pulses) |>
  prcomp(, scale = T)

#proportion of variance explained by each PC
summary(pr.out)

#loadings: how much each variable contributes to each PC
pr.out$rotations

head(pr.out)

#this graphs 
biplot(pr.out)

#this plots the first two PCs, color by species
plot(pr.out$x[,1:2], 
     col =as.numeric(factor(fishsounds_clean$genus)), 
     pch = 19)
legend("topright", 
       legend = levels(factor(fishsounds_clean$genus)), 
       col = 1:3, 
       pch = 19)

#correlation 
cor(fishsounds_clean$CenterFrequency, fishsounds_clean$PeakFrequency)

regression <- lm(CenterFrequency ~ PeakFrequency, data = fishsounds_clean)
summary(regression)
#inch resting






