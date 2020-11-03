suppressMessages(library(tidyverse))
library(mratios)

mytheme <- function() {
  return(theme_bw() +
           theme(
             panel.background = element_rect(size = 0.9),
             text = element_text(family = "Times", size=10),
             #panel.grid.major = element_blank(),
             #panel.grid.minor = element_blank(),
             panel.grid.major = element_line(colour="gray", size=0.1),
             panel.grid.minor =
               element_line(colour="gray", size=0.1, linetype='dotted'),
             axis.ticks = element_line(size=0.05),
             axis.ticks.length=unit("-0.05", "in"),
             axis.text.y = element_text(margin = margin(r = 5)),
             axis.text.x = element_text(margin = margin(t = 5)),
             legend.key = element_rect(colour=NA),
             legend.spacing = unit(0.001, "in"),
             legend.key.size = unit(0.2, "in"),
             legend.title = element_blank(),
             legend.position = c(0.80, .85),
             legend.background = element_blank()))
}

latency_log_list <- list.files(path = "./data/latency", pattern = "*.csv", full.names = TRUE, recursive = TRUE)

latency_logs <- tibble::enframe(latency_log_list, name = "Id", value = "Filename") %>%
  select(-Id) %>%
  mutate(Benchmark = str_match(Filename, "/data/latency/([:alpha:]*)/")[,2],
         Mode = str_extract(Filename, "decontainerized|vanilla|tracing"))


latency0 <- map(latency_log_list, 
               function(filename) 
                 read_csv(filename,
                          col_names = c("Time", "Latency"),
                          col_types = c(col_integer(), col_integer()))
               %>% mutate(Filename = filename)) %>% 
  bind_rows() %>% 
  inner_join(latency_logs, by = c("Filename")) %>%
  select(-Filename) %>%
  filter(Mode != "decontainerized")

start_times <- latency0 %>% 
  group_by(Benchmark,Mode) %>%
  summarize(StartTime = min(Time), 
            EndTime = max(Time),
            Duration = EndTime - StartTime)

latency <- latency0 %>% 
  inner_join(start_times, by = c("Benchmark", "Mode")) %>%
  mutate(Time = as.integer((Time - StartTime) / (1000 * 1000)),
         Latency = Latency / 1000) %>%
  ungroup()  %>%
  select(-StartTime, -EndTime, -Duration) %>%
  mutate(Mode = fct_recode(Mode, "Containers only" = "vanilla",
                                 "Containers + Trace IR" = "tracing"))

latency_normalized <- latency %>% 
  group_by(Benchmark, Mode, Time) %>%
  summarise(Mean = mean(Latency),
            CI =  1.96 * sd(Latency) / sqrt(length(Latency)),
            MinCI = Mean - CI,
            MaxCI = Mean + CI,
            Max = max(Latency),
            Min = min(Latency)) %>%
  ungroup() %>%
  filter(Time <= 359)


myplot <- function(name) {
  ggplot(latency_normalized %>% filter(Benchmark == name), aes(x=Time,y=Mean,factor=Mode)) +
    scale_y_continuous(trans="log10") + 
    geom_line(aes(y=Max,color=Mode),linetype="dashed",size=0.25) +
    geom_ribbon(aes(ymin=MinCI,ymax=MaxCI,fill=Mode,alpha=1/10)) +
    geom_line(aes(color=Mode)) +
    guides(alpha=FALSE,linetype=FALSE) +
    labs(x = "Time (s)", y = "Latency (ms)") +
    mytheme()
}

ggsave("plots/autocomplete_time_series.pdf", myplot("autocomplete"), width=4, height=3, units="in")
ggsave("plots/helloworld_time_series.pdf", myplot("helloworld"), width=4, height=3, units="in")
ggsave("plots/sanity_time_series.pdf", myplot("sanity"), width=4, height=3, units="in")

steady_latency <- latency %>% 
  filter(Time <= 59 & Time >= 30) %>%
  select(-Time) %>%
  mutate(Mode = fct_recode(Mode, B = "Containers only",
                                 T = "Containers + Trace IR")) %>%
  filter(Benchmark != "echo")

calc_ci <- function(benchmark) {
  df <- steady_latency %>% 
    filter(Benchmark == benchmark) %>% 
    select(-Benchmark,Mode,Latency) %>%
    as.data.frame
  r <- ttestratio(Latency~Mode, data=df)
  return (tibble(Benchmark = benchmark, MinCI = r$conf.int[1], MaxCI = r$conf.int[2],  
                Mean = r$estimate[3]))
}

speedups <- map(unique(steady_latency$Benchmark), calc_ci) %>% bind_rows()

plot <- ggplot(speedups, aes(x=Benchmark,y=Mean)) +
  geom_bar(stat = "identity", fill="#9999ff") +
  geom_errorbar(aes(ymin = MinCI, ymax = MaxCI, width=0.5)) +
  labs(x = "Benchmark", y = "Speedup") +
  scale_y_continuous(breaks = 1:100) +
  theme_bw() +
  theme(
    panel.background = element_rect(size = 0.9),
    text = element_text(family = "Times", size=10),
    panel.grid.major = element_line(colour="gray", size=0.1),
    panel.grid.minor =
      element_line(colour="gray", size=0.1, linetype='dotted'),
    axis.ticks = element_line(size=0.05),
    axis.ticks.length=unit("-0.05", "in"),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)))
ggsave("speedups.pdf", plot, width=4, height=3, units="in")

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
gm_mean(speedups$Mean)