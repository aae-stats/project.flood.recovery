
library(aae.db)
library(dplyr)
library(tidyr)
library(ggplot2)


flood_sample_data <- fetch_query(
  "SELECT id_site, site_name, waterbody, id_project, sdate, yr, rank, scientific_name, common_name, fork_length_mm, length_mm, weight_g
	FROM projects.v_floods_sample_data",
  collect = FALSE
)

#collect the data
flood_sample_data <- flood_sample_data %>% collect()

flood_sample_data$before_after <- ifelse(flood_sample_data$rank == 1, 'after', 'before')

site.flood_impact <- read.csv("site_flood_impact.csv", header = TRUE)
flood_sample_data <- inner_join(flood_sample_data, site.flood_impact, by = c('id_site'))

flood_sample_data$hypoxia_rank <- ifelse(flood_sample_data$condition_rank == 'V', 'L', flood_sample_data$condition_rank)

str(flood_sample_data)
flood_sample_data$before_after <- factor(flood_sample_data$before_after, levels=(c("before", "after")))
flood_sample_data$hypoxia_rank <- factor(flood_sample_data$hypoxia_rank, levels=(c("H", "M", "L")))

flood_sample_data$total_length_mm <- flood_sample_data$length_mm
flood_sample_data$length_mm <- ifelse(!is.na(flood_sample_data$fork_length_mm), flood_sample_data$fork_length_mm, flood_sample_data$length_mm)

flood_sample_data <- flood_sample_data[flood_sample_data$scientific_name %in% sp,]


(lenfreqH <- ggplot(data=flood_sample_data[flood_sample_data$hypoxia_rank == "H",],aes(x=length_mm)) +
    geom_histogram(aes(fill=before_after),binwidth=20,boundary=0,closed="left",
                   color="black",position="stack") +
    # scale_fill_manual(values=c("gray80","gray40")) +
    scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
    scale_x_continuous(name="Total Length (mm)") +
    theme_bw() +
    facet_wrap( ~ scientific_name, scales = "free")
)

(lenfreqM <- ggplot(data=flood_sample_data[flood_sample_data$hypoxia_rank == "M",],aes(x=length_mm)) +
    geom_histogram(aes(fill=before_after),binwidth=20,boundary=0,closed="left",
                   color="black",position="stack") +
    # scale_fill_manual(values=c("gray80","gray40")) +
    scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
    scale_x_continuous(name="Total Length (mm)") +
    theme_bw() +
    facet_wrap( ~ scientific_name, scales = "free")
)

(lenfreqL <- ggplot(data=flood_sample_data[flood_sample_data$hypoxia_rank == "L",],aes(x=length_mm)) +
  geom_histogram(aes(fill=before_after),binwidth=20,boundary=0,closed="left",
                 color="black",position="stack") +
  # scale_fill_manual(values=c("gray80","gray40")) +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw() +
  facet_wrap( ~ scientific_name, scales = "free")
)

#Ruby new plot
#just carp and cod, hypoxia rank= H and M 
#density and histogram 

lenfreqnew1<- flood_sample_data_try|>filter(hypoxia_rank %in% c("H", "M"), scientific_name %in% c("Cyprinus carpio", "Maccullochella peelii"))|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..density.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")

lenfreqnew1

#just with blackfish, maccas, trout cod in low hypoxia 

lenfreqnew2<- flood_sample_data_try|>filter(hypoxia_rank %in% c("L"), scientific_name %in% c("Gadopsis marmoratus", "Maccullochella macquariensis", "Macquaria australasica"))|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=1)+
  # geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")

lenfreqnew2

#trout cod in high and medium hypoxia 
lenfreqnew3<- flood_sample_data_try|>filter(hypoxia_rank %in% c("H", "L"), scientific_name %in% c("Maccullochella macquariensis"))|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=1)+
  # geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")

lenfreqnew3

lenfreqcarp<- flood_sample_data_try|>filter(hypoxia_rank %in% c("H", "M"), 
                                            scientific_name %in% c("Cyprinus carpio"), 
                                            length_mm<150)|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..count.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  #geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  labs(x = "Total Length (mm)", y = "Number of Fish")+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")

lenfreqcarp

lenfreqcod<- flood_sample_data_try|>filter(hypoxia_rank %in% c("H", "M"), 
                                           scientific_name %in% c("Maccullochella peelii"), 
                                           length_mm<=130)|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..count.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  #geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  labs(x = "Total Length (mm)", y = "Number of Fish")+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")+
  annotate(geom="text", x=15, y=12, label="(d)",size=4)


lenfreqcod

lenfreqblackfish<- flood_sample_data_try|>filter(hypoxia_rank %in% c("L"), 
                                                 scientific_name %in% c("Gadopsis marmoratus"), 
                                                 length_mm<80)|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..count.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  #geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  labs(x = "Total Length (mm)", y = "Number of Fish")+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")+
  annotate(geom="text", x=10, y=35, label="(c)",size=4)


lenfreqblackfish

lenfreqtroutcod<- flood_sample_data_try|>filter(hypoxia_rank %in% c("L"), 
                                                scientific_name %in% c("Maccullochella macquariensis"), 
                                                length_mm<130)|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..count.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  #geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")+
  annotate(geom="text", x=13, y=35, label="(b)",size=4)


lenfreqtroutcod

lenfreqmacca<- flood_sample_data_try|>filter(hypoxia_rank %in% c("L"), 
                                             scientific_name %in% c("Macquaria australasica"), 
                                             length_mm<100)|>
  ggplot(aes(x=length_mm))+
  scale_fill_manual(aesthetics = 'fill', values = c("#4FC3F7", "#E57373"), name="Legend", labels=c('Before', 'After'))+
  geom_histogram(aes(y=..count.., fill=before_after), binwidth=20, boundary=0, closed="left",color='black', position="dodge", alpha=0.5)+
  #geom_density(aes(fill=before_after), alpha=0.7, lwd=0.8)+
  theme_bw()+
  facet_wrap(~scientific_name, scales= "free")+
  annotate(geom="text", x=10, y=12, label="(a)",size=4)

lenfreqmacca

bottom<-textbox_grob("Total Length (mm)", vjust = 1, x= 0.9)

left<- textbox_grob("Number of Fish", orientation=c("left-rotated"), y=0.9)


# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
shared_legend <- extract_legend(lenfreqblackfish)

grid.arrange(arrangeGrob(lenfreqmacca+theme(legend.position = "none", strip.text.x = element_text( size=12), axis.title.x = element_blank(), axis.title.y = element_blank()),
                         lenfreqtroutcod+theme(legend.position="none", strip.text.x = element_text( size=12), axis.title.x= element_blank(), axis.title.y = element_blank()),
                         lenfreqblackfish +theme(legend.position = "none", strip.text.x = element_text( size=12), axis.title.x= element_blank(), axis.title.y = element_blank()),
                         lenfreqcod+theme(legend.position="none", strip.text.x = element_text( size=12), axis.title.x= element_blank(), axis.title.y = element_blank()),
                         bottom=bottom, left=left, right=shared_legend,
                         nrow=2, ncol=2))


aaedb_disconnect()
