library(tidyverse)

theme_set(theme_gray(base_family="HiraKakuProN-W3"))

df = read_csv("data/output/boot_result.csv")


df %>% 
    ggplot(aes(grp, mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = q025, ymax = q975)) +
    facet_grid(seg_name~cm_id) +
    labs(x = "GRP", y = NULL)


ggsave("figure/grp_AI_by_segment.png", width = 20, height = 16)






