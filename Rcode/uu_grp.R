library(tidyverse)
theme_set(theme_light(base_family = "HiraKakuProN-W3"))



df2 = read_csv("data/intermediate/boot_results_with_uu.csv")
raw = read_csv("data/intermediate/ve_uu_grp.csv")

df %>% 
    ggplot(aes(grp, se)) +
    geom_point() +
    geom_smooth(method = "lm")
    #facet_wrap(~seg_name, scales = "free")


df %>% 
    ggplot(aes(uu, se)) +
    geom_point() +
    geom_smooth(method = "lm")


raw %>%
    filter(seg_name == "All", cm_id == 1200264, grp <= 1000) %>% 
    summarise(ai = sum(e) / sum(v) / max(base_ai))
    
    