# vとgrpからve_uu_grpを作るファイル

library(tidyverse)

v = read_tsv("data/raw/v.tsv") %>% 
    mutate(seg_name = if_else(seg_name == "個人全体", "All", seg_name)) %>% 
    arrange(seg_name, cm_id, start_time)

grp = read_tsv("data/raw/grp.tsv") %>% 
    mutate(seg_name = if_else(seg_name == "個人全体", "All", seg_name)) %>% 
    arrange(seg_name, cm_id, start_time)　%>% 
    filter(seg_name == "All") %>% 
    select(cm_id, start_time, channel_no, grp = cumsum_grp)


v = v %>% 
    unite(id, pid, vid, remove = F) %>%
    mutate(id = as.numeric(as.factor(id))) %>% 
    arrange(seg_name, cm_id, id, start_time) %>% 
    group_by(seg_name, cm_id, id, channel_no) %>% 
    mutate(freq = row_number()) %>% 
    arrange(seg_name, cm_id, start_time)


uu = v %>% 
    filter(freq == 1) %>% 
    group_by(seg_name, cm_id) %>% 
    arrange(start_time) %>% 
    mutate(uu = cumsum(freq)) %>% 
    group_by(seg_name, cm_id, start_time, channel_no) %>% 
    mutate(uu = max(uu)) %>% 
    select(cm_id, start_time, id, pid, vid, seg_name, uu, channel_no) %>% 
    arrange(seg_name, cm_id, start_time) %>% 
    group_by(cm_id, start_time, seg_name, channel_no) %>% 
    summarise(uu = max(uu))
    

seg_name = v %>%
    ungroup() %>%
    distinct(seg_name)

grp = grp %>% 
    mutate(dummy=1) %>% left_join(seg_name %>% mutate(dummy=1), by = "dummy") %>% 
    select(-dummy) 


uu_grp = grp %>% 
    left_join(uu, by = c("cm_id", "start_time", "channel_no", "seg_name")) %>%
    group_by(cm_id, seg_name) %>% 
    arrange(seg_name, cm_id, start_time) %>% 
    mutate(
        uu = if_else(is.na(uu), lag(uu, 1), uu),
        uu = if_else(is.na(uu), lag(uu, 2), uu),
        uu = if_else(is.na(uu), lag(uu, 3), uu),
        uu = if_else(is.na(uu), lag(uu, 4), uu),
        uu = if_else(is.na(uu), lag(uu, 5), uu),
        uu = if_else(is.na(uu), lag(uu, 6), uu),
        uu = if_else(is.na(uu), lag(uu, 7), uu),
        uu = if_else(is.na(uu), lag(uu, 8), uu),
        uu = if_else(is.na(uu), lag(uu, 9), uu),
        uu = if_else(is.na(uu), lag(uu, 10), uu),
        uu = if_else(is.na(uu), lag(uu, 11), uu),
        uu = if_else(is.na(uu), lag(uu, 12), uu),
        uu = if_else(is.na(uu), lag(uu, 13), uu),
        uu = if_else(is.na(uu), lag(uu, 14), uu),
        uu = if_else(is.na(uu), lag(uu, 15), uu),
        uu = if_else(is.na(uu), lag(uu, 16), uu),
        uu = if_else(is.na(uu), lag(uu, 17), uu),
        uu = if_else(is.na(uu), lag(uu, 18), uu),
        uu = if_else(is.na(uu), lag(uu, 19), uu),
        uu = if_else(is.na(uu), lag(uu, 20), uu),
        uu = if_else(is.na(uu), 0, uu)
    )


df = v %>%
    left_join(uu_grp, by = c("cm_id", "start_time", "channel_no", "seg_name")) %>% 
    ungroup()

df_times = df %>% 
    distinct(seg_name, cm_id, start_time, channel_no) %>% 
    group_by(seg_name, cm_id) %>% 
    mutate(times = row_number())

df = df %>% 
    left_join(df_times, by = c("seg_name", "cm_id", "start_time", "channel_no")) %>% 
    select(seg_name, cm_id, start_time, channel_no, times, id, v, e, uu, grp, base_ai)


df %>% 
    write_excel_csv("data/intermediate/ve_uu_grp.csv")
