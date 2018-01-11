library(tidyverse)
library(magrittr)
library(broom)

df = read_csv("data/intermediate/ve_uu_grp.csv")

df_boot = df %>% 
    select(seg_name, cm_id, id, v, e, grp, base_ai)

B = 2000

cm_ids = df$cm_id %>% unique()
segments = df$seg_name %>% unique()
GRPs = c(500, 750, 1000, 1250, 1500, 1750, 2000)

df_info = cross_df(list(seg_name = segments, cm_id = cm_ids, grp = GRPs)) %>% 
    arrange(cm_id, grp, seg_name)


df_max = df_boot %>% 
    group_by(seg_name, cm_id) %>% 
    summarise(max_grp = max(grp))

df_info = df_info %>% 
    left_join(df_max, by = c("seg_name", "cm_id")) %>% 
    filter(grp <= max_grp)


D = nrow(df_info)
D = 63

df_ai = tibble(
    se = 0,
    mean = 0,
    min = 0,
    q025 = 0,
    q050 = 0,
    med = 0,
    q950 = 0,
    q975 = 0,
    max = 0
)


for (d in 1:D) {
    
    tmp = df_boot %>% 
        filter(
            seg_name == df_info$seg_name[d],
            cm_id == df_info$cm_id[d],
            grp <= df_info$grp[d]
        )
    
    
    ids = tmp$id
    boot_ids = sample(ids, length(ids) * B, replace = T)
    
    idx =  map(boot_ids, function(x) which(ids == x)) %>% unlist()
    # boot_idx = idx %>% 
    #     map(., function(x) if (length(x) == 1) {x} else {sample(x, length(x), replace = T)}) %>% 
    #     unlist()
    
    boot_group = numeric(length(idx))
    cum_len = 0
    for (b in 1:B) {
        bs = rep(b, idx[((b-1)*length(ids)+1):(b*length(ids))] %>% unlist() %>% length())
        tmp_len = length(bs)
        boot_group[(cum_len + 1):(cum_len + tmp_len)] = bs
        cum_len = cum_len + tmp_len
    }
    
    ai_boot = tmp %>%
        slice(idx) %>%
        mutate(boot_group = boot_group) %>% 
        group_by(boot_group, base_ai) %>% 
        summarise(
            v = sum(v),
            e = sum(e)
        ) %>% 
        mutate(ai = e / v / base_ai) %>% 
        ungroup() %>% 
        summarise(
            se  = sd(ai),
            mean = mean(ai),
            min = min(ai),
            q025 = quantile(ai, 0.025),
            q050 = quantile(ai, 0.05),
            med  = quantile(ai, 0.5),
            q950 = quantile(ai, 0.95),
            q975 = quantile(ai, 0.975),
            max = max(ai)
        )
    
    df_ai = bind_rows(df_ai, ai_boot)
    print(ai_boot)
    print(d)
}

df_ai = df_ai %>% slice(-1)
df_ai

df_results = bind_cols(df_info[1:D, ], df_ai)




df_results %>% 
    write_excel_csv("data/intermediate/boot_result2.csv")


df_uu = df %>% 
    filter(grp <= 500) %>% 
    group_by(seg_name, cm_id) %>% 
    summarise(uu = max(uu, na.rm = T)) %>% 
    mutate(grp = 500) %>% 
    arrange(cm_id)

for (g in c(750, 1000, 1250, 1500, 1750, 2000)) {
    tmp = df %>% 
        filter(grp <= g) %>% 
        group_by(seg_name, cm_id) %>% 
        summarise(uu = max(uu, na.rm = T)) %>% 
        mutate(grp = g) %>% 
        arrange(cm_id)
    
    df_uu = bind_rows(df_uu, tmp)
}



df_results2 = df_results %>% 
    select(-c(q0950, q0975)) %>% 
    left_join(df_uu, by = c("seg_name", "cm_id", "grp")) %>% 
    write_excel_csv("data/intermediate/boot_results_with_uu2.csv")

