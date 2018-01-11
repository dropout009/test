library(tidyverse)
library(magrittr)
library(broom)

df = read_csv("data/intermediate/ve_uu_grp.csv")


B = 2000
cm_ids = df$cm_id %>% unique()
C = length(cm_ids)
segments = df$seg_name %>% unique()
S = length(segments)
GRPs = c(500, 750, 1000, 1250, 1500, 1750, 2000)
G = length(GRPs)

df_ai = cross_df(list(segment_name = segments, cm = cm_ids, grp = GRPs)) %>% 
    arrange(segment_name, cm, grp) %>% 
    mutate(
        mean = 0,
        min = 0,
        q0025 = 0,
        q005 = 0,
        med = 0,
        q095 = 0,
        q0975 = 0,
        sd = 0
    )

c=1
g=1
b=1
s=1



for (s in 1) {
    tmp_s = df %>% 
        filter(seg_name == segments[s])
    
    for (c in 1){
        
        tmp_c = tmp_s %>% 
            filter(cm_id == cm_ids[c])
        
        for (g in 1) {
            
            tmp_g = tmp_c %>%
                filter(grp <= GRPs[g])
            
            AIs = numeric(B)
            for (b in 1) {
                
                ids = sample(tmp_g$id, length(tmp_g$id), replace = T)
                
                idx =  map(ids, function(x) which(tmp_g$id == x)) %>% 
                    map(., function(x) if (length(x) == 1) {x} else {sample(x, length(x), replace = T)})
                
                tmp = tmp_g %>%
                    slice(unlist(idx)) %>%
                    group_by(base_ai) %>% 
                    summarise(
                        v = sum(v),
                        e = sum(e)
                    ) %>% 
                    mutate(ai = e / v / base_ai)
                
                AIs[b] = tmp$ai
            }
            
            df_ai$mean
            = Ais %>% mean()
            df_ai$mean = Ais %>% mean()
            df_ai$mean = Ais %>% mean()
            df_ai$mean = Ais %>% mean()
            df_ai$mean = Ais %>% mean()
        }
    }
}