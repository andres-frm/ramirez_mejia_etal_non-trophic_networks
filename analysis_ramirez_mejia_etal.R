pks <- c('igraph', 'cmdstanr', 'tidyverse', 'readxl', 
         'magrittr', 'animation', 'cowplot', 'moments', 
         'parallel', 'lubridate', 'ggridges', 'boot', 
         'rethinking', 'dagitty', 'ggdag', 'patchwork', 
         'scales')

sapply(pks, FUN = function(x) library(x, character.only = T))

source('functions_mod_diagnostics.r')

extrafont::loadfonts(device="win")

swarm <- read_xlsx('birds_clime/swarm_traits_2022_23.xlsx', sheet = 1, col_names = T)

net <- read_xlsx('birds_clime/2022-23_AntbirdAssociationNetwork_11Nov2023.xlsx', 
                 sheet = 1, col_names = T, na = 'NA')

# ======= data exploration and cleaning =====

net[grep('^(LEGEB01_01AUG)(202[3-9])$', net$observation_ID), ]

net[grep('LEGEB01_01AUG20', net$observation_ID), ]

net$observation_ID[grep('^(LEGEB01_01AUG)(202[3-9])$', net$observation_ID)] <- 'LEGEB01_01AUG2022'

indx <- grep('^(ARPEB02_28JUL)(2023)$', net$observation_ID)

net$observation_ID[indx] <- net$observation_ID[indx + 1]
net$date[indx] <- net$date[indx + 1]

net <- net[!is.na(net$sp1), ]
net <- net[!is.na(net$sp2), ]

net$month <- as.factor(gsub('^([0-9]*)(.)([0-9]*)(.)([0-9]*)$', '\\3', net$date))
net$year <- gsub('^([0-9]*)(.)([0-9]*)(.)([0-9]*)$', '\\1', net$date)
net$day <- gsub('^([0-9]*)(.)([0-9]*)(.)([0-9]*)$', '\\5', net$date)

net <- net[, -grep('^notes$', colnames(net))]

net$tem_int <- net %$% paste(sp1, '_', sp2, sep = '')

net$frec <- 1

all_spp <- unique(c(net$sp1, net$sp2))

net <- split(net, list(net$year, net$month))

net <- net[unlist(lapply(net, function(x) nrow(x) != 0), use.names = F)] 

unlist(lapply(net, nrow), use.names = F)

net <- 
  lapply(net, FUN = 
           function(z) {
             
             x <- z
             spp <- x$tem_int
             sp1 <- unique(c(x$sp1, x$sp2))
             
             t <- sapply(sp1, FUN = 
                           function(i) {
                             indx <- grep(i, spp)
                             
                             vec1 <- vector('character', length = nrow(x))
                             
                             vec1[indx] <- i
                             vec1
                           })
             
             t2 <- 
               apply(t, 2,  
                     function(j){
                       indx <- nchar(j) > 0
                       indx
                     })
             
             t4 <- 
               lapply(1:nrow(t), FUN = 
                        function(k) {
                          s <- t[k, ][t2[k, ]]
                          tibble(sp1 = s[1], sp2 = s[2], 
                                 spp = paste(s[1], s[2], sep = '_'))
                        })
             
             t4 <- do.call('rbind', t4)
             
             x$sp1 <- t4$sp1
             x$sp2 <- t4$sp2
             x$tem_int <- t4$spp
             x
           })

unique(net[[4]][grep(all_spp[2], net[[4]]$tem_int), ]$tem_int)
unique(net[[4]][grep(all_spp[5], net[[4]]$tem_int), ]$tem_int)

net <- do.call('rbind', net)

# month level networks 

net2 <- 
  net |> 
  group_by(date, site, tem_int) |> 
  mutate(int = sum(frec), 
         sp1 = gsub('^(.*)(_)(.*)$', '\\1', tem_int),
         sp2 = gsub('^(.*)(_)(.*)$', '\\3', tem_int)) |> 
  ungroup() |> 
  select(date, site, month, year, day, sp1, sp2, int) |> 
  unique()

net2 <- split(net2, list(net2$month, net2$year))

net2 <- net2[unlist(lapply(net2, function(x) nrow(x) > 0))]

net2 <- 
  lapply(net2, FUN = 
           function(x) {
             edges <- 
               x[, c('sp1', 'sp2', 'int')]
             nodes <- tibble(sp = unique(c(x$sp1, x$sp2)))
             
             list(edges = edges, 
                  nodes = nodes)
           })

names(net2)

jpeg('observed_networks_months.jpeg', height = 20, width = 15, units = 'cm', 
     res = 700)
par(mfrow = c(4, 3), mar = c(0, 0, 1, 0))
for (i in seq_along(net2)) {
  plot_net <- graph_from_data_frame(d = net2[[i]]$edges, 
                                    vertices = net2[[i]]$nodes,
                                    directed = F)
  
  V(plot_net)$size <- 8 
  V(plot_net)$frame.color <- "white" 
  V(plot_net)$color <- "seagreen" 
  V(plot_net)$label <- ""
  E(plot_net)$weight <- E(plot_net)$int
  V(plot_net)$size <- igraph::degree(plot_net) * 0.1
  
  l1 <- layout_with_kk(plot_net)
  
  plot(plot_net, layout = l1, 
       edge.width =  E(plot_net)$weight/4, 
       vertex.size = degree(plot_net) * 0.14)
  text(x= 0.6, y = 0.6, names(net2)[i])
  
  
}
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
dev.off()

# day level networks 

net2 <- 
  net |> 
  group_by(date, observation_ID, site, tem_int) |> 
  mutate(int = sum(frec), 
         sp1 = gsub('^(.*)(_)(.*)$', '\\1', tem_int),
         sp2 = gsub('^(.*)(_)(.*)$', '\\3', tem_int)) |> 
  ungroup() |> 
  select(observation_ID, date, site, month, year, day, sp1, sp2, int) |> 
  unique()

sum(net2$int) == nrow(net)

net2 <- split(net2, list(net2$observation_ID))

net2 <- net2[unlist(lapply(net2, function(x) nrow(x) > 0))]

size_network <- unlist(lapply(net2, FUN = 
                                function(x) {
                                  sp1 <- x$sp1
                                  sp2 <- x$sp2
                                  length(unique(c(sp1, sp2)))
                                }), use.names = F)

net2 <- 
  lapply(net2, FUN = 
           function(x) {
             edges <- 
               x[, c('sp1', 'sp2', 'int')]
             nodes <- tibble(sp = unique(c(x$sp1, x$sp2)))
             
             list(edges = edges, 
                  nodes = nodes)
           })

names(net2)

jpeg('observed_networks_day.jpeg', height = 20, width = 15, units = 'cm', 
     res = 700)
par(mfrow = c(6, 6), mar = c(0, 0, 1, 0))
for (i in 1:36) {
  plot_net <- graph_from_data_frame(d = net2[[i]]$edges, 
                                    vertices = net2[[i]]$nodes,
                                    directed = F)
  
  V(plot_net)$size <- 8 
  V(plot_net)$frame.color <- "white" 
  V(plot_net)$color <- "seagreen" 
  V(plot_net)$label <- ""
  E(plot_net)$weight <- E(plot_net)$int
  V(plot_net)$size <- igraph::degree(plot_net) * 0.05
  
  l1 <- layout_with_kk(plot_net)
  
  plot(plot_net, layout = l1, 
       edge.width =  E(plot_net)$weight/4, 
       vertex.size = degree(plot_net))
  text(x= 0.6, y = 0.6, names(net2)[i])
  
  
}
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
dev.off()


#=======  Analysis  ======

net[] <- lapply(net, function(x) if(is.character(x)) as.factor(x) else(x))

net

levels(net$month)

apply(net, 2, function(x) sum(is.na(x)))

net <- net[!is.na(net$sp2), ]

net <- 
  net |> 
  group_by(observation_ID, site, year, month, day, tem_int) |> 
  mutate(inter = sum(frec), 
         sp1 = gsub('^(.*)(_)(.*)$', '\\1', tem_int),
         sp2 = gsub('^(.*)(_)(.*)$', '\\3', tem_int)) |> 
  ungroup() |> 
  select(observation_ID, date, site, year, month, day, 
         sp1, sp2, inter) |> 
  unique()

sum(net$inter)

net$date <- as.Date(net$date)

plot(density(net$inter))

net[net$observation_ID == levels(net$observation_ID)[2], ] |> 
  print(n = 37)
# ======== Rainfall pattern =========

all_clime <- readRDS('birds_clime/all_clime_per_month.rds')

all_clime <- do.call('rbind', all_clime)

all_clime$month <- floor_date(all_clime$date, 'month')
all_clime$year <- floor_date(all_clime$date, 'year')

all_clime <- 
  all_clime |> 
  group_by(month, station) |> 
  mutate(rainfall = sum(ra)) |> 
  select(station, month, year, rainfall) |> 
  unique()

plot_clime_20_years <- 
  all_clime[all_clime$year >= '2020-01-01',] |> 
  ggplot(aes(month, rainfall, linetype = station)) +
  geom_line(linewidth = 0.25) +
  theme(legend.position = 'none') +
  labs(y = 'Cum. reinfal per month (mm)', x = 'Year') +
  geom_vline(xintercept = c(min(net$date), max(net$date)), 
             linetype = 1, linewidth = 0.4, 
             col = 'tan1') +
  geom_label(x = mean(net$date), y = 700, 
             label =  paste('Sampled', 'period', sep = '\n'), 
             family = 'Times New Roman') +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid = element_blank())
plot_clime_20_years

all_clime$month_fc <- gsub('^(.*)(-)([0-9]*)(-)(.*)$', '\\3', all_clime$month)
all_clime$month_fc <- as.numeric(all_clime$month_fc)

s_monts <- as.numeric(as.character(net$month))

s_monts <- 
  tibble(x = s_monts, 
         id = net$observation_ID) |>
  unique() |> 
  group_by(x) |> 
  mutate(n = length(id)) |> 
  select(x, n) |> 
  unique()

all_clime <- 
  all_clime |> 
  group_by(station, month_fc) |> 
  transmute(mu_rainfall = median(rainfall)) |> 
  unique()

plot_clime_year <- 
  all_clime |> 
  ggplot(aes(month_fc, mu_rainfall, linetype = station)) +
  geom_vline(xintercept = s_monts$x, s_monts,
             linetype = 1, 
             col = 'tan1', linewidth = s_monts$n * 0.07, 
             alpha = 0.5) +
  geom_line() +
  lims(y = c(0, 500)) +
  scale_x_continuous(breaks = 1:12) + 
  labs(y = 'Median rainfall per month (mm)', x = 'Month') +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid = element_blank())

plot_clime_year

# ======== including rainfall predictors ===== 

stations <- readRDS('chosen_stations.rds')
rainfall <- readRDS('rainfall_ant_followers.rds')

unique(stations$NOMBRE) %in% rainfall$station

stations <- stations[, -c(1:3)]

windows <- do.call('cbind', lapply(c(1, seq(5, 30, by = 5)), FUN = 
                                     function(x) {
                                       d <- tibble(v = stations$sampling_date - x)
                                       colnames(d) <- paste('window_', x, sep = '')
                                       d
                                     }))

windows <- as_tibble(cbind(stations, windows))
indx_win <- colnames(windows)[grep('window', colnames(windows))]

cum_rain <- 
  lapply(indx_win, FUN = 
           function(i) {
             
             r <- lapply(1:nrow(windows), FUN =
                           function(x) {
                             d1 <- windows$sampling_date[x]
                             s <- windows$NOMBRE[x]
                             d2 <- windows[, i, drop = T][x]
                             
                             sum(rainfall[rainfall$date < d1 &
                                            rainfall$date >= d2 &
                                            rainfall$station == s, ]$rainfall_mm)
                           })
             
             r <- unlist(r, use.names = F)
             
             tibble(cum_rainfall = r)
             
           })

cum_rain <- lapply(seq_along(indx_win), FUN = 
                     function(x) {
                       w <- c(1, seq(5, 30, by = 5))
                       colnames(cum_rain[[x]]) <- paste('cum_rainfall_W', w[x], sep = '')
                       cum_rain[[x]]
                     })

cum_rain <- as_tibble(do.call('cbind', cum_rain))

windows <- as_tibble(cbind(windows, cum_rain))

windows <- split(windows, windows$observation_ID)

indx <- grep('rainfall', colnames(windows$ARPEB02_28JUL2022))

windows <- 
  lapply(windows, FUN = 
           function(x) {
             vars <- x[, indx]
             vars <- do.call('cbind', 
                             lapply(vars, function(i) tibble(v = median(i))))
             colnames(vars) <- colnames(x)[indx]
             x <- unique(x[, c(5:(indx[1]-1))])
             as_tibble(cbind(x, vars))
           })

windows <- do.call('rbind', windows)

stations <- split(stations, stations$observation_ID)

net <- full_join(net, windows[, -c(3, grep('window', colnames(windows)))], 
                 by = c('observation_ID', 'site'))

apply(net, 2, function(x) sum(is.na(x)))

net <- net[!is.na(net$inter), ]

sum(net$inter)

# ===== models ======

# ===== distance matrices ====

dist_mat <- readRDS('birds_clime/distance_matrices.rds')

dist_mat$month_matrix

# ===== index for network analysis ====

indx_networks <- net

indx_networks$tem_int <- indx_networks %$% paste(sp1, sp2, sep = '_')

indx_networks <- split(indx_networks, indx_networks$observation_ID)

inter <- unlist(lapply(indx_networks, nrow), use.names = F)

nodes <- unlist(lapply(indx_networks, FUN = 
                         function(x) {
                           sp <- unique(c(x$sp1, x$sp2))
                           length(sp)
                         }), use.names = F)

(cutoff_inter <- quantile(inter, seq(0, 1, by = 0.1)))
(cut_off_nodes <- quantile(nodes, seq(0, 1, by = 0.1)))

mean(((inter >= cutoff_inter[2]) + 
        (nodes >= cut_off_nodes[2])) == 2)

indx_networks <- 
  tibble(obs_ID = names(indx_networks), 
         indx = ((inter >= cutoff_inter[2]) + 
                   (nodes >= cut_off_nodes[2])) == 2)

# ====== Network plotting =====

est_nets <- net[, 1:grep('inter', colnames(net))]

est_nets$site <- as.factor(est_nets$site)

est_nets <- split(est_nets, est_nets$observation_ID)

unlist(lapply(est_nets, function(x) nrow(x) != 0), use.names = F)

names(est_nets)

#est_nets <- est_nets[indx_networks$indx]
est_nets$ARPEB02_28JUL2022
plot(density(unlist(lapply(est_nets, nrow), use.names = F)))

t1 <- Sys.time()
plotting_nets <- 
  mclapply(est_nets, mc.cores = 4, FUN = 
             function(x) {
               
               edges_obs <- 
                 x[, c('sp1', 'sp2', 'inter')]
               
               nodes_obs <- tibble(sp = unique(c(x$sp1, x$sp2)))
               
               obs_nets <- graph_from_data_frame(d = edges_obs, 
                                                 vertices = nodes_obs,
                                                 directed = F)
               
               V(obs_nets)$frame.color <- "white" 
               V(obs_nets)$color <- "seagreen" 
               V(obs_nets)$label <- ""
               
               obs_nets
               
             })
Sys.time() - t1

par(mfrow = c(1, 1), mar = c(0, 0, 1, 0))

plot(plotting_nets$ARPEB02_29JUL2022, 
     layout = layout_with_kk(plotting_nets$ARPEB02_29JUL2022), 
     edge.width =  E(plotting_nets$ARPEB02_29JUL2022)$inter/5, 
     vertex.size = degree(plotting_nets$ARPEB02_29JUL2022) * 0.8)
text(x = 0.2, y = 0.95, label = paste('Observed', 
                                      names(plotting_nets)[1], sep = '\n'))

par(mfrow = c(1, 1), mar = c(0, 0, 1, 0))

dev.off()

# layout(mat = matrix(c(1,1,2,2,3,3,4, 4, 
#                       5,5,6,6,7,7,8,8),
#                     nrow = 2,
#                     byrow = TRUE))

t1 <- Sys.time()
saveGIF(
  {
    
    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
    
    for (i in seq_along(plotting_nets)) {
      
      plot(plotting_nets[[i]], 
           layout = layout_with_kk(plotting_nets[[i]]), 
           edge.width =  E(plotting_nets[[i]])$inter/5, 
           vertex.size = degree(plotting_nets[[i]]) * 1)
      text(x = 0.2, y = 0.95, label = paste('Observed', 
                                            names(plotting_nets)[i], sep = '\n'))
      
    }
  }, movie.name = 'month_posterior_gaussianP.gif', interval = 0.1)
Sys.time() - t1

names(plotting_nets)

# ===== Network metrics =====

plot(cluster_walktrap(plotting_nets$ARPEB03_16AUG2022, 
                      weights = E(plotting_nets$ARPEB03_16AUG2022)$inter, 
                      steps = 1), 
     plotting_nets$ARPEB03_16AUG2022)

plot(cluster_infomap(plotting_nets$ARPEB03_16AUG2022, 
                     e.weights = E(plotting_nets$ARPEB03_16AUG2022)$inter,
                     v.weights = degree(plotting_nets$ARPEB03_16AUG2022)), 
     plotting_nets$ARPEB03_16AUG2022)

plot(cluster_edge_betweenness(plotting_nets$ARPEB03_16AUG2022, 
                              weights = E(plotting_nets$ARPEB03_16AUG2022)$inter), 
     plotting_nets$ARPEB03_16AUG2022)

plot(cluster_louvain(plotting_nets$ARPEB03_16AUG2022, 
                     weights = E(plotting_nets$ARPEB03_16AUG2022)$inter), 
     plotting_nets$ARPEB03_16AUG2022)


network_metrics <- 
  lapply(seq_along(plotting_nets), 
         function(i) {
           
           obs_net <- plotting_nets[[i]]
           
           net_size <- length(degree(obs_net))
           #network_density <- edge_density(obs_net)
           normalized_degree <- degree(obs_net)
           normalized_degree <- 
             normalized_degree / (length(normalized_degree)-1) # calcula lo mismo que la funcion igraph::edge_density
           skewness_indx <- skewness(normalized_degree)
           normalized_degree <- mean(normalized_degree)
           clustering <- transitivity(obs_net)
           
           dat <- 
             tibble(net_size = net_size,
                    #net_density = network_density, 
                    norm_degree = normalized_degree, 
                    skewness = skewness_indx, 
                    clustering = clustering, 
                    type = 'Observed')
           
           
           dat$obs_ID <- est_nets[[i]]$observation_ID[1]
           dat$date <- est_nets[[i]]$date[1]
           dat$year <- est_nets[[i]]$year[1]
           dat$month <- est_nets[[i]]$month[1]
           dat$site <- est_nets[[i]]$site[1]
           
           dat
           
         })

length(network_metrics)

names(network_metrics) <- names(est_nets)

# prueba skewness

# tenemos NaN en está métrica porque todas las especies interactuan 
# con todas las especies posibles en la red

network_metrics_S_OBS <- 
  do.call('rbind', 
          lapply(network_metrics, FUN = 
                   function(x) {
                     x <- x[x$type == 'Observed', ]
                     x[, -c(2:4)]
                   }))

network_metrics <- network_metrics[indx_networks$indx]

indx_nan <- unlist(lapply(network_metrics, FUN = 
                            function(x) {
                              sum(apply(x, 2, function(i) sum(is.na(i)))) != 0
                            }), 
                   use.names = F)

mean(indx_nan)
which(indx_nan == T); length(which(indx_nan == T))


# ==== plotting metrics ======

plot_grid(do.call('rbind', network_metrics) |> 
            ggplot(aes(date, skewness, color = obs_ID, 
                       size = type, alpha = type)) +
            geom_point() +
            #scale_color_manual(values = c('black', 'red')) +
            scale_size_manual(values = c(0.25, 1.5)) +
            scale_alpha_manual(values = c(0.5, 4)) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Skewness', x = 'Date') +
            theme(
              legend.position = 'none',
              axis.title.x = element_blank()
            ), 
          do.call('rbind', network_metrics) |> 
            ggplot(aes(date, norm_degree, color = obs_ID, 
                       size = type, alpha = type)) +
            geom_point() +
            #scale_color_manual(values = c('black', 'red')) +
            scale_size_manual(values = c(0.25, 1.5)) +
            scale_alpha_manual(values = c(0.5, 4)) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Mean normalized degree') +
            theme(
              legend.position = 'none',
              axis.title.x = element_blank()
            ), 
          do.call('rbind', network_metrics) |> 
            ggplot(aes(date, clustering, color = obs_ID, 
                       size = type, alpha = type)) +
            geom_point() +
            #scale_color_manual(values = c('black', 'red')) +
            scale_size_manual(values = c(0.25, 1.5)) +
            scale_alpha_manual(values = c(0.5, 4)) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Clustering', x = 'Date') +
            theme(
              legend.position = 'none'
            ), 
          ncol = 1)

# ====== modelling (daily network)  ====

colnames(network_metrics_S_OBS)[3] <- 'observation_ID'

network_metrics_S_OBS <- 
  full_join(network_metrics_S_OBS, 
            unique(net[, c(1:2, grep('^cum', colnames(net)))]), 
            by = c('observation_ID', 'date'))

net <- split(net, net$observation_ID)
net_2 <- net[indx_networks$indx]

sum(names(net_2) == names(network_metrics)) == length(network_metrics)

network_metrics <- 
  lapply(names(network_metrics), FUN = 
           function(x) {
             metrics <- network_metrics[[x]]
             rain <- net_2[[x]]
             rain <- rain[1, grep('^cum', colnames(rain))]
             as_tibble(cbind(metrics, rain))
           })

names(network_metrics) <- names(net_2)

network_metrics_obs <- network_metrics

network_metrics_obs <- do.call('rbind', network_metrics_obs)

indx_nan <- unlist(lapply(network_metrics, FUN = 
                            function(x) {
                              sum(apply(x, 2, function(i) sum(is.na(i)))) != 0
                            }), 
                   use.names = F) # estamos ok!!!

mean(indx_nan)
which(indx_nan == T); length(which(indx_nan == T))

# network_metrics has NaNs in skewness 

# network_metrics <- do.call('rbind', network_metrics)
# network_metrics <- split(network_metrics, network_metrics$date)

# ===== network modelling === 

# ====== including swarm data =====

# modelling the contrast of both sampling techniques and use it 
# as a sampling error to add random values to one of the variables

swarm <- read_xlsx('birds_clime/swarm_traits_2022_23b.xlsx', sheet = 1, 
                   col_names = T, na = 'NA')

swarm$swarm_ID <- toupper(swarm$swarm_ID)
colnames(swarm)

apply(swarm, 2, function(x) sum(is.na(x)))

swarm %$% plot(log(swarm_biomass), log(swarm_area_inital), 
               col = ifelse(year == 2022, 'red', 'cyan4'))

swarm %$% points(log(swarm_biomass), log(swarm_area_final), 
                 col = ifelse(year == 2022, 'blue', 'yellow'))

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
swarm[swarm$year == 2023,] %$% 
  plot(swarm_area_final, swarm_area_inital, 
       col = ifelse(year == 2022, 'red', 'cyan4'), 
       main = '2023')

swarm[swarm$year == 2022,] %$% 
  plot(swarm_area_final, swarm_area_inital, 
       col = ifelse(year == 2022, 'red', 'cyan4'),
       main = '2022')

swarm[swarm$year == 2023,] %$% 
  plot(swarm_area_final, swarm_biomass, 
       col = ifelse(year == 2022, 'red', 'cyan4'), 
       main = '2023')

swarm[swarm$year == 2022,] %$% 
  plot(swarm_area_final, swarm_biomass, 
       col = ifelse(year == 2022, 'red', 'cyan4'), 
       main = '2022')

swarm[swarm$year == 2023,] %$% 
  plot(swarm_area_inital, swarm_biomass, 
       col = ifelse(year == 2022, 'red', 'cyan4'), 
       main = '2023')

swarm[swarm$year == 2022,] %$% 
  plot(swarm_area_inital, swarm_biomass, 
       col = ifelse(year == 2022, 'red', 'cyan4'), 
       main = '2022')

par(mfrow = c(1, 1))

# ===== estimating measurement error 

swarm_calibration <- read_xlsx('swarm_area_method_calibration.xlsx', sheet = 1, col_names = T)

swarm_calibration1 <- swarm_calibration[, -3]
swarm_calibration2 <- swarm_calibration[, -2]

colnames(swarm_calibration1)[2] <- 'area'
swarm_calibration1$year <- 1

colnames(swarm_calibration2)[2] <- 'area'
swarm_calibration2$year <- 2

swarm_calibration <- rbind(swarm_calibration2, swarm_calibration1)

swarm_calibration |> print(n = 40)

swarm_calibration <- lapply(swarm_calibration, function(x) x)

swarm_calibration$N <- length(swarm_calibration$trial_id)
swarm_calibration$N_year <- 2
swarm_calibration$N_trials <- 10

plot(density(swarm_calibration$area[swarm_calibration$year==1]))
lines(density(swarm_calibration$area[swarm_calibration$year==2]), col = 'red')
lines(density(rlnorm(1e3, log(6), 1)), col = 'green')
lines(density(rgamma(1e3, shape = exp(3)/5, rate = 1/5)), col = 'blue')

cat(file = 'calibration_swarm.stan', 
    "
    data{
      int N;
      int N_year;
      int N_trials;
      vector[N] area;
      array[N] int trial_id;
      array[N] int year;
    }
    
    parameters{
      matrix[N_trials, N_year] z_alpha;
      cholesky_factor_corr[N_trials] rho;
      vector<lower = 0>[N_trials] sigma_alpha;
      vector[N_trials] alpha_bar;
      real<lower = 0> sigma;
    }
    
    transformed parameters{
      vector[N_year] alpha;
      matrix[N_year, N_trials] M_alpha;
      M_alpha = (diag_pre_multiply(sigma_alpha, rho) * z_alpha)';
      alpha = alpha_bar[1] + M_alpha[, 1];
    }
    
    model{
      vector[N] mu;
      to_vector(z_alpha) ~ normal(0, 1);
      rho ~ lkj_corr_cholesky(2);
      alpha_bar ~ normal(2, 1);
      sigma_alpha ~ exponential(1);
      sigma ~ exponential(1);
    
      for (i in 1:N) {
        mu[i] = exp(alpha[year[i]]);
      }
      
    area ~ gamma(mu/sigma, 1/sigma);
    
    }
    
    generated quantities{
      vector[N] mu;
      array[N] real ppcheck;
    
      for (i in 1:N) {
        mu[i] = exp(alpha[year[i]]);
      }
    
      ppcheck = gamma_rng(mu/sigma, 1/sigma);
    }
    "
)


file <- paste(getwd(), '/calibration_swarm.stan', sep = '')

swarm_fit_calibration <- cmdstan_model(file, compile = T)

mod_calibration <- 
  swarm_fit_calibration$sample(
    data = swarm_calibration, 
    chains = 3,
    parallel_chains = 3, 
    iter_sampling = 4000, 
    iter_warmup = 500,
    thin = 5, 
    seed = 123,
    refresh = 500
  )


sum_mod_calibration <- mod_calibration$summary()

mod_calibration$summary(c('alpha', 'sigma'))

ppcheck_calibration <- mod_calibration$draws('ppcheck', format = 'matrix')

dim(ppcheck_calibration)

plot(density(swarm_calibration$area), main = '', xlab = 'Swarm area', 
     ylim = c(0, 0.03))
for (i in 1:100) lines(density(ppcheck_calibration[i, ]), lwd = 0.1)

mod_diagnostics(mod_calibration, sum_mod_calibration)

post_calibration <- mod_calibration$draws('alpha', format = 'df')

post_calibration <- as_tibble(apply(post_calibration[, 1:2], 2, exp))

colnames(post_calibration) <- c('gps', 'trigonometric')

calibration_error <- post_calibration$trigonometric - post_calibration$gps

mean(calibration_error < 0)

par(mfrow = c(1, 2))
plot(density(post_calibration$gps), lwd = 2, col = 'purple', 
     main = '', xlim = c(7, 50), xlab = 'swarm area')
lines(density(post_calibration$trigonometric), 
      lwd = 2, col = 'lightblue')
legend(x = 25, y = 0.11, legend = c('gps', 'trigonometry'), col = c('purple', 'lightblue'), 
       lty = 1, lwd = 2, border = 'white', box.lwd = 0, 
       title.cex = 1)
plot(density(calibration_error), lwd = 2, col = 'tomato', 
     xlab = 'Measurement error', main = '')
par(mfrow = c(1, 1))

#======== including random measurement errors to the variables

swarm <- split(swarm, swarm$year)

n_ini <- sum(!is.na(swarm$`2022`$swarm_area_inital))
n_fin <- sum(!is.na(swarm$`2022`$swarm_area_final))

set.seed(5)
error_ini <- sample(calibration_error, size = n_ini, replace = F)

set.seed(6)
error_fin <- sample(calibration_error, size = n_fin, replace = F)

indx_ini <- which(!is.na(swarm$`2022`$swarm_area_inital))
indx_fin <- which(!is.na(swarm$`2022`$swarm_area_final))

swarm$`2022`$swarm_area_inital[indx_ini] <- 
  error_ini + swarm$`2022`$swarm_area_inital[indx_ini]

swarm$`2022`$swarm_area_final[indx_fin] <- 
  error_fin + swarm$`2022`$swarm_area_final[indx_fin]


swarm <- as_tibble(do.call('rbind', swarm))

#  ========== 

swarm[!is.na(swarm$swarm_area_final), ]

sum(is.na(swarm[swarm$year == 2023, ]$swarm_area_inital))
sum(is.na(swarm[swarm$year == 2023, ]$swarm_area_final))

nrow(swarm)

swarm |> 
  ggplot(aes(swarm_biomass, fill = as.factor(year))) +
  geom_density(alpha = 0.5)

apply(swarm, 2, function(x) sum(is.na(x)))

swarm$date <- gsub("^(.*)(_)(.*)$", '\\3', swarm$swarm_ID)
swarm$date <- as.Date(swarm$date, '%d%b%Y')

colnames(network_metrics_S_OBS)[3] <- 'obs_ID'

colnames(swarm)[1] <- 'obs_ID'

indx_swarm_S_obs <- 
  sapply(swarm$obs_ID, FUN = 
           function(i) {
             sum(i == network_metrics_S_OBS$obs_ID) > 0
           }, USE.NAMES = F)

full_join(network_metrics_S_OBS, 
          swarm[indx_swarm_S_obs, c("obs_ID", "swarm_biomass", 
                                    "swarm_area_final", 'swarm_area_inital')], 
          by = 'obs_ID') |> 
  apply(2, function(x) sum(is.na(x)))

network_metrics_S_OBS <- 
  full_join(network_metrics_S_OBS, 
            swarm[indx_swarm_S_obs, c("obs_ID", "swarm_biomass", 
                                      "swarm_area_final", 'swarm_area_inital')], 
            by = 'obs_ID') 

indx_swarm <- 
  sapply(swarm$obs_ID, FUN = 
           function(i) {
             sum(i == network_metrics_obs$obs_ID) > 0
           }, USE.NAMES = F)

swarm$obs_ID[indx_swarm] %in% network_metrics_obs$obs_ID

swarm <- swarm[indx_swarm, ]

swarm[swarm$obs_ID == swarm$obs_ID[1], ]

which(swarm$obs_ID[3] == names(network_metrics))

apply(swarm, 2, function(x) sum(is.na(x)))

network_metrics_obs2 <- 
  full_join(network_metrics_obs, 
            swarm[, c("obs_ID", "swarm_biomass", 
                      "swarm_area_final", 'swarm_area_inital')], 
            by = c('obs_ID'))

apply(network_metrics_obs2, 2, function(x) sum(is.na(x)))

indx_networks[indx_networks$indx, ]

# network_metrics_obs2 <- na.omit(network_metrics_obs2)

network_metrics2 <- do.call('rbind', network_metrics)

network_metrics2 <- full_join(network_metrics2, 
                              network_metrics_obs2[, c("obs_ID", 
                                                       "swarm_biomass", 
                                                       "swarm_area_final", 
                                                       'swarm_area_inital')], 
                              by = 'obs_ID')

# network_metrics2 <- network_metrics2[!is.na(network_metrics2$swarm_biomass), ]

network_metrics2 <- split(network_metrics2, network_metrics2$obs_ID)

apply(network_metrics2$ARPEB03_17AUG2022, 2,
      FUN = function(x) sum(is.na(x)))

sum(unlist(lapply(network_metrics2, FUN = 
                    function(x) length(unique(x$swarm_biomass)) > 1)))

nas_swarm <- 
  unlist(lapply(network_metrics2, FUN = 
                  function(x) {
                    sum(apply(x, 2, function(i) sum(is.na(x)))) > 0
                  }), use.names = F)

(1 - mean(nas_swarm)) * 100 # ~50% percent of compleate cases

quantile(unlist(lapply(network_metrics2[!nas_swarm], nrow), use.names = F), 
         c(0, 0.5, 1))

subset_network_metrics <- network_metrics2

subset_network_metrics <- do.call('rbind', subset_network_metrics)

subset_network_metrics$date_fct <- as.factor(subset_network_metrics$date)

apply(subset_network_metrics, 2, function(i) sum(is.na(i)))

sum(levels(subset_network_metrics$date_fct) %in%
      colnames(dist_mat$day_matrix))

network_metrics_S_OBS$date_fct <- 
  as.factor(network_metrics_S_OBS$date)

indx_date2 <- 
  colnames(dist_mat$day_matrix) %in%
  levels(network_metrics_S_OBS$date_fct)

indx_date <- colnames(dist_mat$day_matrix) %in% 
  levels(subset_network_metrics$date_fct)

subset_day_matrix <- dist_mat$day_matrix[indx_date, indx_date]

dist_mat$day_matrix <- dist_mat$day_matrix[indx_date2, indx_date2]

levels(subset_network_metrics$date_fct) == colnames(subset_day_matrix)

plot_grid(subset_network_metrics |> 
            ggplot(aes(swarm_biomass, skewness, color = obs_ID)) +
            geom_point(size = 1, alpha = 0.5) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Skewness', x = 'Swarm biomass') +
            theme(
              legend.position = 'none', 
              axis.title.x = element_blank()
            ),
          subset_network_metrics |> 
            ggplot(aes(swarm_biomass, norm_degree, color = obs_ID)) +
            geom_point(size = 1, alpha = 0.5) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Normalized degree', x = 'Swarm biomass') +
            theme(
              legend.position = 'none', 
              axis.title.x = element_blank()
            ),
          subset_network_metrics |> 
            ggplot(aes(swarm_biomass, clustering, color = obs_ID)) +
            geom_point(size = 1, alpha = 0.5) +
            facet_wrap(~year, scales = 'free_x') +
            labs(y = 'Clustering', x = 'Swarm biomass') +
            theme(
              legend.position = 'none'
            ),
          ncol = 1)

#======= Fitting causal model =======

# ====== modeling the observed =======

indx_dist_day2 <- 
  as.vector(sapply(colnames(dist_mat$day_matrix), FUN = 
                     function(x) sum(x == unique(network_metrics_obs2$date)) != 0))

dim(dist_mat$day_matrix[indx_dist_day2, indx_dist_day2])

network_metrics_obs2$date_fct <- as.factor(network_metrics_obs2$date)
network_metrics_obs2 <- network_metrics_obs2[, -c(5, 7)]
network_metrics_obs2$obs_ID <- as.factor(network_metrics_obs2$obs_ID)

network_metrics_INDX <- 
  list(obs_all = network_metrics_obs2, 
       obs_net_size = network_metrics_S_OBS)

# ==== obs three network metrics

network_metrics_obs2 <- network_metrics_INDX$obs_all

network_metrics_obs2$season <- 
  ifelse(as.numeric(as.character(network_metrics_obs2$month)) <= 4, 1, 2)

network_metrics_obs2 %$% 
  aggregate(net_size ~ season + site, FUN = length) ### ok it could work 

network_metrics_obs2$month <- as.numeric(as.character(network_metrics_obs2$month))

network_metrics_obs2 <- lapply(network_metrics_obs2, FUN =
                                 function(i) {
                                   if (is.factor(i)) as.numeric(i)
                                   else i
                                 })

cols <- grep('^cum_', names(network_metrics_obs2))

for (i in cols) network_metrics_obs2[[i]] <- 
  as.vector(scale(network_metrics_obs2[[i]]))

# network_metrics_obs2$swarm_biomass <- as.vector(scale(network_metrics_obs2$swarm_biomass))
network_metrics_obs2$dist_day <- subset_day_matrix
network_metrics_obs2$N <- length(network_metrics_obs2$date_fct)
network_metrics_obs2$N_day <- max(network_metrics_obs2$date_fct)
network_metrics_obs2$N_month <- max(network_metrics_obs2$month)
network_metrics_obs2$N_season <- max(network_metrics_obs2$season)
network_metrics_obs2$N_site <- max(network_metrics_obs2$site)
network_metrics_obs2$N_dim_day <- dim(subset_day_matrix)[1]

# network_metrics_obs2$N_naSkewness <- 
#   sum(is.na(network_metrics_obs2$skewness))
# 
# network_metrics_obs2$Skew_missindx <- 
#   which(is.na(network_metrics_obs2$skewness))

network_metrics_obs2$N_naSwarmBio <- 
  sum(is.na(network_metrics_obs2$swarm_biomass))

network_metrics_obs2$SwarmB_missindx <- 
  which(is.na(network_metrics_obs2$swarm_biomass))

network_metrics_obs2$N_naSwarmINI <- 
  sum(is.na(network_metrics_obs2$swarm_area_inital))

network_metrics_obs2$SwarmINI_missindx <- 
  which(is.na(network_metrics_obs2$swarm_area_inital))

network_metrics_obs2$N_naSwarmFIN <- 
  sum(is.na(network_metrics_obs2$swarm_area_final))

network_metrics_obs2$SwarmFIN_missindx <- 
  which(is.na(network_metrics_obs2$swarm_area_final))

unlist(lapply(network_metrics_obs2, function(x) mean(is.na(x))))

names(network_metrics_obs2)[grep('date_fct', names(network_metrics_obs2))] <- 'day'

network_metrics_obs2$swarm_biomass <- 
  sapply(network_metrics_obs2$swarm_biomass, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })

network_metrics_obs2$swarm_area_final <- 
  sapply(network_metrics_obs2$swarm_area_final, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })

network_metrics_obs2$swarm_area_inital <- 
  sapply(network_metrics_obs2$swarm_area_inital, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })

plot(density(na.omit(network_metrics_obs2$swarm_area_inital)))
plot(density(na.omit(network_metrics_obs2$swarm_area_final)))
plot(density(na.omit(network_metrics_obs2$swarm_biomass)))
plot(density(network_metrics_obs2$cum_rainfall_W1))
plot(density(network_metrics_obs2$cum_rainfall_W5))

mean(network_metrics_obs$norm_degree)
sd(network_metrics_obs$norm_degree)

mean(network_metrics_obs$skewness, na.rm = T)
sd(network_metrics_obs$skewness, na.rm = T)

mean(network_metrics_obs$clustering, na.rm = T)
sd(network_metrics_obs$clustering, na.rm = T)

# ===== obs Net Size

network_metrics_S_OBS <- network_metrics_INDX$obs_net_size
network_metrics_S_OBS <- network_metrics_S_OBS[, -c(2, 4)]
network_metrics_S_OBS$obs_ID <- as.factor(network_metrics_S_OBS$obs_ID)

network_metrics_S_OBS$season <- 
  ifelse(as.numeric(as.character(network_metrics_S_OBS$month)) <= 4, 1, 2)

network_metrics_S_OBS %$% 
  aggregate(net_size ~ season + site, FUN = length) ### ok it could work 

network_metrics_S_OBS$month <- 
  as.numeric(as.character(network_metrics_S_OBS$month))

network_metrics_S_OBS <- lapply(network_metrics_S_OBS, FUN =
                                  function(i) {
                                    if (is.factor(i)) as.numeric(i)
                                    else i
                                  })

cols <- grep('^cum_', names(network_metrics_S_OBS))

for (i in cols) network_metrics_S_OBS[[i]] <- 
  as.vector(scale(network_metrics_S_OBS[[i]]))

summary(network_metrics_S_OBS$swarm_area_final)
sd(network_metrics_S_OBS$swarm_area_final, na.rm = T)
summary(network_metrics_S_OBS$swarm_area_inital)
sd(network_metrics_S_OBS$swarm_area_inital, na.rm = T)
summary(network_metrics_S_OBS$swarm_biomass)
sd(network_metrics_S_OBS$swarm_biomass, na.rm = T)

network_metrics_S_OBS$swarm_biomass <- 
  sapply(network_metrics_S_OBS$swarm_biomass, FUN = 
           function(x) {
             if (is.na(x)) NA
             else log(x)
           })

network_metrics_S_OBS$swarm_area_final <- 
  sapply(network_metrics_S_OBS$swarm_area_final, FUN = 
           function(x) {
             if (is.na(x)) NA
             else log(x)
           })

network_metrics_S_OBS$swarm_area_inital <- 
  sapply(network_metrics_S_OBS$swarm_area_inital, FUN = 
           function(x) {
             if (is.na(x)) NA
             else log(x)
           })
plot(density(na.omit(network_metrics_S_OBS$swarm_area_inital)))
plot(density(na.omit(network_metrics_S_OBS$swarm_area_final)))
plot(density(na.omit(network_metrics_S_OBS$swarm_biomass)))
plot(density(network_metrics_S_OBS$cum_rainfall_W1))
plot(density(network_metrics_S_OBS$cum_rainfall_W5))

# network_metrics_S_OBS$swarm_biomass <- as.vector(scale(network_metrics_S_OBS$swarm_biomass))
network_metrics_S_OBS$dist_day <- dist_mat$day_matrix
network_metrics_S_OBS$N <- length(network_metrics_S_OBS$date_fct)
network_metrics_S_OBS$N_day <- max(network_metrics_S_OBS$date_fct)
network_metrics_S_OBS$N_month <- max(network_metrics_S_OBS$month)
network_metrics_S_OBS$N_season <- max(network_metrics_S_OBS$season)
network_metrics_S_OBS$N_site <- max(network_metrics_S_OBS$site)
network_metrics_S_OBS$N_dim_day <- dim(dist_mat$day_matrix)[1]

network_metrics_S_OBS$N_naSwarmBio <- 
  sum(is.na(network_metrics_S_OBS$swarm_biomass))

network_metrics_S_OBS$SwarmB_missindx <- 
  which(is.na(network_metrics_S_OBS$swarm_biomass))

network_metrics_S_OBS$N_naSwarmINI <- 
  sum(is.na(network_metrics_S_OBS$swarm_area_inital))

network_metrics_S_OBS$SwarmINI_missindx <- 
  which(is.na(network_metrics_S_OBS$swarm_area_inital))

network_metrics_S_OBS$N_naSwarmFIN <- 
  sum(is.na(network_metrics_S_OBS$swarm_area_final))

network_metrics_S_OBS$SwarmFIN_missindx <- 
  which(is.na(network_metrics_S_OBS$swarm_area_final))

unlist(lapply(network_metrics_S_OBS, function(x) mean(is.na(x))))

names(network_metrics_S_OBS)[grep('date_fct', names(network_metrics_S_OBS))] <- 'day'

network_metrics_S_OBS$swarm_biomass <- 
  sapply(network_metrics_S_OBS$swarm_biomass, FUN = 
           function(i) {
             if (is.na(i)) 0
             else i
           })

network_metrics_S_OBS$swarm_area_final <- 
  sapply(network_metrics_S_OBS$swarm_area_final, FUN = 
           function(i) {
             if (is.na(i)) 0
             else i
           })

network_metrics_S_OBS$swarm_area_inital <- 
  sapply(network_metrics_S_OBS$swarm_area_inital, FUN = 
           function(i) {
             if (is.na(i)) 0
             else i
           })

names(network_metrics_S_OBS)

mean(network_metrics_S_OBS$net_size)
sd(network_metrics_S_OBS$net_size)

plot(density(network_metrics_S_OBS$cum_rainfall_W20))

names(network_metrics_S_OBS)

network_metrics_INDX$obs_net_size

# ====== Network size (obs) W1 ====

cat(file = 'w1_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W1;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W1[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    ")


file <- paste(getwd(), '/w1_net_size.stan', sep ='')

fit_net_sizeW1 <- cmdstan_model(file, compile = T)

mod_net_sizeW1_obs <- 
  fit_net_sizeW1$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_net_sizeW1_obs$save_object('causal_mod_net_sizeW1.rds')

mod_net_sizeW1_obs <- readRDS('causal_mod_net_sizeW1.rds')
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW1_obs$summary(paste('theta_e4[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW1_obs$summary(paste('theta_e2[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW1_obs$summary(paste('theta_e3[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130)
# 

png('w1_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(paste('theta_e4[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))
mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(paste('theta_e3[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))

mod_diagnostics(mod_net_sizeW1_obs, 
                mod_net_sizeW1_obs$summary(paste('theta_e2[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))
dev.off()



summary_net_sizeW1 <- mod_net_sizeW1_obs$summary(c('beta_rainW2_e1', 
                                                   'beta_rainW1_e2',
                                                   'beta_rainW1_e3',
                                                   'beta_rainW1_e4',
                                                   'beta_sawrA_e3',
                                                   'beta_sawrA_e4',
                                                   'beta_swarmB_e4', 
                                                   'a_e1', 'a_e2', 
                                                   'a_e3', 'a_e4',
                                                   'sigma_e1', 'sigma_e2', 
                                                   'sigma_e3', 'sigma_e4', 
                                                   'swarm_area',
                                                   'SBiomass_merge',
                                                   'lp__'))

summary_net_sizeW1 |> print(n = 500)


mod_diagnostics(mod_net_sizeW1_obs, summary_net_sizeW1)


post_net_sizeW1_obs <- mod_net_sizeW1_obs$draws(c('beta_rainW2_e1', 
                                                  'beta_rainW1_e2',
                                                  'beta_rainW1_e3',
                                                  'beta_rainW1_e4',
                                                  'beta_sawrA_e3',
                                                  'beta_sawrA_e4',
                                                  'beta_swarmB_e4', 
                                                  'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                  'sigma_e1', 'sigma_e2', 
                                                  'sigma_e3', 'sigma_e4',
                                                  'swarm_area',
                                                  'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_net_sizeW1_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW1_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW1_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW1_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW1_obs[, grep('swarm_area', colnames(post_net_sizeW1_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW1_obs[, grep('SBiomass_merge', colnames(post_net_sizeW1_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W1), 
     main = '', xlab = 'Cum rainfall W1')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW1_obs)

post_beta_net_sizeW1 <- gather(post_net_sizeW1_obs[, grep('beta', colnames(post_net_sizeW1_obs))])

post_beta_net_sizeW1$key <- as.factor(post_beta_net_sizeW1$key)


post_beta_net_sizeW1 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


levels(network_metrics_INDX$obs_net_size$site)




# ====== Network size (obs) W5 ====

cat(file = 'w5_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W5[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] +
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w5_net_size.stan', sep ='')

fit_net_sizeW5 <- cmdstan_model(file, compile = T)

mod_net_sizeW5_obs <- 
  fit_net_sizeW5$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_net_sizeW5_obs$save_object('causal_mod_net_sizeW5.rds')

mod_net_sizeW5_obs <- readRDS('causal_mod_net_sizeW5.rds')
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW5_obs$summary(paste('theta_e4[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW5_obs$summary(paste('theta_e2[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW5_obs$summary(paste('theta_e3[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130)


png('W5_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(paste('theta_e4[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))
mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(paste('theta_e3[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))

mod_diagnostics(mod_net_sizeW5_obs, 
                mod_net_sizeW5_obs$summary(paste('theta_e2[', 
                                                 1:network_metrics_S_OBS$N_day, ']', 
                                                 sep = '')))
dev.off()



summary_net_sizeW5 <- mod_net_sizeW5_obs$summary(c('beta_rainW2_e1', 
                                                   'beta_rainW1_e2',
                                                   'beta_rainW1_e3',
                                                   'beta_rainW1_e4',
                                                   'beta_sawrA_e3',
                                                   'beta_sawrA_e4',
                                                   'beta_swarmB_e4', 
                                                   'a_e1', 'a_e2', 
                                                   'a_e3', 'a_e4',
                                                   'sigma_e1', 'sigma_e2', 
                                                   'sigma_e3', 'sigma_e4', 
                                                   'swarm_area',
                                                   'SBiomass_merge',
                                                   'lp__'))

summary_net_sizeW5 |> print(n = 15)


mod_diagnostics(mod_net_sizeW5_obs, summary_net_sizeW5)


post_net_sizeW5_obs <- mod_net_sizeW5_obs$draws(c('beta_rainW2_e1', 
                                                  'beta_rainW1_e2',
                                                  'beta_rainW1_e3',
                                                  'beta_rainW1_e4',
                                                  'beta_sawrA_e3',
                                                  'beta_sawrA_e4',
                                                  'beta_swarmB_e4', 
                                                  'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                  'sigma_e1', 'sigma_e2', 
                                                  'sigma_e3', 'sigma_e4',
                                                  'swarm_area',
                                                  'SBiomass_merge'), format = 'df')

ppcheck_swarmA <- mod_net_sizeW5_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW5_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW5_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW5_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW5_obs[, grep('swarm_area', colnames(post_net_sizeW5_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW5_obs[, grep('SBiomass_merge', colnames(post_net_sizeW5_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W5), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW5_obs)


post_beta_net_sizeW5 <- gather(post_net_sizeW5_obs[, grep('beta', colnames(post_net_sizeW5_obs))])

post_beta_net_sizeW5$key <- as.factor(post_beta_net_sizeW5$key)

post_beta_net_sizeW5 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)





# ====== Network size (obs) W10 ====

cat(file = 'w10_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W10[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w10_net_size.stan', sep ='')

fit_net_sizeW10 <- cmdstan_model(file, compile = T)

mod_net_sizeW10_obs <- 
  fit_net_sizeW10$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_net_sizeW10_obs$save_object('causal_mod_net_sizeW10.rds')

mod_net_sizeW10_obs <- readRDS('causal_mod_net_sizeW10.rds')
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW10_obs$summary(paste('theta_e4[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW10_obs$summary(paste('theta_e2[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW10_obs$summary(paste('theta_e3[', 
#                                  1:network_metrics_S_OBS$N_day, ']', 
#                                  sep = '')) |> print(n = 130)


png('W10_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_net_sizeW10_obs, 
                mod_net_sizeW10_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_net_sizeW10 <- mod_net_sizeW10_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_net_sizeW10 |> print(n = 500)


mod_diagnostics(mod_net_sizeW10_obs, summary_net_sizeW10)


post_net_sizeW10_obs <- mod_net_sizeW10_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'), format = 'df')

ppcheck_swarmA <- mod_net_sizeW10_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW10_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW10_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW10_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW10_obs[, grep('swarm_area', colnames(post_net_sizeW10_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW10_obs[, grep('SBiomass_merge', colnames(post_net_sizeW10_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W10), 
     main = '', xlab = 'Cum rainfall W10')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW10_obs)



post_beta_net_sizeW10 <- gather(post_net_sizeW10_obs[, grep('beta', colnames(post_net_sizeW10_obs))])

post_beta_net_sizeW10$key <- as.factor(post_beta_net_sizeW10$key)




post_beta_net_sizeW10 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)










# ====== Network size (obs) W15 ====

# cat(file = 'w15_net_sizeLATENT.stan', 
#     "
#     functions {
#     
#       vector scale(vector x) {
#         int N = dims(x)[1];
#         real mu = mean(x);
#         real sigma = sd(x);
#         vector[N] scaled_v;
#     
#         for (i in 1:N) {
#           scaled_v[i] = (x[i] - mu) / sigma;
#         }
#         return scaled_v;
#       }  
#     
#       vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
#                 int N = dims(x_obs)[1];
#                 int N_miss = dims(x_miss)[1];
#                 vector[N] merge;
#                 merge = x_obs;
#                 for (i in 1:N_miss) {
#                     merge[miss_indxex[i]] = x_miss[i];
#                 }
#             return merge;
#           }
#     
#     
#       matrix cov_GPL2(matrix x,
#                       real eta,
#                       real rho,
#                       real delta) {
#                       
#                       int N = dims(x)[1];
#                       matrix[N, N] K;
#     
#                       for (i in 1:(N-1)) {
#                         K[i, i] = eta + delta;
#                         for (j in (i + 1):N) {
#                           K[i, j] = eta * exp(-rho * square(x[i, j]));
#                           K[j, i] = K[i, j];
#                         }
#                       }
#                       K[N, N] = eta + delta;
#                       return K;
#                       }
#     }
#     
#     data {
#       int N;
#       int N_day;
#       int N_month;
#       int N_year;
#       int N_site;
#       int N_dim_day;
#       int N_naSwarmBio;
#       int N_naSwarmINI;
#       int N_naSwarmFIN;
#       array[N_naSwarmBio] int SwarmB_missindx;
#       array[N_naSwarmINI] int SwarmINI_missindx;
#       array[N_naSwarmFIN] int SwarmFIN_missindx;
#       array[N] int net_size;
#       array[N] int day;
#       array[N] int month;
#       array[N] int year;
#       array[N] int site;
#       vector[N] swarm_biomass;
#       vector[N] swarm_area_inital;
#       vector[N] swarm_area_final;
#       array[N] int cum_rainfall_W20;
#       array[N] int cum_rainfall_W15;
#       matrix[N_dim_day, N_dim_day] dist_day;
#     }
#     
#     parameters {
#       vector[N] U;
#       real<lower = 0> k;
#     
#       /////////////
#       ///////////// imputed variables
#       vector[N_naSwarmBio] SBiomass_impute;
#       real mu_sb;
#       real<lower = 0> sigma_sb;
#       vector[N_naSwarmINI] SwarmINI_impute;
#       real mu_sbINI;
#       real<lower = 0> sigma_sbINI;
#       vector[N_naSwarmFIN] SwarmFIN_impute;
#       real mu_sbFIN;
#       real<lower = 0> sigma_sbFIN;
#     
#       //////////////
#       ///////////// rain_i+n -> rain_i
#       real a_e1;
#       real beta_rainW2_e1;
#       real<lower = 0> sigma_e1;
#     
#       /////////////////////////
#       ///////////////////////// rain_i -> swarm
#       real a_e2;
#       real beta_rainW1_e2;
#       real<lower = 0> sigma_e2;
#     
#       vector[N_day] z_theta_e2;
#       real<lower = 0> eta_theta_e2;
#       real<lower = 0> rho_theta_e2;
#     
#       matrix[N_year, N_month] z_tau_e2;
#       real mu_tau_e2;
#       real<lower = 0> sigma_tau_e2;
#     
#       matrix[N_site, N_month] z_psi_e2;
#       real mu_psi_e2;
#       real<lower = 0> sigma_psi_e2;
#       
#       ////////////////////
#     
#       /////////////////////////
#       ///////////////////////// rain_i -> swarm -> insects biomass
#       ///////////////////////// rain_i ->  insects biomass
#       real a_e3;
#       real beta_rainW1_e3;
#       real beta_sawrA_e3;
#       real<lower = 0> sigma_e3;
#     
#       vector[N_day] z_theta_e3;
#       real<lower = 0> eta_theta_e3;
#       real<lower = 0> rho_theta_e3;
#     
#       matrix[N_year, N_month] z_tau_e3;
#       real mu_tau_e3;
#       real<lower = 0> sigma_tau_e3;
#     
#       matrix[N_site, N_month] z_psi_e3;
#       real mu_psi_e3;
#       real<lower = 0> sigma_psi_e3;
#       
#       ////////////////////
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass -> network
#       ///////////////////////// rain_i -> network
#       ///////////////////////// swarm -> network
#       real a_e4;
#       real beta_rainW1_e4;
#       real beta_sawrA_e4;
#       real beta_swarmB_e4;
#       real<lower = 0> sigma_e4;
#     
#       vector[N_day] z_theta_e4;
#       real<lower = 0> eta_theta_e4;
#       real<lower = 0> rho_theta_e4;
#     
#       matrix[N_year, N_month] z_tau_e4;
#       real mu_tau_e4;
#       real<lower = 0> sigma_tau_e4;
#     
#       matrix[N_site, N_month] z_psi_e4;
#       real mu_psi_e4;
#       real<lower = 0> sigma_psi_e4;
#       
#       ////////////////////
#     
#       
#     }
#     
#     transformed parameters{
#       
#       ///////////////////////
#       ///////////////////////// imputed variables
#       vector[N] swarm_area;
#       vector[N] SBiomass_merge;
#       SBiomass_merge = merge_missing(SwarmB_missindx, 
#                                      to_vector(swarm_biomass), 
#                                      SBiomass_impute);
#       vector[N] swarmINI_merge;
#       swarmINI_merge = merge_missing(SwarmINI_missindx, 
#                                      to_vector(swarm_area_inital), 
#                                      SwarmINI_impute);
#       vector[N] swarmFIN_merge;
#       swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
#                                      to_vector(swarm_area_final), 
#                                      SwarmFIN_impute);
#       swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
#       // swarm_area = scale(swarm_area);
#       // SBiomass_merge = scale(SBiomass_merge);
#     
#       ///////////////////
#       ///////////////////////// rain_i -> swarm
#       
#       vector[N_day] theta_e2;
#       matrix[N_dim_day, N_dim_day] sigma_theta_e2;
#       matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
#       sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
#       L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
#       theta_e2 = L_sigma_theta_e2 * z_theta_e2;
#     
#       matrix[N_year, N_month] tau_e2;
#       matrix[N_site, N_month] psi_e2;
#       tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
#       psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
#       
#       ///////////////////
#       ///////////////////////// rain_i -> swarm -> insects biomass
#       ///////////////////////// rain_i ->  insects biomass
#       
#       vector[N_day] theta_e3;
#       matrix[N_dim_day, N_dim_day] sigma_theta_e3;
#       matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
#       sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
#       L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
#       theta_e3 = L_sigma_theta_e3 * z_theta_e3;
#     
#       matrix[N_year, N_month] tau_e3;
#       matrix[N_site, N_month] psi_e3;
#       tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
#       psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
#     
#       ////////////////////
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass -> network
#       ///////////////////////// rain_i -> network
#       ///////////////////////// swarm -> network
#       
#       vector[N_day] theta_e4;
#       matrix[N_dim_day, N_dim_day] sigma_theta_e4;
#       matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
#       sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
#       L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
#       theta_e4 = L_sigma_theta_e4 * z_theta_e4;
#     
#       matrix[N_year, N_month] tau_e4;
#       matrix[N_site, N_month] psi_e4;
#       tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
#       psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
#     
#       ///////////////////
#     }
#     
#     model {
#       U ~ normal(0, 0.5);
#       k ~ exponential(1);
#       
#       ///////////////////////
#       /////////////////////// imputed variables
#       mu_sb ~ normal(3.5, 1);
#       sigma_sb ~ exponential(1);
#       SBiomass_merge ~ normal(mu_sb, sigma_sb);
#       mu_sbINI ~ normal(3.5, 1);
#       sigma_sbINI ~ exponential(1);
#       swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
#       mu_sbFIN ~ normal(3.5, 1);
#       sigma_sbFIN ~ exponential(1);
#       swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
#     
#       /////////////////////////
#       ///////////////////// rain_i+n -> rain i
#       vector[N] mu_e1;
#       a_e1 ~ normal(0, 1);
#       beta_rainW2_e1 ~ normal(0, 0.5);
#       sigma_e1 ~ exponential(1);
#       
#       ///////////////////
#       /////////////////// rain_i -> swarm
#       a_e2 ~ normal(3, 1);
#       sigma_e2 ~ exponential(1);
#       beta_rainW1_e2 ~ normal(0, 0.25);
#     
#       z_theta_e2 ~ normal(0, 0.5);     
#       eta_theta_e2 ~ exponential(3);   
#       rho_theta_e2 ~ exponential(0.5); 
#       
#       to_vector(z_tau_e2) ~ normal(0, 0.5);
#       mu_tau_e2 ~ normal(0, 0.5);
#       sigma_tau_e2 ~ exponential(1);
#     
#       to_vector(z_psi_e2) ~ normal(0, 0.5);
#       mu_psi_e2 ~ normal(0, 0.5);
#       sigma_psi_e2 ~ exponential(1);
#     
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass
#       ///////////////////////// rain_i ->  insects biomass
#       a_e3 ~ normal(2.5, 1);
#       sigma_e3 ~ exponential(1);
#       beta_rainW1_e3 ~ normal(0, 0.5);
#       beta_sawrA_e3 ~ normal(0, 3);
#     
#       z_theta_e3 ~ normal(0, 1);
#       eta_theta_e3 ~ exponential(3);
#       rho_theta_e3 ~ exponential(0.5);
#       
#       to_vector(z_tau_e3) ~ normal(0, 0.5);
#       mu_tau_e3 ~ normal(0, 0.5);
#       sigma_tau_e3 ~ exponential(1);
#     
#       to_vector(z_psi_e3) ~ normal(0, 0.5);
#       mu_psi_e3 ~ normal(0, 0.5);
#       sigma_psi_e3 ~ exponential(1);
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass -> network
#       ///////////////////////// rain_i -> network
#       ///////////////////////// swarm -> network
#       a_e4 ~ normal(2.5, 2);
#       sigma_e4 ~ exponential(1);
#       beta_rainW1_e4 ~ normal(0, 0.5);
#       beta_sawrA_e4 ~ normal(0, 0.5);
#       beta_swarmB_e4 ~ normal(0, 0.5);
#     
#       z_theta_e4 ~ normal(0, 1);
#       eta_theta_e4 ~ exponential(3);
#       rho_theta_e4 ~ exponential(0.5);
#       
#       to_vector(z_tau_e4) ~ normal(0, 0.5);
#       mu_tau_e4 ~ normal(0, 0.5);
#       sigma_tau_e4 ~ exponential(1);
#     
#       to_vector(z_psi_e4) ~ normal(0, 0.5);
#       mu_psi_e4 ~ normal(0, 0.5);
#       sigma_psi_e4 ~ exponential(1);
#       
#     
#       /////////////////////////
#       ///////////////////// rain_i+n -> rain i
#       for (i in 1:N) {
#         cum_rainfall_W15[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i], 
#                                      sigma_e1);
#       }
#     
#       ///////////////////
#       /////////////////// rain_i -> swarm
#       for (i in 1:N) {
#         swarm_area[i] ~ student_t(2,
#                                   a_e2 + beta_rainW1_e2*cum_rainfall_W15[i] + 
#                                   theta_e2[day[i]] + k*U[i] +
#                                   tau_e2[year[i], month[i]] + 
#                                   psi_e2[site[i], month[i]], 
#                                   sigma_e2);
#       }
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass
#       ///////////////////////// rain_i ->  insects biomass
#       for (i in 1:N) {
#         SBiomass_merge[i] ~ student_t(2,
#                                      a_e3 + beta_rainW1_e3*cum_rainfall_W15[i] +
#                                      beta_sawrA_e3*swarm_area[i] + k*U[i] +
#                                      theta_e3[day[i]] + tau_e3[year[i], month[i]] + 
#                                      psi_e3[site[i], month[i]], 
#                                      sigma_e3);
#       }
#     
#     
#       ///////////////////////// rain_i -> swarm -> insects biomass -> network
#       ///////////////////////// rain_i -> network
#       ///////////////////////// swarm -> network
#       for (i in 1:N) {
#         net_size[i] ~ neg_binomial_2(exp(a_e4 + beta_rainW1_e4*cum_rainfall_W15[i] +
#                                      beta_sawrA_e4*swarm_area[i] +
#                                      beta_swarmB_e4*SBiomass_merge[i] +
#                                      theta_e4[day[i]] + tau_e4[year[i], month[i]] + 
#                                      psi_e4[site[i], month[i]]),
#                                      sigma_e4);
#       }
#     }
#     
#     generated quantities {
#       vector[N] mu_e1;
#       vector[N] mu_e2;
#       vector[N] mu_e3;
#       vector[N] mu_e4;
#       array[N] real ppcheck_e1;
#       array[N] real ppcheck_e2;
#       array[N] real ppcheck_e3;
#       array[N] int ppcheck_e4;
#       
#       for (i in 1:N) {
#         mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i]; 
#       }
#     
#       ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
#     
#       for (i in 1:N) {
#         mu_e2[i] = a_e2 + beta_rainW1_e2*cum_rainfall_W15[i] + k*U[i] +
#                 theta_e2[day[i]] + tau_e2[year[i], month[i]] + 
#                 psi_e2[site[i], month[i]];
#       }
#       
#       ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
#     
#     
#       for (i in 1:N) {
#         mu_e3[i] = a_e3 + beta_rainW1_e3*cum_rainfall_W15[i] +
#                    beta_sawrA_e3*swarm_area[i] + k*U[i] +
#                    theta_e3[day[i]] + tau_e3[year[i], month[i]] + 
#                    psi_e3[site[i], month[i]];
#       }
#       
#       ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
#     
#       for (i in 1:N) {
#         mu_e4[i] = a_e4 + beta_rainW1_e4*cum_rainfall_W15[i] +
#                    beta_sawrA_e4*swarm_area[i] +
#                    beta_swarmB_e4*SBiomass_merge[i] +
#                    theta_e4[day[i]] + tau_e4[year[i], month[i]] + 
#                    psi_e4[site[i], month[i]];
#         mu_e4[i] = exp(mu_e4[i]);
#       }
#     
#       ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
#     
#     }
#     
#     ")
# 
# file <- paste(getwd(), '/w15_net_sizeLATENT.stan', sep ='')
# 
# fit_net_sizeW15LATENT <- cmdstan_model(file, compile = T)
# 
# mod_net_sizeW15_obsLATENT <- 
#   fit_net_sizeW15LATENT$sample(
#     data = network_metrics_S_OBS, 
#     chains = 4, 
#     parallel_chains = 4, 
#     iter_sampling = 1e4, 
#     iter_warmup = 50, 
#     thin = 30, 
#     seed = 123
#   ) # it takes 12h
# 
# mod_net_sizeW15_obsLATENT$save_object('causal_mod_net_sizeW15LATENT.rds')
# mod_net_sizeW15_obsLATENT <- readRDS('causal_mod_net_sizeW15LATENT.rds')
# 
# mod_net_sizeW15_obsLATENT$summary('U') |> print(n = 200)
# mod_net_sizeW15_obsLATENT$summary('k') 
# 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obsLATENT$summary(paste('theta_e4[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW15_obsLATENT$summary(paste('theta_e2[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW15_obsLATENT$summary(paste('theta_e3[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130)
# 
# 
# png('W15_net_size_diagnosticsLATENT.png', width = 10, height = 15, 
#     res = 500, units = 'cm')
# par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('psi_e2[', 
#                                network_metrics_S_OBS$site,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = '')))) 
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('psi_e3[', 
#                                network_metrics_S_OBS$site,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = ''))))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('psi_e4[', 
#                                network_metrics_S_OBS$site,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = ''))))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('tau_e4[', 
#                                network_metrics_S_OBS$year,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = ''))))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('tau_e2[', 
#                                network_metrics_S_OBS$year,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = ''))))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(
#                   unique(paste('tau_e3[', 
#                                network_metrics_S_OBS$year,
#                                ',',
#                                network_metrics_S_OBS$month, ']', 
#                                sep = ''))))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(paste('theta_e4[', 
#                                                   1:network_metrics_S_OBS$N_day, ']', 
#                                                   sep = '')))
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(paste('theta_e3[', 
#                                                   1:network_metrics_S_OBS$N_day, ']', 
#                                                   sep = '')))
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, 
#                 mod_net_sizeW15_obsLATENT$summary(paste('theta_e2[', 
#                                                   1:network_metrics_S_OBS$N_day, ']', 
#                                                   sep = '')))
# dev.off()
# 
# 
# 
# summary_net_sizeW15LATENT <- mod_net_sizeW15_obsLATENT$summary(c('beta_rainW2_e1', 
#                                                      'beta_rainW1_e2',
#                                                      'beta_rainW1_e3',
#                                                      'beta_rainW1_e4',
#                                                      'beta_sawrA_e3',
#                                                      'beta_sawrA_e4',
#                                                      'beta_swarmB_e4',
#                                                      'k',
#                                                      'a_e1', 'a_e2', 
#                                                      'a_e3', 'a_e4',
#                                                      'sigma_e1', 'sigma_e2', 
#                                                      'sigma_e3', 'sigma_e4', 
#                                                      'swarm_area',
#                                                      'SBiomass_merge',
#                                                      'lp__'))
# 
# summary_net_sizeW15LATENT |> print(n = 20)
# 
# 
# mod_diagnostics(mod_net_sizeW15_obsLATENT, summary_net_sizeW15LATENT)
# 
# 
# post_net_sizeW15_obsLATENT <- mod_net_sizeW15_obsLATENT$draws(c('beta_rainW2_e1', 
#                                                     'beta_rainW1_e2',
#                                                     'beta_rainW1_e3',
#                                                     'beta_rainW1_e4',
#                                                     'beta_sawrA_e3',
#                                                     'beta_sawrA_e4',
#                                                     'beta_swarmB_e4', 
#                                                     'a_e1', 'a_e2', 'a_e3', 'a_e4',
#                                                     'sigma_e1', 'sigma_e2', 
#                                                     'sigma_e3', 'sigma_e4',
#                                                     'swarm_area',
#                                                     'SBiomass_merge'), format = 'df')
# 
# ppcheck_swarmALATEN <- mod_net_sizeW15_obsLATENT$draws('ppcheck_e2', format = 'matrix')
# ppcheck_swarmBLATEN <- mod_net_sizeW15_obsLATENT$draws('ppcheck_e3', format = 'matrix')
# ppcheck_net_sizeLATEN <- mod_net_sizeW15_obsLATENT$draws('ppcheck_e4', format = 'matrix')
# ppcheck_R1LATEN <- mod_net_sizeW15_obsLATENT$draws('ppcheck_e1', format = 'matrix')
# 
# plot(density(network_metrics_S_OBS$cum_rainfall_W15), 
#      main = '', xlab = 'Cum rainfall W15')
# for(i in 1:100) lines(density(ppcheck_R1LATEN[i, ]), lwd = 0.1)
# 
# plot(density(swarm_area), main = '', xlab = 'Swarm area')
# for(i in 1:100) lines(density(ppcheck_swarmALATEN[i, ]), lwd = 0.1)
# 
# plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
# for(i in 1:100) lines(density(ppcheck_swarmBLATEN[i, ]), lwd = 0.1)
# 
# plot(density(network_metrics_S_OBS$net_size), 
#      main = '', xlab = 'Net size')
# for(i in 1:100) lines(density(ppcheck_net_sizeLATEN[i, ]), lwd = 0.1)


#===========================3

cat(file = 'w15_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
    
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W15[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i], 
                                     sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] + 
                                  theta_e2[day[i]] + 
                                  tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] + 
                   theta_e2[day[i]] + 
                   tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w15_net_size.stan', sep ='')

fit_net_sizeW15 <- cmdstan_model(file, compile = T)

mod_net_sizeW15_obs <- 
  fit_net_sizeW15$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 10e3, 
    iter_warmup = 500, 
    thin = 30, 
    seed = 123
  )

mod_net_sizeW15_obs$save_object('causal_mod_net_sizeW15.rds')
mod_net_sizeW15_obs <- readRDS('causal_mod_net_sizeW15.rds')
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW15_obs$summary(paste('theta_e4[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW15_obs$summary(paste('theta_e2[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW15_obs$summary(paste('theta_e3[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130)


png('W15_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_net_sizeW15_obs, 
                mod_net_sizeW15_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_net_sizeW15 <- mod_net_sizeW15_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4',
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_net_sizeW15 |> print(n = 500)


mod_diagnostics(mod_net_sizeW15_obs, summary_net_sizeW15)


post_net_sizeW15_obs <- mod_net_sizeW15_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'), format = 'df')

ppcheck_swarmA <- mod_net_sizeW15_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW15_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW15_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW15_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW15_obs[, grep('swarm_area', colnames(post_net_sizeW15_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW15_obs[, grep('SBiomass_merge', colnames(post_net_sizeW15_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W15), 
     main = '', xlab = 'Cum rainfall W15')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW15_obs)



post_beta_net_sizeW15 <- gather(post_net_sizeW15_obs[, grep('beta', colnames(post_net_sizeW15_obs))])

post_beta_net_sizeW15$key <- as.factor(post_beta_net_sizeW15$key)

post_beta_net_sizeW15 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)




# ====== Network size (obs) W20 ====

cat(file = 'w20_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W20[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i], 
                                     sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w20_net_size.stan', sep ='')

fit_net_sizeW20 <- cmdstan_model(file, compile = T)

mod_net_sizeW20_obs <- 
  fit_net_sizeW20$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_net_sizeW20_obs$save_object('causal_mod_net_sizeW20.rds')

mod_net_sizeW20_obs <- readRDS('causal_mod_net_sizeW20.rds')

# mod_net_sizeW20_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW20_obs$summary(paste('theta_e4[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW20_obs$summary(paste('theta_e2[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW20_obs$summary(paste('theta_e3[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130)


png('W20_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_net_sizeW20_obs, 
                mod_net_sizeW20_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_net_sizeW20 <- mod_net_sizeW20_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_net_sizeW20 |> print(n = 500)


mod_diagnostics(mod_net_sizeW20_obs, summary_net_sizeW20)


post_net_sizeW20_obs <- mod_net_sizeW20_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'), format = 'df')

ppcheck_swarmA <- mod_net_sizeW20_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW20_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW20_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW20_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW20_obs[, grep('swarm_area', colnames(post_net_sizeW20_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW20_obs[, grep('SBiomass_merge', colnames(post_net_sizeW20_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W20), 
     main = '', xlab = 'Cum rainfall W20')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW20_obs)


post_beta_net_sizeW20 <- gather(post_net_sizeW20_obs[, grep('beta', colnames(post_net_sizeW20_obs))])

post_beta_net_sizeW20$key <- as.factor(post_beta_net_sizeW20$key)



post_beta_net_sizeW20 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)









# ====== Network size (obs) W25 ====

cat(file = 'w25_net_size.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      array[N] int net_size;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W30;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(2.5, 2);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 1);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W25[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i], 
                                     sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
        net_size[i] ~ neg_binomial_2(exp(a_e4[site[i], season[i]] + 
                                     beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                                     beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                                     beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                                     theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                                     psi_e4[site[i], month[i]]),
                                     sigma_e4);
      }
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] int ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
        mu_e4[i] = a_e4[site[i], season[i]] + 
                   beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                   beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                   beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                   theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                   psi_e4[site[i], month[i]];
        mu_e4[i] = exp(mu_e4[i]);
      }
    
      ppcheck_e4 = neg_binomial_2_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w25_net_size.stan', sep ='')

fit_net_sizeW25 <- cmdstan_model(file, compile = T)

mod_net_sizeW25_obs <- 
  fit_net_sizeW25$sample(
    data = network_metrics_S_OBS, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_net_sizeW25_obs$save_object('causal_mod_net_sizeW25.rds')

mod_net_sizeW25_obs <- readRDS('causal_mod_net_sizeW25.rds')
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_S_OBS$site,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_S_OBS$year,
#                ',',
#                network_metrics_S_OBS$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_net_sizeW25_obs$summary(paste('theta_e4[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 200) 
# 
# mod_net_sizeW25_obs$summary(paste('theta_e2[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_net_sizeW25_obs$summary(paste('theta_e3[', 
#                                   1:network_metrics_S_OBS$N_day, ']', 
#                                   sep = '')) |> print(n = 130)


png('W25_net_size_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_S_OBS$site,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_S_OBS$year,
                               ',',
                               network_metrics_S_OBS$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_net_sizeW25_obs, 
                mod_net_sizeW25_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_S_OBS$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_net_sizeW25 <- mod_net_sizeW25_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_net_sizeW25 |> print(n = 500)


mod_diagnostics(mod_net_sizeW25_obs, summary_net_sizeW25)


post_net_sizeW25_obs <- mod_net_sizeW25_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'), format = 'df')

ppcheck_swarmA <- mod_net_sizeW25_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_net_sizeW25_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_net_size <- mod_net_sizeW25_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_net_sizeW25_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_net_sizeW25_obs[, grep('swarm_area', colnames(post_net_sizeW25_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_net_sizeW25_obs[, grep('SBiomass_merge', colnames(post_net_sizeW25_obs))], 
        2, mean)

plot(density(network_metrics_S_OBS$cum_rainfall_W25), 
     main = '', xlab = 'Cum rainfall W25')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_S_OBS$net_size), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_net_size[i, ]), lwd = 0.1)

dim(post_net_sizeW25_obs)



post_beta_net_sizeW25 <- gather(post_net_sizeW25_obs[, grep('beta', colnames(post_net_sizeW25_obs))])

post_beta_net_sizeW25$key <- as.factor(post_beta_net_sizeW25$key)


post_beta_net_sizeW25 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== plotting and counterfactual net size ====

# ====== forest plot and conditional effects ====

t <- paste('W', c(1, seq(5, 25, by = 5)), sep = '')

plot_beta_net_size <- vector('list', length(t))

for (i in seq_along(t)) {
  cadena <- paste('^(.*)beta_net(.*)', t[i], '$', sep = '')
  cadena2 <- paste('post_net_size', t[i], '_obs', sep = '')
  df <- get(ls()[grepl(cadena, ls())])
  temp <- get(ls()[grepl(cadena2, ls())])
  
  temp <- temp[, grep('^beta', colnames(temp))]
  
  temp <- apply(temp, 2, function(x) mean(x > 0))
  
  temp <- tibble(p = temp, key = names(temp))
  
  df <- 
    df |> 
    group_by(key) |> 
    transmute(li = quantile(value, 0.025), 
              ls = quantile(value, 0.975), 
              mu = mean(value), 
              window = t[i]) |> 
    unique()
  
  n <- c(1, seq(5, 25, by = 5))[i]
  
  df <- df[!grepl(n + 4, df$key), ]
  
  df <- full_join(df, temp, by = 'key')
  
  plot_beta_net_size[[i]] <- df
  print(i)
}

plot_beta_net_size <- do.call('rbind', plot_beta_net_size)

plot_beta_net_size <- plot_beta_net_size[!grepl('W2', plot_beta_net_size$key),]

plot_beta_net_size$window <- as.factor(plot_beta_net_size$window)

plot_beta_net_size$season <- 
  ifelse(grepl('^(.*)(..,1.)$', plot_beta_net_size$key), 'Dry', 'Wet')

plot_beta_net_size$site <- 
  ifelse(grepl('^(.*)(.1,..)$', plot_beta_net_size$key), '1', 
         ifelse(grepl('^(.*)(.2,..)$', plot_beta_net_size$key), '2', 
                ifelse(grepl('^(.*)(.3,..)$', plot_beta_net_size$key), '3', '4')))

plot_beta_net_size$site <- as.factor(plot_beta_net_size$site)

plot_beta_net_size$site <- 
  factor(plot_beta_net_size$site, 
         labels = levels(network_metrics_INDX$obs_net_size$site))

plot_beta_net_size$rainfall_level <- 
  factor(plot_beta_net_size$site, 
         labels = c('Medium-high SL rainfall', 'Medium-low SL rainfall', 
                    'Low SL rainfall', 'High SL rainfall'))

plot_beta_net_size$rainfall_level <- 
  factor(plot_beta_net_size$rainfall_level, 
         levels = c('Low SL rainfall', 'Medium-low SL rainfall', 
                    'Medium-high SL rainfall', 'High SL rainfall'))

plot_beta_net_size$key <- gsub('^(.*)(.....)$', '\\1', plot_beta_net_size$key)

plot_beta_net_sizeWET <- plot_beta_net_size[plot_beta_net_size$season == 'Wet',]

plot_beta_net_sizeWET$key <- factor(plot_beta_net_sizeWET$key) 

plot_beta_net_sizeWET$code <- 
  plot_beta_net_sizeWET %$% paste(key, window, rainfall_level, sep = '_')

sum(plot_beta_net_sizeWET$code == "beta_rainW1_e2_W15_Medium-low rainfall" |
      plot_beta_net_sizeWET$code == "beta_sawrA_e4_W15_Medium-low rainfall")

levels(plot_beta_net_sizeWET$key)

levels(plot_beta_net_sizeWET$rainfall_level)
levels(plot_beta_net_sizeWET$site)

plot_beta_net_sizeWET$key <- 
  factor(plot_beta_net_sizeWET$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

levels(plot_beta_net_sizeWET$window)
plot_beta_net_sizeWET$window <- 
  factor(plot_beta_net_sizeWET$window, 
         levels = c("W1", "W5", "W10", "W15", "W20", "W25"))

#plot_effects_net_sizeWET2 <- 
plot_beta_net_sizeWET |> 
  filter(rainfall_level != 'High SL rainfall') |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Network size'),
                              expression('Swarm area' %->% 'Network size'),
                              expression('ST rainfall' %->% 'Network size'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    title = element_text(size = 13),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x.top = element_text(size = 12.5), 
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    text = element_text(family = 'Times New Roman')
  )

ggsave('wet_season_net_size.jpg', units = 'cm', dpi = 1500,
       height = 25, width = 11)


plot_effects_net_sizeWET <- 
  plot_beta_net_sizeWET |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Network size'),
                              expression('Swarm area' %->% 'Network size'),
                              expression('Rainfall' %->% 'Network size'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = 'Effect', title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman')
  )


plot_beta_net_sizeDRY <- plot_beta_net_size[plot_beta_net_size$season == 'Dry',]

plot_beta_net_sizeDRY$key <- factor(plot_beta_net_sizeDRY$key)

levels(plot_beta_net_sizeDRY$key)

levels(plot_beta_net_sizeDRY$site)

plot_beta_net_sizeDRY$key <- 
  factor(plot_beta_net_sizeDRY$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

plot_effects_net_sizeDRY <- 
  plot_beta_net_sizeDRY |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Network size'),
                              expression('Swarm area' %->% 'Network size'),
                              expression('Rainfall' %->% 'Network size'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = 'Effect', title = 'Dry season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.10), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman'), 
    axis.text.y = element_blank()
  )

plot_effects_net_sizeWET | plot_effects_net_sizeDRY +
  plot_layout(ncol = 1)

ggsave('netwrok_size_effect.jpg', units = 'cm', 
       height = 20, width = 18, dpi = 500)

plot_effects_net_sizeDRY 

plot_effects_net_sizeWET2



# ======= conditional effects ======

s <- network_metrics_INDX$obs_net_size
s <- s[, -c(2, 4)]

s$season <- 
  ifelse(as.numeric(as.character(s$month)) <= 4, 1, 2)

s <- 
  s %$% 
  aggregate(net_size ~ season + site, FUN = length)

s

s2 <- network_metrics_INDX$obs_net_size
s2 <- s2[, -c(2, 4)]
s2$obs_ID <- as.factor(s2$obs_ID)

unique(tibble(x = network_metrics_INDX$obs_net_size$site, 
              x1 = as.numeric(network_metrics_INDX$obs_net_size$site)))

plot_beta_net_size[plot_beta_net_size$p >= 0.9 |
                     plot_beta_net_size$p <= 0.1, ] |> print(n = 100)

effects_net_size <- 
  plot_beta_net_size[plot_beta_net_size$p >= 0.9 |
                       plot_beta_net_size$p <= 0.1, ]

effects_net_size <- split(effects_net_size, 
                          list(effects_net_size$window, 
                               effects_net_size$season, 
                               effects_net_size$site))

effects_net_size <- effects_net_size[unlist(lapply(effects_net_size, function(x) nrow(x) > 0), 
                                            use.names = F)]

# ==== site JUAN ====
for (i in grep('JUAN', names(effects_net_size))) print(effects_net_size[[i]])
# JUAN swarm area --> swarm biomass --> net size (independent of the window)

swarm_area <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_S_OBS$site, 
         day = network_metrics_S_OBS$day, 
         month = network_metrics_S_OBS$month, 
         season = network_metrics_S_OBS$season)

day <- unique(sites[sites$site == 1 & sites$season == 2, ]$day)
month <- unique(sites[sites$site == 1 & sites$season == 2, ]$month)

day <- sites[, c('site', 'month')]
month <- sites[, c('season', 'month')]

psi_e3W15MHR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('psi_e3[', 
                 day$site,
                 ',',
                 day$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

psi_e4W15MHR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('psi_e4[', 
                 day$site,
                 ',',
                 day$month, ']',  
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

tau_e3W15MHR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('tau_e3[', 
                 month$season,
                 ',',
                 month$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

tau_e4W15MHR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('tau_e4[', 
                 month$season,
                 ',',
                 month$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

theta_e3W15MHR <- 
  mod_net_sizeW15_obs$draws(paste('theta_e3[', 
                                  unique(sites$day), ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)


theta_e4W15MHR <- 
  mod_net_sizeW15_obs$draws(paste('theta_e4[', 
                                  unique(sites$day), ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)

x_W15 <- seq(min(swarm_area), max(swarm_area), length.out = 1000)

est_counterSB <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             s_BM <- 
               post_net_sizeW15_obs$`a_e3[1,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e3[1,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e3[1,2]`*x +
               psi_e3W15MHR + theta_e3W15MHR + tau_e3W15MHR
             
             rstudent(2400, nu = 2, mu = s_BM, sigma = post_net_sizeW15_obs$sigma_e3)
             
           })


est_counterSB_MU <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             s_BM <- 
               post_net_sizeW15_obs$`a_e3[1,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e3[1,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e3[1,2]`*x +
               psi_e3W15MHR + theta_e3W15MHR + tau_e3W15MHR
             
             s_BM
             
           })

est_counterSB <- 
  do.call('rbind', 
          apply(est_counterSB, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterSB$x <- x_W15
est_counterSB$season <- 'Wet season'
est_counterSB$mu <- apply(est_counterSB_MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_S_OBS$season, 
              site = network_metrics_S_OBS$site, 
              net_size = network_metrics_S_OBS$net_size)

dat

plot_swarm_biomas_NS_JUAN <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 1 & dat$season == 2,],
    aes(x, y), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterSB, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tan1'
  ) +
  geom_line(
    data = est_counterSB, aes(x, mu), linewidth = 1, 
    color = 'tan1', linetype = 2
  ) +
  labs(x = 'Swarm area', 
       y = 'Arthropod biomass') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 15))

plot_swarm_biomas_NS_JUAN

x_W15_2 <- seq(min(swarm_BM), max(swarm_BM), length.out = 1000)

est_counterNET_SIZE <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[1,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[1,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e4[1,2]`*mean(swarm_area) +
               post_net_sizeW15_obs$`beta_swarmB_e4[1,2]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             rnbinom(2400, mu = exp(NS), size = post_net_sizeW15_obs$sigma_e4)
             
           })


est_counterNET_SIZE_mu <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[1,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[1,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e4[1,2]`*mean(swarm_area) +
               post_net_sizeW15_obs$`beta_swarmB_e4[1,2]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             exp(NS)
             
           })

est_counterNET_SIZE <- 
  do.call('rbind', 
          apply(est_counterNET_SIZE, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNET_SIZE$x <- x_W15_2
est_counterNET_SIZE$season <- 'Wet season'
est_counterNET_SIZE$mu <- apply(est_counterNET_SIZE_mu, 2, mean)


plot_NS_JUAN <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 1 & dat$season == 2,],
    aes(y, net_size), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNET_SIZE, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tan1'
  ) +
  geom_line(
    data = est_counterNET_SIZE, aes(x, mu), linewidth = 1, 
    color = 'tan1'
  ) +
  labs(y = 'Network size', 
       x = 'Arthropod biomass') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 15))

plot_NS_JUAN


plot_effects_net_sizeWET +
  (plot_swarm_biomas_NS_JUAN | plot_NS_JUAN) +
  plot_layout(ncol = 1)

(plot_swarm_biomas_NS_JUAN | plot_NS_JUAN) 

ggsave('JUAN_wet_net_size.jpg', units = 'cm', dpi = 1000, 
       height = 7, width = 14)

# ====== causal effect JUAN =====

counterfact_net_size_JUAN <- 
  function(.fun = mean, mu = T, year = 2022, effect = 'swarm') {
    
    if (effect == 'swarm') {
      s_BM <- 
        post_net_sizeW15_obs$`a_e3[1,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e3[1,2]`*0 +
        post_net_sizeW15_obs$`beta_sawrA_e3[1,2]`*.fun(swarm_area) + 
        psi_e3W15MHR + theta_e3W15MHR + tau_e3W15MHR
      
      NS <- 
        post_net_sizeW15_obs$`a_e4[1,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e4[1,2]`*0 +
        post_net_sizeW15_obs$`beta_sawrA_e4[1,2]`*.fun(swarm_area) +
        post_net_sizeW15_obs$`beta_swarmB_e4[1,2]`*s_BM +
        psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
      
    } else {
      NS <- 
        post_net_sizeW15_obs$`a_e4[1,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e4[1,2]`*0 +
        post_net_sizeW15_obs$`beta_sawrA_e4[1,2]`*mean(swarm_area) +
        post_net_sizeW15_obs$`beta_swarmB_e4[1,2]`*.fun(swarm_BM) +
        psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
    }
    
    if (mu) {
      exp(NS)
    } else {
      set.seed(123)
      rnbinom(length(NS), mu = exp(NS), 
              size = post_net_sizeW15_obs$sigma_e4)
    }
  } 

length(counterfact_net_size_JUAN(min))

plot(density(counterfact_net_size_JUAN(min, mu = F)))
lines(density(counterfact_net_size_JUAN(mean, mu = F)), col = 'blue')
lines(density(counterfact_net_size_JUAN(max, mu = F)), col = 'red')

net_size_counterFAC_contrast_JUAN_SWARM <- 
  tibble(mu_val = c(counterfact_net_size_JUAN(max, F) - counterfact_net_size_JUAN(min, F)), 
         Intervention = 'Causal effect of\n swarm area  ')

mean(net_size_counterFAC_contrast_JUAN_SWARM$mu_val)
sd(net_size_counterFAC_contrast_JUAN_SWARM$mu_val)
mean(net_size_counterFAC_contrast_JUAN_SWARM$mu_val > 0)

(mean(counterfact_net_size_JUAN(min, mu = F)) * 100)/
  mean(counterfact_net_size_JUAN(max, mu = F))

net_size_counterFAC_contrast_JUAN_ARTH <- 
  tibble(mu_val = c(counterfact_net_size_JUAN(max, F, effect = 'n') - 
                      counterfact_net_size_JUAN(min, F, effect = 'n')), 
         Intervention = 'Causal effect of  \n arthropod biomass')

mean(net_size_counterFAC_contrast_JUAN_ARTH$mu_val)
sd(net_size_counterFAC_contrast_JUAN_ARTH$mu_val)
mean(net_size_counterFAC_contrast_JUAN_ARTH$mu_val > 0)

100 - (mean(net_size_counterFAC_contrast_JUAN_SWARM$mu_val) * 100) /
  mean(net_size_counterFAC_contrast_JUAN_ARTH$mu_val)

(median(counterfact_net_size_JUAN(min, mu = F, effect = 'n')) * 100)/
  median(counterfact_net_size_JUAN(max, mu = F, effect = 'n'))

net_size_counter_JUAN <- 
  rbind(net_size_counterFAC_contrast_JUAN_SWARM,
        net_size_counterFAC_contrast_JUAN_ARTH)

plot_contrast_net_sawrm_JUAN <- 
  ggplot() +
  geom_boxplot(data = net_size_counter_JUAN, 
               aes(x = mu_val, y = Intervention), 
               alpha = 0.3, color = 'tan1', 
               fill = 'tan1', width = 0.2) +
  geom_vline(xintercept = 0, linetype = 2, color = 'red') +
  labs(x = 'Contrast between\n interventions (min - max)', 
       y = NULL) +
  theme_bw() +
  lims(x = c(-30, 55)) +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 18))

plot_contrast_net_sawrm_JUAN

ggsave('causal_eff_net_size_JUAN.jpg', units = 'cm', dpi = 1000, 
       height = 5.25, width = 14)

# ======= site PLRD =======

for (i in grep('PLRD', names(effects_net_size))) print(effects_net_size[[i]])
# PLRD swarm area --> net size (independent of the window)

swarm_area <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_S_OBS$site, 
         day = network_metrics_S_OBS$day, 
         month = network_metrics_S_OBS$month, 
         season = network_metrics_S_OBS$season)

day <- unique(sites[sites$site == 3 & sites$season == 2, ]$day)
month <- unique(sites[sites$site == 3 & sites$season == 2, ]$month)

day <- sites[, c('site', 'month')]
month <- sites[, c('season', 'month')]

psi_e4W15LR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('psi_e4[', 
                 day$site,
                 ',',
                 day$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


tau_e4W15LR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('tau_e4[', 
                 month$season,
                 ',',
                 month$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

theta_e4W15LR <- 
  mod_net_sizeW15_obs$draws(paste('theta_e4[', 
                                  unique(sites$day), ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)

x_W15 <- seq(min(swarm_area), max(swarm_area), length.out = 1000)

est_counterNS <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[3,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[3,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e4[3,2]`*x +
               post_net_sizeW15_obs$`beta_swarmB_e4[3,2]`*mean(swarm_BM) +
               psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
             
             rnbinom(length(NS), mu = exp(NS), size = post_net_sizeW15_obs$sigma_e4)
             
           })


est_counterNS_MU <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[3,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[3,2]`*0 +
               post_net_sizeW15_obs$`beta_sawrA_e4[3,2]`*x +
               post_net_sizeW15_obs$`beta_swarmB_e4[3,2]`*mean(swarm_BM) +
               psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
             
             exp(NS)
             
           })

est_counterNS <- 
  do.call('rbind', 
          apply(est_counterNS, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS$x <- x_W15
est_counterNS$season <- 'Wet season'
est_counterNS$mu <- apply(est_counterNS_MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_S_OBS$season, 
              site = network_metrics_S_OBS$site, 
              net_size = network_metrics_S_OBS$net_size)

dat

plot_NS_PLR <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 1 & dat$season == 2,],
    aes(x, net_size), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tomato3'
  ) +
  geom_line(
    data = est_counterNS, aes(x, mu), linewidth = 1, 
    color = 'tomato3'
  ) +
  labs(x = 'Swarm area', 
       y = 'Network size') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 16))

plot_NS_PLR

ggsave('PRL_wet_net_size.jpg', units = 'cm', dpi = 1000, 
       height = 7, width = 7)

# ====== causal effect PRL =====

counterfact_net_size_PRL <- 
  function(.fun = mean, mu = T, year = 2022) {
    
    NS <- 
      post_net_sizeW15_obs$`a_e4[3,2]` + 
      post_net_sizeW15_obs$`beta_rainW1_e4[3,2]`*0 +
      post_net_sizeW15_obs$`beta_sawrA_e4[3,2]`*.fun(swarm_area) +
      post_net_sizeW15_obs$`beta_swarmB_e4[3,2]`*mean(swarm_BM) +
      psi_e4W15LR + theta_e4W15LR + tau_e4W15LR 
    
    if (mu) {
      exp(NS)
    } else {
      set.seed(123)
      rnbinom(length(NS), mu = exp(NS), 
              size = post_net_sizeW15_obs$sigma_e4)
    }
  } 

length(counterfact_net_size_PRL(min))

plot(density(counterfact_net_size_PRL(min, mu = F)))
lines(density(counterfact_net_size_PRL(mean, mu = F)), col = 'blue')
lines(density(counterfact_net_size_PRL(max, mu = F)), col = 'red')

net_size_counter_PRL <- 
  tibble(mu_val = c(counterfact_net_size_PRL(max, F) - counterfact_net_size_PRL(min, F)), 
         Intervention = 'Causal effect of\n swarm area  ')

mean(net_size_counter_PRL$mu_val)
sd(net_size_counter_PRL$mu_val)
mean(net_size_counter_PRL$mu_val > 0)

(mean(counterfact_net_size_PRL(min, F)) * 100) /
  mean(counterfact_net_size_PRL(max, F))

plot_contrast_net_sawrm_PRL <- 
  ggplot() +
  geom_boxplot(data = net_size_counter_PRL, 
               aes(x = mu_val, y = Intervention), 
               alpha = 0.3, color = 'tomato3', 
               fill = 'tomato3', width = 0.2) +
  geom_vline(xintercept = 0, linetype = 2, color = 'red') +
  labs(x = 'Contrast between\n interventions (min - max)', 
       y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18))

plot_contrast_net_sawrm_PRL

ggsave('causal_eff_net_size_PRL.jpg', units = 'cm', dpi = 1000, 
       height = 5, width = 14)


# ================== site LIMB =======================


for (i in grep('LIMB', names(effects_net_size))) print(effects_net_size[[i]])
# LIMB Rain W15 --> swarm area --> net size
# LIMB Rain W15 --> net size
# LIMB Rain W25 --> net size
# LIMB swarm area W25 --> net size

unique(tibble(x = network_metrics_INDX$obs_net_size$site, 
              x1 = as.numeric(network_metrics_INDX$obs_net_size$site)))

swarm_area <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_S_OBS$site, 
         day = network_metrics_S_OBS$day, 
         month = network_metrics_S_OBS$month, 
         season = network_metrics_S_OBS$season, 
         rainW15 = network_metrics_S_OBS$cum_rainfall_W15)

day <- unique(sites[sites$site == 2 & sites$season == 2, ]$day)
month <- unique(sites[sites$site == 2 & sites$season == 2, ]$month)

day <- sites[, c('site', 'month')]
month <- sites[, c('season', 'month')]

psi_e2W15MLR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('psi_e2[', 
                 day$site,
                 ',',
                 day$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

psi_e4W15MLR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('psi_e4[', 
                 day$site,
                 ',',
                 day$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

tau_e2W15MLR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('tau_e2[', 
                 month$season,
                 ',',
                 month$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


tau_e4W15MLR <- 
  mod_net_sizeW15_obs$draws(
    unique(paste('tau_e4[', 
                 month$season,
                 ',',
                 month$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)
unique(sites$day)
theta_e2W15MLR <- 
  mod_net_sizeW15_obs$draws(paste('theta_e2[', 
                                  unique(sites$day), ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)


theta_e4W15MLR <- 
  mod_net_sizeW15_obs$draws(paste('theta_e4[', 
                                  unique(sites$day), ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)

x_W15_1 <- seq(min(network_metrics_S_OBS$cum_rainfall_W15), 
               max(network_metrics_S_OBS$cum_rainfall_W15), length.out = 1000)

est_counterSA <- 
  sapply(x_W15_1, FUN = 
           function(x) {
             
             SA <- 
               post_net_sizeW15_obs$`a_e2[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e2[2,2]`*x +
               psi_e2W15MLR + theta_e2W15MLR + tau_e2W15MLR
             
             rstudent(2400, nu = 2, mu = SA, sigma = post_net_sizeW15_obs$sigma_e2)
             
           })


est_counterSA_MU <- 
  sapply(x_W15_1, FUN = 
           function(x) {
             
             SA <- 
               post_net_sizeW15_obs$`a_e2[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e2[2,2]`*x +
               psi_e2W15MLR + theta_e2W15MLR + tau_e2W15MLR
             
             SA
             
           })

est_counterSA <- 
  do.call('rbind', 
          apply(est_counterSA, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterSA$x <- x_W15_1
est_counterSA$season <- 'Wet season'
est_counterSA$mu <- apply(est_counterSA_MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_S_OBS$season, 
              site = network_metrics_S_OBS$site, 
              net_size = network_metrics_S_OBS$net_size, 
              rainW15 = network_metrics_S_OBS$cum_rainfall_W15)

dat

plot_swarm_area_LIMB <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 2 & dat$season == 2,],
    aes(x = rainW15, y = x), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterSA, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'purple4'
  ) +
  geom_line(
    data = est_counterSA, aes(x, mu), linewidth = 1, 
    color = 'purple4', linetype = 2
  ) +
  labs(y = 'Swarm area', 
       x = 'ST rainfall W15') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16))

plot_swarm_area_LIMB

x_W15_2 <- seq(min(swarm_area), max(swarm_area), length.out = 1000)

est_counterNS <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*0 +
               post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*x +
               psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
             
             rnbinom(length(NS), mu = exp(NS), size = post_net_sizeW15_obs$sigma_e4)
             
           })


est_counterNS_MU <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*0 +
               post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*x +
               psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
             
             exp(NS)
             
           })

est_counterNS <- 
  do.call('rbind', 
          apply(est_counterNS, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS$x <- x_W15_2
est_counterNS$season <- 'Wet season'
est_counterNS$mu <- apply(est_counterNS_MU, 2, mean)


plot_NS_LIMB <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 2 & dat$season == 2,],
    aes(x, net_size), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'purple4'
  ) +
  geom_line(
    data = est_counterNS, aes(x, mu), linewidth = 1, 
    color = 'purple4'
  ) +
  labs(y = 'Network size', 
       x = 'Swarm area') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16))

plot_NS_LIMB


est_counterNS2 <- 
  sapply(x_W15_1, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*x +
               post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
               psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
             
             rnbinom(length(NS), mu = exp(NS), size = post_net_sizeW15_obs$sigma_e4)
             
           })


est_counterNS2_MU <- 
  sapply(x_W15_1, FUN = 
           function(x) {
             
             NS <- 
               post_net_sizeW15_obs$`a_e4[2,2]` + 
               post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*x +
               post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
               psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
             
             exp(NS)
             
           })

est_counterNS2 <- 
  do.call('rbind', 
          apply(est_counterNS2, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS2$x <- x_W15_1
est_counterNS2$season <- 'Wet season'
est_counterNS2$mu <- apply(est_counterNS2_MU, 2, mean)


plot_NS_RAIN_LIMB <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 2 & dat$season == 2,],
    aes(rainW15, net_size), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS2, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'purple4'
  ) +
  geom_line(
    data = est_counterNS2, aes(x, mu), linewidth = 1, 
    color = 'purple4'
  ) +
  labs(y = 'Network size', 
       x = 'ST rainfall W15') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16))

plot_swarm_area_LIMB +
  plot_NS_LIMB + 
  plot_NS_RAIN_LIMB +
  plot_layout(nrow = 1)

ggsave('LIMB_wet_net_size.jpg', units = 'cm', dpi = 1000, 
       height = 7, width = 18)

# ====== causal effect LIMB =====

counterfact_net_size_LIMB <- 
  function(.fun = mean, mu = T, year = 2022, effect = 'rain1') {
    
    rain <- network_metrics_S_OBS$cum_rainfall_W15
    
    if (effect == 'rain1') {
      
      SA <- 
        post_net_sizeW15_obs$`a_e2[2,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e2[2,2]`*.fun(rain) +
        psi_e2W15MLR + theta_e2W15MLR + tau_e2W15MLR
      
      NS <- 
        post_net_sizeW15_obs$`a_e4[2,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*0 +
        post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*.fun(swarm_BM) +
        post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*SA +
        psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
      
      
    } 
    
    if (effect == 'n') {
      NS <- 
        post_net_sizeW15_obs$`a_e4[2,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*.fun(rain) +
        post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
        post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
        psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
    }
    
    if (effect == 'swarm') {
      NS <- 
        post_net_sizeW15_obs$`a_e4[2,2]` + 
        post_net_sizeW15_obs$`beta_rainW1_e4[2,2]`*0 +
        post_net_sizeW15_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
        post_net_sizeW15_obs$`beta_sawrA_e4[2,2]`*.fun(swarm_area) +
        psi_e4W15MLR + theta_e4W15MLR + tau_e4W15MLR
    }
    
    if (mu) {
      exp(NS)
    } else {
      set.seed(123)
      rnbinom(length(NS), mu = exp(NS), 
              size = post_net_sizeW15_obs$sigma_e4)
    }
  } 

net_size_counterFAC_contrast_LIMB_SWARM <- 
  tibble(mu_val = c(counterfact_net_size_LIMB(max, F) - counterfact_net_size_LIMB(min, F)), 
         Intervention = 'Causal effect of       \n ST rainfall W15 (indirect)')

mean(net_size_counterFAC_contrast_LIMB_SWARM$mu_val > 0)
mean(net_size_counterFAC_contrast_LIMB_SWARM$mu_val)
sd(net_size_counterFAC_contrast_LIMB_SWARM$mu_val)

(mean(counterfact_net_size_LIMB(min, F)) * 100) /
  mean(counterfact_net_size_LIMB(max, F))

net_size_counterFAC_contrast_LIMB_ARTH <- 
  tibble(mu_val = c(counterfact_net_size_LIMB(max, F, effect = 'n') - 
                      counterfact_net_size_LIMB(min, F, effect = 'n')), 
         Intervention = 'Causal effect of       \n ST rainfall W15 (direct)')

mean(net_size_counterFAC_contrast_LIMB_ARTH$mu_val > 0)
mean(net_size_counterFAC_contrast_LIMB_ARTH$mu_val)
sd(net_size_counterFAC_contrast_LIMB_ARTH$mu_val)

(mean(counterfact_net_size_LIMB(min, F, effect = 'n')) * 100) /
  mean(counterfact_net_size_LIMB(max, F, effect = 'n'))

net_size_counterFAC_contrast_LIMB_SWARM2 <- 
  tibble(mu_val = c(counterfact_net_size_LIMB(max, F, effect = 'swarm') - 
                      counterfact_net_size_LIMB(min, F, effect = 'swarm')), 
         Intervention = 'Causal effect of    \n swarm area')

mean(net_size_counterFAC_contrast_LIMB_SWARM2$mu_val > 0)
mean(net_size_counterFAC_contrast_LIMB_SWARM2$mu_val)
sd(net_size_counterFAC_contrast_LIMB_SWARM2$mu_val)


100 - (mean(net_size_counterFAC_contrast_LIMB_SWARM$mu_val) * 100) / 
  mean(net_size_counterFAC_contrast_LIMB_SWARM2$mu_val)

net_size_counter_LIMB <- 
  rbind(net_size_counterFAC_contrast_LIMB_SWARM,
        net_size_counterFAC_contrast_LIMB_ARTH)

plot_contrast_net_sawrm_LIMB <- 
  ggplot() +
  geom_boxplot(data = net_size_counter_LIMB, 
               aes(x = mu_val, y = Intervention), 
               alpha = 0.3, color = 'purple4', 
               fill = 'purple4', width = 0.2) +
  geom_vline(xintercept = 0, linetype = 2, color = 'red') +
  labs(x = 'Contrast between\n interventions (min - max)', 
       y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 17))

plot_contrast_net_sawrm_LIMB

ggsave('causal_eff_net_size_LIMB.jpg', units = 'cm', dpi = 1000, 
       height = 5, width = 15)



for (i in grep('SHER', names(effects_net_size))) print(effects_net_size[[i]])
# effects too variable (most consistent comes from swarm area --> arthropod biomass)


# NOO EFFECTS in this site


# ==== contrast seasons ======

posteriors <- ls()[grep('^(post_net_size)(W[0-9]*)', ls())]

df_seasons <- 
  lapply(posteriors, FUN = 
         function(draws) {
           
           p <- get(draws)
           
           w <- gsub('^(.*)(W[0-9]*)(.*)$', '\\2', draws)
           
           post_season <- 
             p[, colnames(p)[grep('^a_e4', colnames(post_net_sizeW1_obs))]] |> 
             gather() 
           
           post_season <- split(post_season, post_season$key)  
           
           sites <- rep(c('Medium-high rainfall', 
                          'Medium-low rainfall', 
                          'low rainfall', 
                          'High rainfall'), each = 2)
           
           season <- rep(c('Dry', 'Wet'), 4)
           
           for (i in seq_along(sites)) {
             post_season[[i]]$site <- sites[[i]]
             post_season[[i]]$season <- season[[i]]
             post_season[[i]] <- post_season[[i]][, -1]
           }
           
           post_season <- do.call('rbind', post_season)
           post_season <- split(post_season, post_season$site)
           
           post_season <- 
             lapply(post_season, FUN = 
                      function(x) {
                        j <- 
                          exp(x[x$season == 'Dry', ]$value) - 
                          exp(x[x$season == 'Wet', ]$value)
                        
                        tibble(site = x$site[1], 
                               contrast = j, 
                               p = mean(j > 0))
                      })
           
           post_season <- do.call('rbind', post_season)
           
           post_season$model <- w
           
           post_season
         })

df_seasons <- do.call('rbind', df_seasons)

df_seasons |> 
  ggplot(aes(contrast, fill = site)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  labs(x = 'Network size\n (contrast between wet and dry season)') +
  lims(x = c(-40, 80)) +
  facet_wrap(~model)

# ====== Clustering (obs) W1 ====

str(network_metrics_obs2)

network_metrics_obs2$clustering <- 
  sapply(network_metrics_obs2$clustering, FUN = 
           function(x) {
             if(x >= 1) 0.999
             else x
           })
names(network_metrics_obs2)
summary(network_metrics_obs2$clustering)

cat(file = 'w1_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W1;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W1[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w1_clustering.stan', sep ='')

fit_clusteringW1 <- cmdstan_model(file, compile = T)

mod_clusteringW1_obs <- 
  fit_clusteringW1$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW1_obs$save_object('causal_mod_clusteringW1.rds')

mod_clusteringW1_obs <- readRDS('causal_mod_clusteringW1.rds')

# mod_clusteringW1_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW1_obs$summary(paste('theta_e4[', 
#                                  1:network_metrics_obs2[-3]$N_day, ']', 
#                                  sep = '')) |> print(n = 200) 
# 
# mod_clusteringW1_obs$summary(paste('theta_e2[', 
#                                  1:network_metrics_obs2[-3]$N_day, ']', 
#                                  sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW1_obs$summary(paste('theta_e3[', 
#                                  1:network_metrics_obs2[-3]$N_day, ']', 
#                                  sep = '')) |> print(n = 130)
# 

png('w1_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('psi_e2')) 

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('psi_e3'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('psi_e4'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('tau_e4'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('tau_e2'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('tau_e3'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('theta_e4'))
mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('theta_e3'))

mod_diagnostics(mod_clusteringW1_obs, 
                mod_clusteringW1_obs$summary('theta_e2'))
dev.off()



summary_clusteringW1 <- mod_clusteringW1_obs$summary(c('beta_rainW2_e1', 
                                                       'beta_rainW1_e2',
                                                       'beta_rainW1_e3',
                                                       'beta_rainW1_e4',
                                                       'beta_sawrA_e3',
                                                       'beta_sawrA_e4',
                                                       'beta_swarmB_e4', 
                                                       'a_e1', 'a_e2', 
                                                       'a_e3', 'a_e4',
                                                       'sigma_e1', 'sigma_e2', 
                                                       'sigma_e3', 'sigma_e4', 
                                                       'swarm_area',
                                                       'SBiomass_merge',
                                                       'lp__'))

summary_clusteringW1 |> print(n = 15)



mod_diagnostics(mod_clusteringW1_obs, summary_clusteringW1)


post_clusteringW1_obs <- mod_clusteringW1_obs$draws(c('beta_rainW2_e1', 
                                                      'beta_rainW1_e2',
                                                      'beta_rainW1_e3',
                                                      'beta_rainW1_e4',
                                                      'beta_sawrA_e3',
                                                      'beta_sawrA_e4',
                                                      'beta_swarmB_e4', 
                                                      'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                      'sigma_e1', 'sigma_e2', 
                                                      'sigma_e3', 'sigma_e4',
                                                      'swarm_area',
                                                      'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_clusteringW1_obs[, grep('swarm_area', colnames(post_clusteringW1_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_clusteringW1_obs[, grep('SBiomass_merge', colnames(post_clusteringW1_obs))], 
        2, mean)

ppcheck_swarmA <- mod_clusteringW1_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW1_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW1_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW1_obs$draws('ppcheck_e1', format = 'matrix')

plot(density(network_metrics_obs2[-3]$cum_rainfall_W1), 
     main = '', xlab = 'Cum rainfall W1')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W1), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW1_obs)

post_beta_clusteringW1 <- gather(post_clusteringW1_obs[, grep('beta', colnames(post_clusteringW1_obs))])

post_beta_clusteringW1$key <- as.factor(post_beta_clusteringW1$key)

post_beta_clusteringW1 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== Clustering (obs) W5 ====


cat(file = 'w5_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W5[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w5_clustering.stan', sep ='')

fit_clusteringW5 <- cmdstan_model(file, compile = T)

mod_clusteringW5_obs <- 
  fit_clusteringW5$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW5_obs$save_object('causal_mod_clusteringW5.rds')

mod_clusteringW5_obs <- readRDS('causal_mod_clusteringW5.rds')

# mod_clusteringW5_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW5_obs$summary(paste('theta_e4[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 200) 
# 
# mod_clusteringW5_obs$summary(paste('theta_e2[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW5_obs$summary(paste('theta_e3[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130)
# 

png('W5_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(paste('theta_e4[', 
                                                   1:network_metrics_obs2[-3]$N_day, ']', 
                                                   sep = '')))
mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(paste('theta_e3[', 
                                                   1:network_metrics_obs2[-3]$N_day, ']', 
                                                   sep = '')))

mod_diagnostics(mod_clusteringW5_obs, 
                mod_clusteringW5_obs$summary(paste('theta_e2[', 
                                                   1:network_metrics_obs2[-3]$N_day, ']', 
                                                   sep = '')))
dev.off()



summary_clusteringW5 <- mod_clusteringW5_obs$summary(c('beta_rainW2_e1', 
                                                       'beta_rainW1_e2',
                                                       'beta_rainW1_e3',
                                                       'beta_rainW1_e4',
                                                       'beta_sawrA_e3',
                                                       'beta_sawrA_e4',
                                                       'beta_swarmB_e4', 
                                                       'a_e1', 'a_e2', 
                                                       'a_e3', 'a_e4',
                                                       'sigma_e1', 'sigma_e2', 
                                                       'sigma_e3', 'sigma_e4', 
                                                       'swarm_area',
                                                       'SBiomass_merge',
                                                       'lp__'))

summary_clusteringW5 |> print(n = 15)



mod_diagnostics(mod_clusteringW5_obs, summary_clusteringW5)


post_clusteringW5_obs <- mod_clusteringW5_obs$draws(c('beta_rainW2_e1', 
                                                      'beta_rainW1_e2',
                                                      'beta_rainW1_e3',
                                                      'beta_rainW1_e4',
                                                      'beta_sawrA_e3',
                                                      'beta_sawrA_e4',
                                                      'beta_swarmB_e4', 
                                                      'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                      'sigma_e1', 'sigma_e2', 
                                                      'sigma_e3', 'sigma_e4',
                                                      'swarm_area',
                                                      'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_clusteringW5_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW5_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW5_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW5_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_clusteringW5_obs[, grep('swarm_area', colnames(post_clusteringW5_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_clusteringW5_obs[, grep('SBiomass_merge', colnames(post_clusteringW5_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W5), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W5), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW5_obs)


post_beta_clusteringW5 <- gather(post_clusteringW5_obs[, grep('beta', colnames(post_clusteringW5_obs))])

post_beta_clusteringW5$key <- as.factor(post_beta_clusteringW5$key)

post_beta_clusteringW5 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== Clustering (obs) W10 ====


cat(file = 'w10_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W10[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w10_clustering.stan', sep ='')

fit_clusteringW10 <- cmdstan_model(file, compile = T)

mod_clusteringW10_obs <- 
  fit_clusteringW10$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW10_obs$save_object('causal_mod_clusteringW10.rds')

mod_clusteringW10_obs <- readRDS('causal_mod_clusteringW10.rds')
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW10_obs$summary(paste('theta_e4[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 200) 
# 
# mod_clusteringW10_obs$summary(paste('theta_e2[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW10_obs$summary(paste('theta_e3[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130)


png('W10_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_clusteringW10_obs, 
                mod_clusteringW10_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_clusteringW10 <- mod_clusteringW10_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_clusteringW10 |> print(n = 15)



mod_diagnostics(mod_clusteringW10_obs, summary_clusteringW10)


post_clusteringW10_obs <- mod_clusteringW10_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_clusteringW10_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW10_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW10_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW10_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_clusteringW10_obs[, grep('swarm_area', colnames(post_clusteringW10_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_clusteringW10_obs[, grep('SBiomass_merge', colnames(post_clusteringW10_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W10), 
     main = '', xlab = 'Cum rainfall W1')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W10), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW10_obs)


post_beta_clusteringW10 <- gather(post_clusteringW10_obs[, grep('beta', colnames(post_clusteringW10_obs))])

post_beta_clusteringW10$key <- as.factor(post_beta_clusteringW10$key)


post_beta_clusteringW10 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== Clustering (obs) W15 ====


cat(file = 'w15_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W15[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w15_clustering.stan', sep ='')

fit_clusteringW15 <- cmdstan_model(file, compile = T)

mod_clusteringW15_obs <- 
  fit_clusteringW15$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW15_obs$save_object('causal_mod_clusteringW15.rds')

mod_clusteringW15_obs <- readRDS('causal_mod_clusteringW15.rds')
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW15_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_clusteringW15_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW15_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)


png('W15_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_clusteringW15_obs, 
                mod_clusteringW15_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_clusteringW15 <- mod_clusteringW15_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_clusteringW15 |> print(n = 15)



mod_diagnostics(mod_clusteringW15_obs, summary_clusteringW15)


post_clusteringW15_obs <- mod_clusteringW15_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_clusteringW15_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW15_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW15_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW15_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_clusteringW15_obs[, grep('swarm_area', colnames(post_clusteringW15_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_clusteringW15_obs[, grep('SBiomass_merge', colnames(post_clusteringW15_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W15), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W15), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW15_obs)

post_beta_clusteringW15 <- gather(post_clusteringW15_obs[, grep('beta', colnames(post_clusteringW15_obs))])

post_beta_clusteringW15$key <- as.factor(post_beta_clusteringW15$key)


post_beta_clusteringW15 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== Clustering (obs) W20 ====


cat(file = 'w20_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W20[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w20_clustering.stan', sep ='')

fit_clusteringW20 <- cmdstan_model(file, compile = T)

mod_clusteringW20_obs <- 
  fit_clusteringW20$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW20_obs$save_object('causal_mod_clusteringW20.rds')

mod_clusteringW20_obs <- readRDS('causal_mod_clusteringW20.rds')
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW20_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_clusteringW20_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW20_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)
# 

png('W20_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_clusteringW20_obs, 
                mod_clusteringW20_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_clusteringW20 <- mod_clusteringW20_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_clusteringW20 |> print(n = 15)



mod_diagnostics(mod_clusteringW20_obs, summary_clusteringW20)


post_clusteringW20_obs <- mod_clusteringW20_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_clusteringW20_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW20_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW20_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW20_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- 
  apply(post_clusteringW20_obs[, grep('swarm_area', 
                                      colnames(post_clusteringW20_obs))], 
        2, mean)

swarm_BM <- 
  apply(post_clusteringW20_obs[, grep('SBiomass_merge', 
                                      colnames(post_clusteringW20_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W20), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W20), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW20_obs)

post_beta_clusteringW20 <- gather(post_clusteringW20_obs[, grep('beta', colnames(post_clusteringW20_obs))])

post_beta_clusteringW20$key <- as.factor(post_beta_clusteringW20$key)


post_beta_clusteringW20 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)



# ====== Clustering (obs) W25 ====


cat(file = 'w25_clustering.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] clustering;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W30;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W25[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      clustering ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w25_clustering.stan', sep ='')

fit_clusteringW25 <- cmdstan_model(file, compile = T)

mod_clusteringW25_obs <- 
  fit_clusteringW25$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_clusteringW25_obs$save_object('causal_mod_clusteringW25.rds')

mod_clusteringW25_obs <- readRDS('causal_mod_clusteringW25.rds')
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_clusteringW25_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_clusteringW25_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_clusteringW25_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)


png('W25_clustering_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_clusteringW25_obs, 
                mod_clusteringW25_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_clusteringW25 <- mod_clusteringW25_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_clusteringW25 |> print(n = 15)



mod_diagnostics(mod_clusteringW25_obs, summary_clusteringW25)


post_clusteringW25_obs <- mod_clusteringW25_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_clusteringW25_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_clusteringW25_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_clustering <- mod_clusteringW25_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_clusteringW25_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_clusteringW25_obs[, grep('swarm_area', colnames(post_clusteringW25_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_clusteringW25_obs[, grep('SBiomass_merge', colnames(post_clusteringW25_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W25), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W25), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$clustering), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_clustering[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$clustering), col = 'red')

dim(post_clusteringW25_obs)

post_beta_clusteringW25 <- gather(post_clusteringW25_obs[, grep('beta', colnames(post_clusteringW25_obs))])

post_beta_clusteringW25$key <- as.factor(post_beta_clusteringW25$key)


post_beta_clusteringW25 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== plotting and counterfactual clustering ====

# ====== forest plot and conditional effects ====

t <- paste('W', c(1, seq(5, 25, by = 5)), sep = '')

plot_beta_clustering <- vector('list', length(t))

for (i in seq_along(t)) {
  cadena <- paste('^(.*)beta_clustering(.*)', t[i], '$', sep = '')
  cadena2 <- paste('post_clustering', t[i], '_obs', sep = '')
  df <- get(ls()[grepl(cadena, ls())])
  temp <- get(ls()[grepl(cadena2, ls())])
  
  temp <- temp[, grep('^beta', colnames(temp))]
  
  temp <- apply(temp, 2, function(x) mean(x > 0))
  
  temp <- tibble(p = temp, key = names(temp))
  
  df <- 
    df |> 
    group_by(key) |> 
    transmute(li = quantile(value, 0.025), 
              ls = quantile(value, 0.975), 
              mu = mean(value), 
              window = t[i]) |> 
    unique()
  
  n <- c(1, seq(5, 25, by = 5))[i]
  
  df <- df[!grepl(n + 4, df$key), ]
  
  df <- full_join(df, temp, by = 'key')
  
  plot_beta_clustering[[i]] <- df
  print(i)
}

plot_beta_clustering <- do.call('rbind', plot_beta_clustering)

plot_beta_clustering <- plot_beta_clustering[!grepl('W2', plot_beta_clustering$key),]

plot_beta_clustering$window <- as.factor(plot_beta_clustering$window)

plot_beta_clustering$season <- 
  ifelse(grepl('^(.*)(..,1.)$', plot_beta_clustering$key), 'Dry', 'Wet')

plot_beta_clustering$site <- 
  ifelse(grepl('^(.*)(.1,..)$', plot_beta_clustering$key), '1', 
         ifelse(grepl('^(.*)(.2,..)$', plot_beta_clustering$key), '2', 
                ifelse(grepl('^(.*)(.3,..)$', plot_beta_clustering$key), '3', '4')))

plot_beta_clustering$site <- as.factor(plot_beta_clustering$site)

plot_beta_clustering$site <- 
  factor(plot_beta_clustering$site, 
         labels = levels(network_metrics_INDX$obs_all$site))

plot_beta_clustering$rainfall_level <- 
  factor(plot_beta_clustering$site, 
         labels = c('Medium-high SL rainfall', 'Medium-low SL rainfall', 
                    'Low SL rainfall', 'High SL rainfall'))

plot_beta_clustering$rainfall_level <- 
  factor(plot_beta_clustering$rainfall_level, 
         levels = c('Low SL rainfall', 'Medium-low SL rainfall', 
                    'Medium-high SL rainfall', 'High SL rainfall'))


plot_beta_clustering$key <- gsub('^(.*)(.....)$', '\\1', plot_beta_clustering$key)

plot_beta_clusteringWET <- plot_beta_clustering[plot_beta_clustering$season == 'Wet',]

plot_beta_clusteringWET$key <- factor(plot_beta_clusteringWET$key) 

plot_beta_clusteringWET$code <- 
  plot_beta_clusteringWET %$% paste(key, window, rainfall_level, sep = '_')

sum(plot_beta_clusteringWET$code == "beta_rainW1_e2_W15_Medium-low rainfall" |
      plot_beta_clusteringWET$code == "beta_sawrA_e4_W15_Medium-low rainfall")

levels(plot_beta_clusteringWET$key)

levels(plot_beta_clusteringWET$rainfall_level)
levels(plot_beta_clusteringWET$site)

plot_beta_clusteringWET$key <- 
  factor(plot_beta_clusteringWET$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

plot_effects_clusteringWET <- 
  plot_beta_clusteringWET |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Clustering'),
                              expression('Rainfall' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = 'Effect', title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman')
  )


plot_beta_clusteringDRY <- plot_beta_clustering[plot_beta_clustering$season == 'Dry',]

plot_beta_clusteringDRY$key <- factor(plot_beta_clusteringDRY$key)

levels(plot_beta_clusteringDRY$key)

levels(plot_beta_clusteringDRY$site)

plot_beta_clusteringDRY$key <- 
  factor(plot_beta_clusteringDRY$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

plot_effects_clusteringDRY <- 
  plot_beta_clusteringDRY |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Clustering'),
                              expression('Rainfall' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Dry season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.10), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman'),
    axis.text.y = element_blank()
  )

plot_effects_clusteringWET +
  plot_effects_clusteringDRY +
  plot_layout(ncol = 2)

ggsave('clustering_effect.jpg', units = 'cm', 
       height = 20, width = 20, dpi = 500)

levels(plot_beta_clusteringDRY$window)
plot_beta_clusteringDRY$window <- 
  factor(plot_beta_clusteringDRY$window, 
         levels = c("W1", "W5", "W10", "W15", "W20", "W25"))

plot_beta_clusteringDRY2 <- 
  plot_beta_clusteringDRY |> 
  filter(rainfall_level == 'Medium-high SL rainfall') |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Clustering'),
                              expression('ST rainfall' %->% 'Clustering'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Dry season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    title = element_text(size = 13),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x.top = element_text(size = 12.5), 
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.2), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    text = element_text(family = 'Times New Roman')
  )

plot_beta_clusteringDRY2

ggsave('clustering_effect.jpg', units = 'cm', 
       height = 10, width = 13, dpi = 1500)

# ======= conditional effects ======

s <- network_metrics_INDX$obs_all
s <- s[, -c(1:3)]

s$season <- 
  ifelse(as.numeric(as.character(s$month)) <= 4, 1, 2)

s <- 
  s %$% 
  aggregate(clustering ~ season + site, FUN = length)

s

s2 <- network_metrics_INDX$obs_all
s2 <- s2[, -c(2, 4)]
s2$obs_ID <- as.factor(s2$obs_ID)

unique(tibble(x = network_metrics_INDX$obs_all$site, 
              x1 = as.numeric(network_metrics_INDX$obs_all$site)))

plot_beta_clustering[plot_beta_clustering$p >= 0.9 |
                       plot_beta_clustering$p <= 0.1, ] |> print(n = 100)

effects_clustering <- 
  plot_beta_clustering[plot_beta_clustering$p >= 0.9 |
                         plot_beta_clustering$p <= 0.1, ]

effects_clustering <- split(effects_clustering, 
                            list(effects_clustering$window, 
                                 effects_clustering$season, 
                                 effects_clustering$site))

effects_clustering <- effects_clustering[unlist(lapply(effects_clustering, function(x) nrow(x) > 0), 
                                                use.names = F)]

# ==== site JUAN ====
for (i in grep('JUAN', names(effects_clustering))) print(effects_clustering[[i]])
# JUAN arthropod biomass --> net size (Dry season)
#  Medium-high rainfall

swarm_area <- 
  sapply(ls()[grep('post_net_size', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')


swarm_BM <- 
  sapply(ls()[grep('post_clustering', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_obs2$site, 
         day = network_metrics_obs2$day, 
         month = network_metrics_obs2$month, 
         season = network_metrics_obs2$season)

day <- unique(sites[, c('day', 'site', 'month', 'season')])

day <- day[day$site == 1 & 
             day$season == 1, ]

day2 <- unique(day[, -1])

psi_e4W15MHR <- 
  mod_clusteringW15_obs$draws(
    paste('psi_e4[', 1, ',', day2$month, ']', sep = ''), format = 'matrix') |> 
  apply(1, mean)

tau_e4W15MHR <- 
  mod_clusteringW15_obs$draws(
    paste('tau_e4[', 1, ',', day2$month, ']', sep = ''), format = 'matrix') |> 
  apply(1, mean)

theta_e4W15MHR <- 
  mod_clusteringW15_obs$draws(
    paste('theta_e4[', day$day, ']', sep = ''), format = 'matrix') |> 
  apply(1, mean)


dat <- tibble(x = swarm_BM, 
              season = network_metrics_obs2$season, 
              site = network_metrics_obs2$site, 
              clustering = network_metrics_obs2$clustering)

dat


x_W15_2 <- seq(min(swarm_BM), max(swarm_BM), length.out = 1000)

est_counterclustering <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_clusteringW15_obs$`a_e4[1,1]` + 
               post_clusteringW15_obs$`beta_rainW1_e4[1,1]`*0 +
               post_clusteringW15_obs$`beta_sawrA_e4[1,1]`*mean(swarm_area) +
               post_clusteringW15_obs$`beta_swarmB_e4[1,1]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             NS <- inv_logit(NS)
             
             p1 <- NS * post_clusteringW15_obs$sigma_e4
             p2 <- (1 - NS) * post_clusteringW15_obs$sigma_e4
             
             
             rbeta(length(NS), p1, p2)
             
           })


est_counterclustering_mu <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_clusteringW15_obs$`a_e4[1,1]` + 
               post_clusteringW15_obs$`beta_rainW1_e4[1,1]`*0 +
               post_clusteringW15_obs$`beta_sawrA_e4[1,1]`*mean(swarm_area) +
               post_clusteringW15_obs$`beta_swarmB_e4[1,1]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             inv_logit(NS)
             
           })

est_counterclustering <- 
  do.call('rbind', 
          apply(est_counterclustering, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterclustering$x <- x_W15_2
est_counterclustering$season <- 'Wet season'
est_counterclustering$mu <- apply(est_counterclustering_mu, 2, mean)


#plot_NS_JUAN <- 
ggplot() +
  geom_point(
    data = dat[dat$site == 1 & dat$season == 2,],
    aes(x, clustering), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterclustering, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tan1'
  ) +
  geom_line(
    data = est_counterclustering, aes(x, mu), linewidth = 1, 
    color = 'tan1'
  ) +
  labs(y = 'Clustering', 
       x = 'Arthropod biomass') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16))

ggsave('JUAN_DRY_clustering.jpg', units = 'cm', dpi = 1000, 
       height = 9, width = 9)

# ====== causal effect JUAN =====

counterfact_clustering_JUAN <- 
  function(.fun = mean, mu = T) {
    
    NS <- 
      post_clusteringW15_obs$`a_e4[1,1]` + 
      post_clusteringW15_obs$`beta_rainW1_e4[1,1]`*0 +
      post_clusteringW15_obs$`beta_sawrA_e4[1,1]`*mean(swarm_area) +
      post_clusteringW15_obs$`beta_swarmB_e4[1,1]`*.fun(swarm_BM) +
      psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
    
    if (mu) {
      inv_logit(NS)
    } else {
      NS <- inv_logit(NS)
      p1 <- NS * post_clusteringW15_obs$sigma_e4
      p2 <- (1 - NS) * post_clusteringW15_obs$sigma_e4
      
      
      rbeta(length(NS), p1, p2)
    }
  } 

length(counterfact_clustering_JUAN(min))

plot(density(counterfact_clustering_JUAN(min, mu = F)))
lines(density(counterfact_clustering_JUAN(mean, mu = F)), col = 'blue')
lines(density(counterfact_clustering_JUAN(max, mu = F)), col = 'red')

clustering_counterFAC_contrast_JUAN_SWARM <- 
  tibble(mu_val = c(counterfact_clustering_JUAN(max, F) - 
                      counterfact_clustering_JUAN(min, F)), 
         Intervention = 'Causal effect of  \n arthropod biomass')

#plot_contrast_net_sawrm_JUAN <- 
ggplot() +
  geom_boxplot(data = clustering_counterFAC_contrast_JUAN_SWARM, 
               aes(y = mu_val, x = Intervention), 
               alpha = 0.3, color = 'tan1', 
               fill = 'tan1', width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  labs(y = 'Contrast between\n interventions (min - max)', 
       x = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16))

ggsave('causal_eff_clustering_JUAN.jpg', units = 'cm', dpi = 1000, 
       height = 10, width = 8)

# ======= site PLRD =======

for (i in grep('PLRD', names(effects_clustering))) print(effects_clustering[[i]])
# PLRD swarm area --> net size (independent of the window)


# ================== site LIMB =======================


for (i in grep('LIMB', names(effects_clustering))) print(effects_clustering[[i]])
# NO effects



# ======= site SHER =====

for (i in grep('SHER', names(effects_clustering))) print(effects_clustering[[i]])
# effects too variable (most consistent comes from swarm area --> arthropod biomass)


# NOO EFFECTS in this site

# ==== contrast seasons ======

posteriors <- ls()[grep('^(post_clustering)(W[0-9]*)', ls())]

df_seasons <- 
  lapply(posteriors, FUN = 
           function(draws) {
             
             p <- get(draws)
             
             w <- gsub('^(.*)(W[0-9]*)(.*)$', '\\2', draws)
             
             post_season <- 
               p[, colnames(p)[grep('^a_e4', colnames(post_net_sizeW1_obs))]] |> 
               gather() 
             
             post_season <- split(post_season, post_season$key)  
             
             sites <- rep(c('Medium-high rainfall', 
                            'Medium-low rainfall', 
                            'low rainfall', 
                            'High rainfall'), each = 2)
             
             season <- rep(c('Dry', 'Wet'), 4)
             
             for (i in seq_along(sites)) {
               post_season[[i]]$site <- sites[[i]]
               post_season[[i]]$season <- season[[i]]
               post_season[[i]] <- post_season[[i]][, -1]
             }
             
             post_season <- do.call('rbind', post_season)
             post_season <- split(post_season, post_season$site)
             
             post_season <- 
               lapply(post_season, FUN = 
                        function(x) {
                          j <- 
                            inv_logit(x[x$season == 'Dry', ]$value) - 
                            inv_logit(x[x$season == 'Wet', ]$value)
                          
                          tibble(site = x$site[1], 
                                 contrast = j, 
                                 p = mean(j > 0))
                        })
             
             post_season <- do.call('rbind', post_season)
             
             post_season$model <- w
             
             post_season
           })

df_seasons <- do.call('rbind', df_seasons)

df_seasons |> 
  ggplot(aes(contrast, fill = site)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  labs(x = 'Clustering\n (contrast between wet and dry season)') +
  #lims(x = c(-40, 80)) +
  facet_wrap(~model)


# ====== norm_degree (obs) W1 ====

str(network_metrics_obs2)

network_metrics_obs2$norm_degree <- 
  sapply(network_metrics_obs2$norm_degree, FUN = 
           function(x) {
             if(x >= 1) 0.999
             else x
           })
names(network_metrics_obs2)
summary(network_metrics_obs2$norm_degree)

cat(file = 'w1_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W1;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W1[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w1_norm_degree.stan', sep ='')

fit_norm_degreeW1 <- cmdstan_model(file, compile = T)

mod_norm_degreeW1_obs <- 
  fit_norm_degreeW1$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW1_obs$save_object('causal_mod_norm_degreeW1.rds')

mod_norm_degreeW1_obs <- readRDS('causal_mod_norm_degreeW1.rds')
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW1_obs$summary(paste('theta_e4[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW1_obs$summary(paste('theta_e2[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW1_obs$summary(paste('theta_e3[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130)


png('w1_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_norm_degreeW1_obs, 
                mod_norm_degreeW1_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_norm_degreeW1 <- mod_norm_degreeW1_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_norm_degreeW1 |> print(n = 15)



mod_diagnostics(mod_norm_degreeW1_obs, summary_norm_degreeW1)


post_norm_degreeW1_obs <- mod_norm_degreeW1_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_norm_degreeW1_obs[, grep('swarm_area', colnames(post_norm_degreeW1_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW1_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW1_obs))], 
        2, mean)

ppcheck_swarmA <- mod_norm_degreeW1_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW1_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW1_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW1_obs$draws('ppcheck_e1', format = 'matrix')

plot(density(network_metrics_obs2[-3]$cum_rainfall_W1), 
     main = '', xlab = 'Cum rainfall W1')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W1), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red')

dim(post_norm_degreeW1_obs)


post_beta_norm_degreeW1 <- gather(post_norm_degreeW1_obs[, grep('beta', colnames(post_norm_degreeW1_obs))])

post_beta_norm_degreeW1$key <- as.factor(post_beta_norm_degreeW1$key)

post_beta_norm_degreeW1 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== norm_degree (obs) W5 ====


cat(file = 'w5_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W5[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                   beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                   theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                   psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w5_norm_degree.stan', sep ='')

fit_norm_degreeW5 <- cmdstan_model(file, compile = T)

mod_norm_degreeW5_obs <- 
  fit_norm_degreeW5$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW5_obs$save_object('causal_mod_norm_degreeW5.rds')

mod_norm_degreeW5_obs <- readRDS('causal_mod_norm_degreeW5.rds')

# mod_norm_degreeW5_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW5_obs$summary(paste('theta_e4[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW5_obs$summary(paste('theta_e2[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW5_obs$summary(paste('theta_e3[', 
#                                    1:network_metrics_obs2[-3]$N_day, ']', 
#                                    sep = '')) |> print(n = 130)


png('W5_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(paste('theta_e4[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(paste('theta_e3[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))

mod_diagnostics(mod_norm_degreeW5_obs, 
                mod_norm_degreeW5_obs$summary(paste('theta_e2[', 
                                                    1:network_metrics_obs2[-3]$N_day, ']', 
                                                    sep = '')))
dev.off()



summary_norm_degreeW5 <- mod_norm_degreeW5_obs$summary(c('beta_rainW2_e1', 
                                                         'beta_rainW1_e2',
                                                         'beta_rainW1_e3',
                                                         'beta_rainW1_e4',
                                                         'beta_sawrA_e3',
                                                         'beta_sawrA_e4',
                                                         'beta_swarmB_e4', 
                                                         'a_e1', 'a_e2', 
                                                         'a_e3', 'a_e4',
                                                         'sigma_e1', 'sigma_e2', 
                                                         'sigma_e3', 'sigma_e4', 
                                                         'swarm_area',
                                                         'SBiomass_merge',
                                                         'lp__'))

summary_norm_degreeW5 |> print(n = 15)



mod_diagnostics(mod_norm_degreeW5_obs, summary_norm_degreeW5)


post_norm_degreeW5_obs <- mod_norm_degreeW5_obs$draws(c('beta_rainW2_e1', 
                                                        'beta_rainW1_e2',
                                                        'beta_rainW1_e3',
                                                        'beta_rainW1_e4',
                                                        'beta_sawrA_e3',
                                                        'beta_sawrA_e4',
                                                        'beta_swarmB_e4', 
                                                        'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                        'sigma_e1', 'sigma_e2', 
                                                        'sigma_e3', 'sigma_e4',
                                                        'swarm_area',
                                                        'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_norm_degreeW5_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW5_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW5_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW5_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_norm_degreeW5_obs[, grep('swarm_area', colnames(post_norm_degreeW5_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW5_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW5_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W5), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W5), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red')

dim(post_norm_degreeW5_obs)


post_beta_norm_degreeW5 <- gather(post_norm_degreeW5_obs[, grep('beta', colnames(post_norm_degreeW5_obs))])

post_beta_norm_degreeW5$key <- as.factor(post_beta_norm_degreeW5$key)

post_beta_norm_degreeW5 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== norm_degree (obs) W10 ====


cat(file = 'w10_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W10[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w10_norm_degree.stan', sep ='')

fit_norm_degreeW10 <- cmdstan_model(file, compile = T)

mod_norm_degreeW10_obs <- 
  fit_norm_degreeW10$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW10_obs$save_object('causal_mod_norm_degreeW10.rds')

mod_norm_degreeW10_obs <- readRDS('causal_mod_norm_degreeW10.rds')

# mod_norm_degreeW10_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
mod_norm_degreeW10_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2[-3]$site,
               ',',
               network_metrics_obs2[-3]$month, ']',
               sep = ''))) |> print(n = 400)
# 
# mod_norm_degreeW10_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW10_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW10_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW10_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW10_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW10_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW10_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)


png('W10_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(paste('theta_e4[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(paste('theta_e3[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))

mod_diagnostics(mod_norm_degreeW10_obs, 
                mod_norm_degreeW10_obs$summary(paste('theta_e2[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
dev.off()



summary_norm_degreeW10 <- mod_norm_degreeW10_obs$summary(c('beta_rainW2_e1', 
                                                           'beta_rainW1_e2',
                                                           'beta_rainW1_e3',
                                                           'beta_rainW1_e4',
                                                           'beta_sawrA_e3',
                                                           'beta_sawrA_e4',
                                                           'beta_swarmB_e4', 
                                                           'a_e1', 'a_e2', 
                                                           'a_e3', 'a_e4',
                                                           'sigma_e1', 'sigma_e2', 
                                                           'sigma_e3', 'sigma_e4', 
                                                           'swarm_area',
                                                           'SBiomass_merge',
                                                           'lp__'))

summary_norm_degreeW10 |> print(n = 15)



mod_diagnostics(mod_norm_degreeW10_obs, summary_norm_degreeW10)


post_norm_degreeW10_obs <- mod_norm_degreeW10_obs$draws(c('beta_rainW2_e1', 
                                                          'beta_rainW1_e2',
                                                          'beta_rainW1_e3',
                                                          'beta_rainW1_e4',
                                                          'beta_sawrA_e3',
                                                          'beta_sawrA_e4',
                                                          'beta_swarmB_e4', 
                                                          'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                          'sigma_e1', 'sigma_e2', 
                                                          'sigma_e3', 'sigma_e4',
                                                          'swarm_area',
                                                          'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_norm_degreeW10_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW10_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW10_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW10_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_norm_degreeW10_obs[, grep('swarm_area', colnames(post_norm_degreeW10_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW10_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW10_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W10), 
     main = '', xlab = 'Cum rainfall W10')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W10), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red')

dim(post_norm_degreeW10_obs)


post_beta_norm_degreeW10 <- gather(post_norm_degreeW10_obs[, grep('beta', colnames(post_norm_degreeW10_obs))])

post_beta_norm_degreeW10$key <- as.factor(post_beta_norm_degreeW10$key)


post_beta_norm_degreeW10 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== norm_degree (obs) W15 ====


cat(file = 'w15_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W15[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    
    
    ")

file <- paste(getwd(), '/w15_norm_degree.stan', sep ='')

fit_norm_degreeW15 <- cmdstan_model(file, compile = T)

mod_norm_degreeW15_obs <- 
  fit_norm_degreeW15$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW15_obs$save_object('causal_mod_norm_degreeW15.rds')

mod_norm_degreeW15_obs <- readRDS('causal_mod_norm_degreeW15.rds')
# 
# mod_norm_degreeW15_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
mod_norm_degreeW15_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2[-3]$site,
               ',',
               network_metrics_obs2[-3]$month, ']',
               sep = ''))) |> print(n = 400)
# 
# mod_norm_degreeW15_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW15_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW15_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW15_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW15_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW15_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW15_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)
# 

png('W15_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(paste('theta_e4[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(paste('theta_e3[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))

mod_diagnostics(mod_norm_degreeW15_obs, 
                mod_norm_degreeW15_obs$summary(paste('theta_e2[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
dev.off()



summary_norm_degreeW15 <- mod_norm_degreeW15_obs$summary(c('beta_rainW2_e1', 
                                                           'beta_rainW1_e2',
                                                           'beta_rainW1_e3',
                                                           'beta_rainW1_e4',
                                                           'beta_sawrA_e3',
                                                           'beta_sawrA_e4',
                                                           'beta_swarmB_e4', 
                                                           'a_e1', 'a_e2', 
                                                           'a_e3', 'a_e4',
                                                           'sigma_e1', 'sigma_e2', 
                                                           'sigma_e3', 'sigma_e4', 
                                                           'swarm_area',
                                                           'SBiomass_merge',
                                                           'lp__'))

summary_norm_degreeW15 |> print(n = 500)



mod_diagnostics(mod_norm_degreeW15_obs, summary_norm_degreeW15)


post_norm_degreeW15_obs <- mod_norm_degreeW15_obs$draws(c('beta_rainW2_e1', 
                                                          'beta_rainW1_e2',
                                                          'beta_rainW1_e3',
                                                          'beta_rainW1_e4',
                                                          'beta_sawrA_e3',
                                                          'beta_sawrA_e4',
                                                          'beta_swarmB_e4', 
                                                          'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                          'sigma_e1', 'sigma_e2', 
                                                          'sigma_e3', 'sigma_e4',
                                                          'swarm_area',
                                                          'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_norm_degreeW15_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW15_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW15_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW15_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_norm_degreeW15_obs[, grep('swarm_area', colnames(post_norm_degreeW15_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW15_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW15_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W15), 
     main = '', xlab = 'Cum rainfall W15')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W15), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red')

dim(post_norm_degreeW15_obs)


post_beta_norm_degreeW15 <- gather(post_norm_degreeW15_obs[, grep('beta', colnames(post_norm_degreeW15_obs))])

post_beta_norm_degreeW15$key <- as.factor(post_beta_norm_degreeW15$key)


post_beta_norm_degreeW15 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== norm_degree (obs) W20 ====


cat(file = 'w20_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W20[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w20_norm_degree.stan', sep ='')

fit_norm_degreeW20 <- cmdstan_model(file, compile = T)

mod_norm_degreeW20_obs <- 
  fit_norm_degreeW20$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW20_obs$save_object('causal_mod_norm_degreeW20.rds')

mod_norm_degreeW20_obs <- readRDS('causal_mod_norm_degreeW20.rds')
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW20_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW20_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW20_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)


png('W20_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(paste('theta_e4[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(paste('theta_e3[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))

mod_diagnostics(mod_norm_degreeW20_obs, 
                mod_norm_degreeW20_obs$summary(paste('theta_e2[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
dev.off()



summary_norm_degreeW20 <- mod_norm_degreeW20_obs$summary(c('beta_rainW2_e1', 
                                                           'beta_rainW1_e2',
                                                           'beta_rainW1_e3',
                                                           'beta_rainW1_e4',
                                                           'beta_sawrA_e3',
                                                           'beta_sawrA_e4',
                                                           'beta_swarmB_e4', 
                                                           'a_e1', 'a_e2', 
                                                           'a_e3', 'a_e4',
                                                           'sigma_e1', 'sigma_e2', 
                                                           'sigma_e3', 'sigma_e4', 
                                                           'swarm_area',
                                                           'SBiomass_merge',
                                                           'lp__'))

summary_norm_degreeW20 |> print(n = 15)



mod_diagnostics(mod_norm_degreeW20_obs, summary_norm_degreeW20)


post_norm_degreeW20_obs <- mod_norm_degreeW20_obs$draws(c('beta_rainW2_e1', 
                                                          'beta_rainW1_e2',
                                                          'beta_rainW1_e3',
                                                          'beta_rainW1_e4',
                                                          'beta_sawrA_e3',
                                                          'beta_sawrA_e4',
                                                          'beta_swarmB_e4', 
                                                          'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                          'sigma_e1', 'sigma_e2', 
                                                          'sigma_e3', 'sigma_e4',
                                                          'swarm_area',
                                                          'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_norm_degreeW20_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW20_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW20_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW20_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_norm_degreeW20_obs[, grep('swarm_area', colnames(post_norm_degreeW20_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW20_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW20_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W20), 
     main = '', xlab = 'Cum rainfall W20')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W20), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red')

dim(post_norm_degreeW20_obs)



post_beta_norm_degreeW20 <- gather(post_norm_degreeW20_obs[, grep('beta', colnames(post_norm_degreeW20_obs))])

post_beta_norm_degreeW20$key <- as.factor(post_beta_norm_degreeW20$key)

post_beta_norm_degreeW20 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== norm_degree (obs) W25 ====


cat(file = 'w25_norm_degree.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector<lower=0,upper=1>[N] norm_degree;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W30;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      vector[N] mu;
      vector[N] p1;
      vector[N] p2;
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W25[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         mu[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu * sigma_e4;
      p2 = (1 - mu) * sigma_e4;
    
      norm_degree ~ beta(p1, p2);
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      vector[N] p1;
      vector[N] p2;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = inv_logit(a_e4[site[i], season[i]] + 
                           beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                           beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                           beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                           theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                           psi_e4[site[i], month[i]]);
      }
    
      p1 = mu_e4 * sigma_e4;
      p2 = (1 - mu_e4) * sigma_e4;
    
      ppcheck_e4 = beta_rng(p1, p2);
    
    }
    
    ")

file <- paste(getwd(), '/w25_norm_degree.stan', sep ='')

fit_norm_degreeW25 <- cmdstan_model(file, compile = T)

mod_norm_degreeW25_obs <- 
  fit_norm_degreeW25$sample(
    data = network_metrics_obs2[-3], 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_norm_degreeW25_obs$save_object('causal_mod_norm_degreeW25.rds')

mod_norm_degreeW25_obs <- readRDS('causal_mod_norm_degreeW25.rds')
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('psi_e2[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('psi_e3[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('psi_e4[', 
#                network_metrics_obs2[-3]$site,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('tau_e4[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('tau_e2[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(
#   unique(paste('tau_e3[', 
#                network_metrics_obs2[-3]$year,
#                ',',
#                network_metrics_obs2[-3]$month, ']', 
#                sep = ''))) |> print(n = 400) 
# 
# mod_norm_degreeW25_obs$summary(paste('theta_e4[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 200) 
# 
# mod_norm_degreeW25_obs$summary(paste('theta_e2[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130) # ajustar este parametro
# 
# mod_norm_degreeW25_obs$summary(paste('theta_e3[', 
#                                     1:network_metrics_obs2[-3]$N_day, ']', 
#                                     sep = '')) |> print(n = 130)
# 

png('W25_norm_degree_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2[-3]$site,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2[-3]$year,
                               ',',
                               network_metrics_obs2[-3]$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(paste('theta_e4[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(paste('theta_e3[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))

mod_diagnostics(mod_norm_degreeW25_obs, 
                mod_norm_degreeW25_obs$summary(paste('theta_e2[', 
                                                     1:network_metrics_obs2[-3]$N_day, ']', 
                                                     sep = '')))
dev.off()



summary_norm_degreeW25 <- mod_norm_degreeW25_obs$summary(c('beta_rainW2_e1', 
                                                           'beta_rainW1_e2',
                                                           'beta_rainW1_e3',
                                                           'beta_rainW1_e4',
                                                           'beta_sawrA_e3',
                                                           'beta_sawrA_e4',
                                                           'beta_swarmB_e4', 
                                                           'a_e1', 'a_e2', 
                                                           'a_e3', 'a_e4',
                                                           'sigma_e1', 'sigma_e2', 
                                                           'sigma_e3', 'sigma_e4', 
                                                           'swarm_area',
                                                           'SBiomass_merge',
                                                           'lp__'))

summary_norm_degreeW25 |> print(n = 15)



mod_diagnostics(mod_norm_degreeW25_obs, summary_norm_degreeW25)


post_norm_degreeW25_obs <- mod_norm_degreeW25_obs$draws(c('beta_rainW2_e1', 
                                                          'beta_rainW1_e2',
                                                          'beta_rainW1_e3',
                                                          'beta_rainW1_e4',
                                                          'beta_sawrA_e3',
                                                          'beta_sawrA_e4',
                                                          'beta_swarmB_e4', 
                                                          'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                          'sigma_e1', 'sigma_e2', 
                                                          'sigma_e3', 'sigma_e4',
                                                          'swarm_area',
                                                          'SBiomass_merge'
), 
format = 'df')

ppcheck_swarmA <- mod_norm_degreeW25_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_norm_degreeW25_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_norm_degree <- mod_norm_degreeW25_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_norm_degreeW25_obs$draws('ppcheck_e1', format = 'matrix')

swarm_area <- apply(post_norm_degreeW25_obs[, grep('swarm_area', colnames(post_norm_degreeW25_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_norm_degreeW25_obs[, grep('SBiomass_merge', colnames(post_norm_degreeW25_obs))], 
        2, mean)

plot(density(network_metrics_obs2[-3]$cum_rainfall_W25), 
     main = '', xlab = 'Cum rainfall W25')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$cum_rainfall_W25), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:100) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2[-3]$norm_degree), 
     main = '', xlab = 'Net size')
for(i in 1:100) lines(density(ppcheck_norm_degree[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2[-3]$norm_degree), col = 'red') 

dim(post_norm_degreeW25_obs)

post_beta_norm_degreeW25 <- gather(post_norm_degreeW25_obs[, grep('beta', colnames(post_norm_degreeW25_obs))])

post_beta_norm_degreeW25$key <- as.factor(post_beta_norm_degreeW25$key)

post_beta_norm_degreeW20 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== plotting and counterfactual norm degree ====

# ====== forest plot and conditional effects ====

t <- paste('W', c(1, seq(5, 25, by = 5)), sep = '')

plot_beta_norm_degree <- vector('list', length(t))

for (i in seq_along(t)) {
  cadena <- paste('^(.*)beta_norm_degree(.*)', t[i], '$', sep = '')
  cadena2 <- paste('post_norm_degree', t[i], '_obs', sep = '')
  df <- get(ls()[grepl(cadena, ls())])
  temp <- get(ls()[grepl(cadena2, ls())])
  
  temp <- temp[, grep('^beta', colnames(temp))]
  
  temp <- apply(temp, 2, function(x) mean(x > 0))
  
  temp <- tibble(p = temp, key = names(temp))
  
  df <- 
    df |> 
    group_by(key) |> 
    transmute(li = quantile(value, 0.025), 
              ls = quantile(value, 0.975), 
              mu = mean(value), 
              window = t[i]) |> 
    unique()
  
  n <- c(1, seq(5, 25, by = 5))[i]
  
  df <- df[!grepl(n + 4, df$key), ]
  
  df <- full_join(df, temp, by = 'key')
  
  plot_beta_norm_degree[[i]] <- df
  print(i)
}

plot_beta_norm_degree <- do.call('rbind', plot_beta_norm_degree)

plot_beta_norm_degree <- plot_beta_norm_degree[!grepl('W2', plot_beta_norm_degree$key),]

plot_beta_norm_degree$window <- as.factor(plot_beta_norm_degree$window)

plot_beta_norm_degree$season <- 
  ifelse(grepl('^(.*)(..,1.)$', plot_beta_norm_degree$key), 'Dry', 'Wet')

plot_beta_norm_degree$site <- 
  ifelse(grepl('^(.*)(.1,..)$', plot_beta_norm_degree$key), '1', 
         ifelse(grepl('^(.*)(.2,..)$', plot_beta_norm_degree$key), '2', 
                ifelse(grepl('^(.*)(.3,..)$', plot_beta_norm_degree$key), '3', '4')))

plot_beta_norm_degree$site <- as.factor(plot_beta_norm_degree$site)

plot_beta_norm_degree$site <- 
  factor(plot_beta_norm_degree$site, 
         labels = levels(network_metrics_INDX$obs_all$site))

plot_beta_norm_degree$rainfall_level <- 
  factor(plot_beta_norm_degree$site, 
         labels = c('Medium-high rainfall', 'Medium-low rainfall', 
                    'Low rainfall', 'High rainfall'))

plot_beta_norm_degree$rainfall_level <- 
  factor(plot_beta_norm_degree$rainfall_level, 
         levels = c('Low rainfall', 'Medium-low rainfall', 
                    'Medium-high rainfall', 'High rainfall'))


plot_beta_norm_degree$key <- gsub('^(.*)(.....)$', '\\1', plot_beta_norm_degree$key)

plot_beta_norm_degreeWET <- plot_beta_norm_degree[plot_beta_norm_degree$season == 'Wet',]

plot_beta_norm_degreeWET$key <- factor(plot_beta_norm_degreeWET$key) 

plot_beta_norm_degreeWET$code <- 
  plot_beta_norm_degreeWET %$% paste(key, window, rainfall_level, sep = '_')

sum(plot_beta_norm_degreeWET$code == "beta_rainW1_e2_W15_Medium-low rainfall" |
      plot_beta_norm_degreeWET$code == "beta_sawrA_e4_W15_Medium-low rainfall")

levels(plot_beta_norm_degreeWET$key)

levels(plot_beta_norm_degreeWET$rainfall_level)
levels(plot_beta_norm_degreeWET$site)

plot_beta_norm_degreeWET$key <- 
  factor(plot_beta_norm_degreeWET$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

#plot_effects_norm_degreeWET2 <- 
plot_beta_norm_degreeWET |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Norm. degree'),
                              expression('Rainfall' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    title = element_text(size = 13),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x.top = element_text(size = 12.5), 
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    text = element_text(family = 'Times New Roman')
  )

ggsave('wet_season_norm_degree.jpg', units = 'cm', dpi = 1500,
       height = 25, width = 11)


plot_effects_norm_degreeWET <- 
  plot_beta_norm_degreeWET |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Norm. degree'),
                              expression('Rainfall' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = 'Effect', title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman')
  )


plot_beta_norm_degreeDRY <- plot_beta_norm_degree[plot_beta_norm_degree$season == 'Dry',]

plot_beta_norm_degreeDRY$key <- factor(plot_beta_norm_degreeDRY$key)

levels(plot_beta_norm_degreeDRY$key)

levels(plot_beta_norm_degreeDRY$site)

plot_beta_norm_degreeDRY$key <- 
  factor(plot_beta_norm_degreeDRY$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

plot_effects_norm_degreeDRY <- 
  plot_beta_norm_degreeDRY |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Norm. degree'),
                              expression('Rainfall' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Dry season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.10), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman'), 
    axis.text.y = element_blank()
  )

plot_effects_norm_degreeWET | plot_effects_norm_degreeDRY +
  plot_layout(ncol = 1)

ggsave('netwrok_size_effect.jpg', units = 'cm', 
       height = 20, width = 18, dpi = 500)

plot_effects_norm_degreeDRY 

plot_effects_norm_degreeWET2



# ======= conditional effects ======

s <- network_metrics_INDX$obs_all

s$season <- 
  ifelse(as.numeric(as.character(s$month)) <= 4, 1, 2)

s <- 
  s %$% 
  aggregate(norm_degree ~ season + site, FUN = length)

s

s2 <- network_metrics_INDX$obs_all
s2$obs_ID <- as.factor(s2$obs_ID)

unique(tibble(x = network_metrics_INDX$obs_all$site, 
              x1 = as.numeric(network_metrics_INDX$obs_all$site)))

plot_beta_norm_degree[plot_beta_norm_degree$p >= 0.9 |
                        plot_beta_norm_degree$p <= 0.1, ] |> print(n = 100)

effects_norm_degree <- 
  plot_beta_norm_degree[plot_beta_norm_degree$p >= 0.9 |
                          plot_beta_norm_degree$p <= 0.1, ]

effects_norm_degree <- split(effects_norm_degree, 
                             list(effects_norm_degree$window, 
                                  effects_norm_degree$season, 
                                  effects_norm_degree$site))

effects_norm_degree <- effects_norm_degree[unlist(lapply(effects_norm_degree, function(x) nrow(x) > 0), 
                                                  use.names = F)]

# ==== site JUAN ====
for (i in grep('JUAN', names(effects_norm_degree))) print(effects_norm_degree[[i]])
# JUAN arthropod biomass --> net size (independent of the window)
# No relevant effects

swarm_area <- 
  sapply(ls()[grep('post_norm_degree', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_norm_degree', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_obs2$site, 
         day = network_metrics_obs2$day, 
         month = network_metrics_obs2$month, 
         season = network_metrics_obs2$season)

day <- unique(sites[, c('day', 'site', 'month', 'season')])

day <- day[day$site == 1 & 
             day$season == 1, ]

day2 <- unique(day[, -1])

psi_e4W15MHR <- 
  mod_norm_degreeW15_obs$draws(
    paste('psi_e4[',day2$site, ',', day2$month,']', sep = ''), 
                               format = 'matrix') |> 
  apply(1, mean)

tau_e4W15MHR <- 
  mod_norm_degreeW15_obs$draws(
    paste('tau_e4[', 2, ',', day2$month,']', sep = ''), 
    format = 'matrix') |> 
  apply(1, mean)


theta_e4W15MHR <- 
  mod_norm_degreeW15_obs$draws(
    paste('theta_e4[', day$day, ']', sep = ''), 
    format = 'matrix') |> 
  apply(1, mean)

x_W15 <- seq(min(swarm_BM), max(swarm_BM), length.out = 1000)

est_counterSB <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             s_BM <- 
               post_norm_degreeW15_obs$`a_e4[1,1]` + 
               post_norm_degreeW15_obs$`beta_rainW1_e4[1,1]`*0 +
               post_norm_degreeW15_obs$`beta_sawrA_e4[1,1]`*mean(swarm_area) +
               post_norm_degreeW15_obs$`beta_swarmB_e4[1,1]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             s_BM <- inv_logit(s_BM)
             
             p1 <- s_BM * post_norm_degreeW15_obs$sigma_e4;
             p2 <- (1 - s_BM) * post_norm_degreeW15_obs$sigma_e4;
             
             rbeta(1e3, p1, p2)
             
           })


est_counterSB_MU <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             s_BM <- 
               post_norm_degreeW15_obs$`a_e4[1,1]` + 
               post_norm_degreeW15_obs$`beta_rainW1_e4[1,1]`*0 +
               post_norm_degreeW15_obs$`beta_sawrA_e4[1,1]`*mean(swarm_area) +
               post_norm_degreeW15_obs$`beta_swarmB_e4[1,1]`*x +
               psi_e4W15MHR + theta_e4W15MHR + tau_e4W15MHR
             
             inv_logit(s_BM)
             
           })

est_counterSB <- 
  do.call('rbind', 
          apply(est_counterSB, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterSB$x <- x_W15
est_counterSB$season <- 'Dry season'
est_counterSB$mu <- apply(est_counterSB_MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_obs2$season, 
              site = network_metrics_obs2$site, 
              norm_degree = network_metrics_obs2$norm_degree)

filt <- quantile(dat[dat$site == 1 & dat$season == 1,]$y)[c(1, 5)]


#plot_swarm_biomas_ND_JUAN <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 1 & dat$season == 1,],
    aes(y, norm_degree), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterSB[est_counterSB$x >= filt[1] &
                           est_counterSB$x <= filt[2],], 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tan1'
  ) +
  geom_line(
    data = est_counterSB[est_counterSB$x >= filt[1] &
                           est_counterSB$x <= filt[2], ], 
    aes(x, mu), linewidth = 1, 
    color = 'tan1'
  ) +
  labs(x = 'Swarm area', 
       y = 'Normalized degree') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 15))

plot_swarm_biomas_ND_JUAN

ggsave('JUAN_wet_norm_degree.jpg', units = 'cm', dpi = 1000, 
       height = 7, width = 14)

# ======= site PLRD =======

for (i in grep('PLRD', names(effects_norm_degree))) print(effects_norm_degree[[i]])
# No vonsistent effects

swarm_area <- 
  sapply(ls()[grep('post_norm_degree', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_norm_degree', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_obs2$site, 
         day = network_metrics_obs2$day, 
         month = network_metrics_obs2$month, 
         season = network_metrics_obs2$season)

day <- unique(sites[sites$site == 3 & sites$season == 2, ]$day)
month <- unique(sites[sites$site == 3 & sites$season == 2, ]$month)

day <- sites[, c('site', 'month')]
month <- sites[, c('season', 'month')]

unique(day[day$site == 3, ])

psi_e3W15LR <- 
  mod_norm_degreeW15_obs$draws('psi_e3', format = 'matrix') |> 
  apply(1, mean)


tau_e3W15LR <- 
  mod_norm_degreeW15_obs$draws('tau_e3', format = 'matrix') |> 
  apply(1, mean)

theta_e3W15LR <- 
  mod_norm_degreeW15_obs$draws('theta_e3', format = 'matrix') |> 
  apply(1, mean)

psi_e4W15LR <- 
  mod_norm_degreeW15_obs$draws('psi_e4', format = 'matrix') |> 
  apply(1, mean)


tau_e4W15LR <- 
  mod_norm_degreeW15_obs$draws('tau_e4', format = 'matrix') |> 
  apply(1, mean)

theta_e4W15LR <- 
  mod_norm_degreeW15_obs$draws('theta_e4', format = 'matrix') |> 
  apply(1, mean)

x_W15 <- seq(min(swarm_area), max(swarm_area), length.out = 1000)

est_counter_AB1 <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             NS <- 
               post_norm_degreeW15_obs$`a_e3[3,2]` + 
               post_norm_degreeW15_obs$`beta_rainW1_e3[3,2]`*0 +
               post_norm_degreeW15_obs$`beta_sawrA_e3[3,2]`*x +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
             rstudent(length(NS), mu = NS, sigma = post_norm_degreeW15_obs$sigma_e3)
             
           })


est_counterAB1_MU <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             post_norm_degreeW15_obs$`a_e3[3,2]` + 
               post_norm_degreeW15_obs$`beta_rainW1_e3[3,2]`*0 +
               post_norm_degreeW15_obs$`beta_sawrA_e3[3,2]`*x +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
           })

est_counter_AB1 <- 
  do.call('rbind', 
          apply(est_counter_AB1, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counter_AB1$x <- x_W15
est_counter_AB1$season <- 'Wet season'
est_counter_AB1$mu <- apply(est_counterAB1_MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_obs2$season, 
              site = network_metrics_obs2$site, 
              norm_degree = network_metrics_obs2$norm_degree, 
              rain = network_metrics_obs2$cum_rainfall_W15)



#plot_NS_PLR <- 
ggplot() +
  geom_point(
    data = dat[dat$site == 3 & dat$season == 2,],
    aes(x, y), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counter_AB1, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tomato3'
  ) +
  geom_line(
    data = est_counter_AB1, aes(x, mu), linewidth = 1, 
    color = 'tomato3'
  ) +
  labs(x = 'Swarm area', 
       y = 'Arthropod biomas') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 16))

plot_NS_PLR

ggsave('PRL_wet_norm_degree.jpg', units = 'cm', dpi = 1000, 
       height = 7, width = 7)

# ================== site LIMB =======================

for (i in grep('LIMB', names(effects_norm_degree))) print(effects_norm_degree[[i]])

# No effects




# ====== site SHER ====

for (i in grep('SHER', names(effects_norm_degree))) print(effects_norm_degree[[i]])
# No effects 


# ==== contrast seasons ======

posteriors <- ls()[grep('^(post_norm_degree)(W[0-9]*)', ls())]

df_seasons <- 
  lapply(posteriors, FUN = 
           function(draws) {
             
             p <- get(draws)
             
             w <- gsub('^(.*)(W[0-9]*)(.*)$', '\\2', draws)
             
             post_season <- 
               p[, colnames(p)[grep('^a_e4', colnames(post_net_sizeW1_obs))]] |> 
               gather() 
             
             post_season <- split(post_season, post_season$key)  
             
             sites <- rep(c('Medium-high rainfall', 
                            'Medium-low rainfall', 
                            'low rainfall', 
                            'High rainfall'), each = 2)
             
             season <- rep(c('Dry', 'Wet'), 4)
             
             for (i in seq_along(sites)) {
               post_season[[i]]$site <- sites[[i]]
               post_season[[i]]$season <- season[[i]]
               post_season[[i]] <- post_season[[i]][, -1]
             }
             
             post_season <- do.call('rbind', post_season)
             post_season <- split(post_season, post_season$site)
             
             post_season <- 
               lapply(post_season, FUN = 
                        function(x) {
                          j <- 
                            inv_logit(x[x$season == 'Dry', ]$value) - 
                            inv_logit(x[x$season == 'Wet', ]$value)
                          
                          tibble(site = x$site[1], 
                                 contrast = j, 
                                 p = mean(j > 0))
                        })
             
             post_season <- do.call('rbind', post_season)
             
             post_season$model <- w
             
             post_season
           })

df_seasons <- do.call('rbind', df_seasons)

df_seasons |> 
  ggplot(aes(contrast, fill = site)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  labs(x = 'Normalized degree\n (contrast between wet and dry season)') +
  #lims(x = c(-40, 80)) +
  facet_wrap(~model)





# NOO EFFECTS in this site
# ====== skewness (obs) W1 ====

network_metrics_obs2.1 <- network_metrics_INDX$obs_all

network_metrics_obs2.1 <- 
  network_metrics_obs2.1[!is.na(network_metrics_obs2.1$skewness), ]

network_metrics_obs2.1$season <- 
  ifelse(as.numeric(as.character(network_metrics_obs2.1$month)) <= 4, 1, 2)

network_metrics_obs2.1 %$% 
  aggregate(net_size ~ season + site, FUN = length) ### ok it could work 

network_metrics_obs2.1$month <- 
  as.numeric(as.character(network_metrics_obs2.1$month))

network_metrics_obs2.1[] <- 
  lapply(network_metrics_obs2.1, function(x) if(is.factor(x)) factor(x) else(x))

day_indx_skewness <- 
  colnames(subset_day_matrix) %in% levels(network_metrics_obs2.1$date_fct)

subset_day_matrix2.1 <- subset_day_matrix[day_indx_skewness, day_indx_skewness]

network_metrics_obs2.1 <- lapply(network_metrics_obs2.1, FUN =
                                   function(i) {
                                     if (is.factor(i)) as.numeric(i)
                                     else i
                                   })

max(network_metrics_obs2.1$date_fct); dim(subset_day_matrix2.1)

cols <- grep('^cum_', names(network_metrics_obs2.1))

for (i in cols) network_metrics_obs2.1[[i]] <- 
  as.vector(scale(network_metrics_obs2.1[[i]]))

network_metrics_obs2.1$dist_day <- subset_day_matrix2.1
network_metrics_obs2.1$N <- length(network_metrics_obs2.1$date_fct)
network_metrics_obs2.1$N_day <- max(network_metrics_obs2.1$date_fct)
network_metrics_obs2.1$N_month <- max(network_metrics_obs2.1$month)
network_metrics_obs2.1$N_season <- max(network_metrics_obs2.1$season)
network_metrics_obs2.1$N_site <- max(network_metrics_obs2.1$site)
network_metrics_obs2.1$N_dim_day <- dim(subset_day_matrix2.1)[1]

network_metrics_obs2.1$N_naSwarmBio <- 
  sum(is.na(network_metrics_obs2.1$swarm_biomass))

network_metrics_obs2.1$SwarmB_missindx <- 
  which(is.na(network_metrics_obs2.1$swarm_biomass))

network_metrics_obs2.1$N_naSwarmINI <- 
  sum(is.na(network_metrics_obs2.1$swarm_area_inital))

network_metrics_obs2.1$SwarmINI_missindx <- 
  which(is.na(network_metrics_obs2.1$swarm_area_inital))

network_metrics_obs2.1$N_naSwarmFIN <- 
  sum(is.na(network_metrics_obs2.1$swarm_area_final))

network_metrics_obs2.1$SwarmFIN_missindx <- 
  which(is.na(network_metrics_obs2.1$swarm_area_final))

unlist(lapply(network_metrics_obs2.1, function(x) mean(is.na(x))))

names(network_metrics_obs2.1)[grep('date_fct', names(network_metrics_obs2.1))] <- 'day'

network_metrics_obs2.1$swarm_biomass <- 
  sapply(network_metrics_obs2.1$swarm_biomass, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })

network_metrics_obs2.1$swarm_area_final <- 
  sapply(network_metrics_obs2.1$swarm_area_final, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })

network_metrics_obs2.1$swarm_area_inital <- 
  sapply(network_metrics_obs2.1$swarm_area_inital, FUN = 
           function(i) {
             if (is.na(i)) 0
             else log(i)
           })


mean(network_metrics_obs2.1$skewness, na.rm = T)
sd(network_metrics_obs2.1$skewness, na.rm = T)

cat(file = 'w1_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W1;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W1[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W5[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W1[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W1[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W1[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w1_skewness.stan', sep ='')

fit_skewnessW1 <- cmdstan_model(file, compile = T)

mod_skewnessW1_obs <- 
  fit_skewnessW1$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW1_obs$save_object('causal_mod_skewnessW1.rds')

mod_skewnessW1_obs <- readRDS('causal_mod_skewnessW1.rds')

mod_skewnessW1_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW1_obs$summary(paste('theta_e4[',
                                    1:network_metrics_obs2.1$N_day, ']',
                                    sep = '')) |> print(n = 200)

mod_skewnessW1_obs$summary(paste('theta_e2[',
                                    1:network_metrics_obs2.1$N_day, ']',
                                    sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW1_obs$summary(paste('theta_e3[',
                                    1:network_metrics_obs2.1$N_day, ']',
                                    sep = '')) |> print(n = 130)


png('w1_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(paste('theta_e4[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))
mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(paste('theta_e3[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))

mod_diagnostics(mod_skewnessW1_obs, 
                mod_skewnessW1_obs$summary(paste('theta_e2[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))
dev.off()



summary_skewnessW1 <- mod_skewnessW1_obs$summary(c('beta_rainW2_e1', 
                                                   'beta_rainW1_e2',
                                                   'beta_rainW1_e3',
                                                   'beta_rainW1_e4',
                                                   'beta_sawrA_e3',
                                                   'beta_sawrA_e4',
                                                   'beta_swarmB_e4', 
                                                   'a_e1', 'a_e2', 
                                                   'a_e3', 'a_e4',
                                                   'sigma_e1', 'sigma_e2', 
                                                   'sigma_e3', 'sigma_e4', 
                                                   'swarm_area',
                                                   'SBiomass_merge',
                                                   'lp__'))

summary_skewnessW1 |> print(n = 15)



mod_diagnostics(mod_skewnessW1_obs, summary_skewnessW1)


post_skewnessW1_obs <- mod_skewnessW1_obs$draws(c('beta_rainW2_e1', 
                                                  'beta_rainW1_e2',
                                                  'beta_rainW1_e3',
                                                  'beta_rainW1_e4',
                                                  'beta_sawrA_e3',
                                                  'beta_sawrA_e4',
                                                  'beta_swarmB_e4', 
                                                  'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                  'sigma_e1', 'sigma_e2', 
                                                  'sigma_e3', 'sigma_e4',
                                                  'swarm_area',
                                                  'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW1_obs[, grep('swarm_area', colnames(post_skewnessW1_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW1_obs[, grep('SBiomass_merge', colnames(post_skewnessW1_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW1_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW1_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW1_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW1_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(1, 1), mar= c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W1), 
     main = '', xlab = 'Cum rainfall W1')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:500) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)

plot(density(network_metrics_obs2.1$skewness), 
     main = '', xlab = 'skewness')
for(i in 1:100) lines(density(ppcheck_skewness[i, ]), lwd = 0.1)

dim(post_skewnessW1_obs)


post_beta_skewnessW1 <- gather(post_skewnessW1_obs[, grep('beta', colnames(post_skewnessW1_obs))])

post_beta_skewnessW1$key <- as.factor(post_beta_skewnessW1$key)

post_beta_skewnessW1 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== skewness (obs) W5 ====


cat(file = 'w5_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W5;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W5[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W10[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W5[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W5[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W5[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w5_skewness.stan', sep ='')

fit_skewnessW5 <- cmdstan_model(file, compile = T)

mod_skewnessW5_obs <- 
  fit_skewnessW5$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW5_obs$save_object('causal_mod_skewnessW5.rds')

mod_skewnessW5_obs <- readRDS('causal_mod_skewnessW5.rds')

mod_skewnessW5_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW5_obs$summary(paste('theta_e4[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 200)

mod_skewnessW5_obs$summary(paste('theta_e2[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW5_obs$summary(paste('theta_e3[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 130)


png('W5_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(paste('theta_e4[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))
mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(paste('theta_e3[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))

mod_diagnostics(mod_skewnessW5_obs, 
                mod_skewnessW5_obs$summary(paste('theta_e2[', 
                                                 1:network_metrics_obs2.1$N_day, ']', 
                                                 sep = '')))
dev.off()



summary_skewnessW5 <- mod_skewnessW5_obs$summary(c('beta_rainW2_e1', 
                                                   'beta_rainW1_e2',
                                                   'beta_rainW1_e3',
                                                   'beta_rainW1_e4',
                                                   'beta_sawrA_e3',
                                                   'beta_sawrA_e4',
                                                   'beta_swarmB_e4', 
                                                   'a_e1', 'a_e2', 
                                                   'a_e3', 'a_e4',
                                                   'sigma_e1', 'sigma_e2', 
                                                   'sigma_e3', 'sigma_e4', 
                                                   'swarm_area',
                                                   'SBiomass_merge',
                                                   'lp__'))

summary_skewnessW5 |> print(n = 15)



mod_diagnostics(mod_skewnessW5_obs, summary_skewnessW5)


post_skewnessW5_obs <- mod_skewnessW5_obs$draws(c('beta_rainW2_e1', 
                                                  'beta_rainW1_e2',
                                                  'beta_rainW1_e3',
                                                  'beta_rainW1_e4',
                                                  'beta_sawrA_e3',
                                                  'beta_sawrA_e4',
                                                  'beta_swarmB_e4', 
                                                  'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                  'sigma_e1', 'sigma_e2', 
                                                  'sigma_e3', 'sigma_e4',
                                                  'swarm_area',
                                                  'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW5_obs[, grep('swarm_area', colnames(post_skewnessW5_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW5_obs[, grep('SBiomass_merge', colnames(post_skewnessW5_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW5_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW5_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW5_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW5_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(1, 1), mar= c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W5), 
     main = '', xlab = 'Cum rainfall W5')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$cum_rainfall_W5), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')


post_beta_skewnessW5 <- gather(post_skewnessW5_obs[, grep('beta', colnames(post_skewnessW5_obs))])

post_beta_skewnessW5$key <- as.factor(post_beta_skewnessW5$key)

post_beta_skewnessW5 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== skewness (obs) W10 ====


cat(file = 'w10_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W10;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W10[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W15[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W10[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W10[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W10[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w10_skewness.stan', sep ='')

fit_skewnessW10 <- cmdstan_model(file, compile = T)

mod_skewnessW10_obs <- 
  fit_skewnessW10$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW10_obs$save_object('causal_mod_skewnessW10.rds')

mod_skewnessW10_obs <- readRDS('causal_mod_skewnessW10.rds')

mod_skewnessW10_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW10_obs$summary(paste('theta_e4[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 200)

mod_skewnessW10_obs$summary(paste('theta_e2[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW10_obs$summary(paste('theta_e3[',
                                 1:network_metrics_obs2.1$N_day, ']',
                                 sep = '')) |> print(n = 130)


png('W10_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_skewnessW10_obs, 
                mod_skewnessW10_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_skewnessW10 <- mod_skewnessW10_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_skewnessW10 |> print(n = 15)



mod_diagnostics(mod_skewnessW10_obs, summary_skewnessW10)


post_skewnessW10_obs <- mod_skewnessW10_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW10_obs[, grep('swarm_area', colnames(post_skewnessW10_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW10_obs[, grep('SBiomass_merge', colnames(post_skewnessW10_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW10_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW10_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW10_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW10_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(1, 1), mar= c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W10), 
     main = '', xlab = 'Cum rainfall W10')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$cum_rainfall_W10), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:500) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2.1$skewness), 
     main = '', xlab = 'skewness')
for(i in 1:100) lines(density(ppcheck_skewness[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$skewness), col = 'red')

dim(post_skewnessW10_obs)


post_beta_skewnessW10 <- gather(post_skewnessW10_obs[, grep('beta', colnames(post_skewnessW10_obs))])

post_beta_skewnessW10$key <- as.factor(post_beta_skewnessW10$key)


post_beta_skewnessW10 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== skewness (obs) W15 ====


cat(file = 'w15_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W15;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W15[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W20[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W15[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W15[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W15[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w15_skewness.stan', sep ='')

fit_skewnessW15 <- cmdstan_model(file, compile = T)

mod_skewnessW15_obs <- 
  fit_skewnessW15$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW15_obs$save_object('causal_mod_skewnessW15.rds')

mod_skewnessW15_obs <- readRDS('causal_mod_skewnessW15.rds')

mod_skewnessW15_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW15_obs$summary(paste('theta_e4[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 200)

mod_skewnessW15_obs$summary(paste('theta_e2[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW15_obs$summary(paste('theta_e3[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130)


png('W15_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_skewnessW15_obs, 
                mod_skewnessW15_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_skewnessW15 <- mod_skewnessW15_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_skewnessW15 |> print(n = 15)



mod_diagnostics(mod_skewnessW15_obs, summary_skewnessW15)


post_skewnessW15_obs <- mod_skewnessW15_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW15_obs[, grep('swarm_area', colnames(post_skewnessW15_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW15_obs[, grep('SBiomass_merge', colnames(post_skewnessW15_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW15_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW15_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW15_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW15_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(1, 1), mar= c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W15), 
     main = '', xlab = 'Cum rainfall W15')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$cum_rainfall_W15), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), main = '', xlab = 'Swarm area', col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:500) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2.1$skewness), 
     main = '', xlab = 'skewness')
for(i in 1:100) lines(density(ppcheck_skewness[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$skewness), col = 'red')

dim(post_skewnessW15_obs)


post_beta_skewnessW15 <- gather(post_skewnessW15_obs[, grep('beta', colnames(post_skewnessW15_obs))])

post_beta_skewnessW15$key <- as.factor(post_beta_skewnessW15$key)

post_beta_skewnessW15 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== skewness (obs) W20 ====


cat(file = 'w20_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W20;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W20[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W20[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W20[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W20[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w20_skewness.stan', sep ='')

fit_skewnessW20 <- cmdstan_model(file, compile = T)

mod_skewnessW20_obs <- 
  fit_skewnessW20$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW20_obs$save_object('causal_mod_skewnessW20.rds')

mod_skewnessW20_obs <- readRDS('causal_mod_skewnessW20.rds')

mod_skewnessW20_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW20_obs$summary(paste('theta_e4[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 200)

mod_skewnessW20_obs$summary(paste('theta_e2[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW20_obs$summary(paste('theta_e3[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130)


png('W20_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_skewnessW20_obs, 
                mod_skewnessW20_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_skewnessW20 <- mod_skewnessW20_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_skewnessW20 |> print(n = 15)



mod_diagnostics(mod_skewnessW20_obs, summary_skewnessW20)


post_skewnessW20_obs <- mod_skewnessW20_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW20_obs[, grep('swarm_area', colnames(post_skewnessW20_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW20_obs[, grep('SBiomass_merge', colnames(post_skewnessW20_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW20_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW20_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW20_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW20_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(1, 1), mar= c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W20), 
     main = '', xlab = 'Cum rainfall W20')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$cum_rainfall_W20), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:500) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2.1$skewness), 
     main = '', xlab = 'skewness')
for(i in 1:100) lines(density(ppcheck_skewness[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$skewness), col = 'red')

dim(post_skewnessW20_obs)


post_beta_skewnessW20 <- gather(post_skewnessW20_obs[, grep('beta', colnames(post_skewnessW20_obs))])

post_beta_skewnessW20$key <- as.factor(post_beta_skewnessW20$key)


post_beta_skewnessW20 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)


# ====== skewness (obs) W25 ====


cat(file = 'w25_skewness.stan', 
    "
    
    functions {
    
      vector scale(vector x) {
        int N = dims(x)[1];
        real mu = mean(x);
        real sigma = sd(x);
        vector[N] scaled_v;
    
        for (i in 1:N) {
          scaled_v[i] = (x[i] - mu) / sigma;
        }
        return scaled_v;
      }  
    
      vector merge_missing(int[] miss_indxex, vector x_obs, vector x_miss) {
                int N = dims(x_obs)[1];
                int N_miss = dims(x_miss)[1];
                vector[N] merge;
                merge = x_obs;
                for (i in 1:N_miss) {
                    merge[miss_indxex[i]] = x_miss[i];
                }
            return merge;
          }
    
    
      matrix cov_GPL2(matrix x,
                      real eta,
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      }
    }
    
    data {
      int N;
      int N_day;
      int N_month;
      int N_season;
      int N_site;
      int N_dim_day;
      int N_naSwarmBio;
      int N_naSwarmINI;
      int N_naSwarmFIN;
      array[N_naSwarmBio] int SwarmB_missindx;
      array[N_naSwarmINI] int SwarmINI_missindx;
      array[N_naSwarmFIN] int SwarmFIN_missindx;
      vector[N] skewness;
      array[N] int day;
      array[N] int month;
      array[N] int season;
      array[N] int site;
      vector[N] swarm_biomass;
      vector[N] swarm_area_inital;
      vector[N] swarm_area_final;
      array[N] int cum_rainfall_W30;
      array[N] int cum_rainfall_W25;
      matrix[N_dim_day, N_dim_day] dist_day;
    }
    
    parameters {
      /////////////
      ///////////// imputed variables
      vector[N_naSwarmBio] SBiomass_impute;
      real mu_sb;
      real<lower = 0> sigma_sb;
      vector[N_naSwarmINI] SwarmINI_impute;
      real mu_sbINI;
      real<lower = 0> sigma_sbINI;
      vector[N_naSwarmFIN] SwarmFIN_impute;
      real mu_sbFIN;
      real<lower = 0> sigma_sbFIN;
    
      //////////////
      ///////////// rain_i+n -> rain_i
      real a_e1;
      real beta_rainW2_e1;
      real<lower = 0> sigma_e1;
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm
      matrix[N_site, N_season] a_e2;
      matrix[N_site, N_season] beta_rainW1_e2;
      real<lower = 0> sigma_e2;
    
      vector[N_day] z_theta_e2;
      real<lower = 0> eta_theta_e2;
      real<lower = 0> rho_theta_e2;
    
      matrix[N_season, N_month] z_tau_e2;
      real mu_tau_e2;
      real<lower = 0> sigma_tau_e2;
    
      matrix[N_site, N_month] z_psi_e2;
      real mu_psi_e2;
      real<lower = 0> sigma_psi_e2;
      
      ////////////////////
    
      /////////////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      matrix[N_site, N_season] a_e3;
      matrix[N_site, N_season] beta_rainW1_e3;
      matrix[N_site, N_season] beta_sawrA_e3;
      real<lower = 0> sigma_e3;
    
      vector[N_day] z_theta_e3;
      real<lower = 0> eta_theta_e3;
      real<lower = 0> rho_theta_e3;
    
      matrix[N_season, N_month] z_tau_e3;
      real mu_tau_e3;
      real<lower = 0> sigma_tau_e3;
    
      matrix[N_site, N_month] z_psi_e3;
      real mu_psi_e3;
      real<lower = 0> sigma_psi_e3;
      
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      matrix[N_site, N_season] a_e4;
      matrix[N_site, N_season] beta_rainW1_e4;
      matrix[N_site, N_season] beta_sawrA_e4;
      matrix[N_site, N_season] beta_swarmB_e4;
      real<lower = 0> sigma_e4;
    
      vector[N_day] z_theta_e4;
      real<lower = 0> eta_theta_e4;
      real<lower = 0> rho_theta_e4;
    
      matrix[N_season, N_month] z_tau_e4;
      real mu_tau_e4;
      real<lower = 0> sigma_tau_e4;
    
      matrix[N_site, N_month] z_psi_e4;
      real mu_psi_e4;
      real<lower = 0> sigma_psi_e4;
      
      ////////////////////
    
      
    }
    
    transformed parameters{
      
      ///////////////////////
      ///////////////////////// imputed variables
      vector[N] swarm_area;
      vector[N] SBiomass_merge;
      SBiomass_merge = merge_missing(SwarmB_missindx, 
                                     to_vector(swarm_biomass), 
                                     SBiomass_impute);
      vector[N] swarmINI_merge;
      swarmINI_merge = merge_missing(SwarmINI_missindx, 
                                     to_vector(swarm_area_inital), 
                                     SwarmINI_impute);
      vector[N] swarmFIN_merge;
      swarmFIN_merge = merge_missing(SwarmFIN_missindx, 
                                     to_vector(swarm_area_final), 
                                     SwarmFIN_impute);
      swarm_area = (swarmINI_merge + swarmFIN_merge) / 2;
      // swarm_area = scale(swarm_area);
      // SBiomass_merge = scale(SBiomass_merge);
    
      ///////////////////
      ///////////////////////// rain_i -> swarm
      
      vector[N_day] theta_e2;
      matrix[N_dim_day, N_dim_day] sigma_theta_e2;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e2;
      sigma_theta_e2 = cov_GPL2(dist_day, eta_theta_e2, rho_theta_e2, 0.01);
      L_sigma_theta_e2 = cholesky_decompose(sigma_theta_e2);
      theta_e2 = L_sigma_theta_e2 * z_theta_e2;
    
      matrix[N_season, N_month] tau_e2;
      matrix[N_site, N_month] psi_e2;
      tau_e2 = mu_tau_e2 + z_tau_e2 * sigma_tau_e2;
      psi_e2 = mu_psi_e2 + z_psi_e2 * sigma_psi_e2;
      
      ///////////////////
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      
      vector[N_day] theta_e3;
      matrix[N_dim_day, N_dim_day] sigma_theta_e3;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e3;
      sigma_theta_e3 = cov_GPL2(dist_day, eta_theta_e3, rho_theta_e3, 0.01);
      L_sigma_theta_e3 = cholesky_decompose(sigma_theta_e3);
      theta_e3 = L_sigma_theta_e3 * z_theta_e3;
    
      matrix[N_season, N_month] tau_e3;
      matrix[N_site, N_month] psi_e3;
      tau_e3 = mu_tau_e3 + z_tau_e3 * sigma_tau_e3;
      psi_e3 = mu_psi_e3 + z_psi_e3 * sigma_psi_e3;
    
      ////////////////////
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      
      vector[N_day] theta_e4;
      matrix[N_dim_day, N_dim_day] sigma_theta_e4;
      matrix[N_dim_day, N_dim_day] L_sigma_theta_e4;
      sigma_theta_e4 = cov_GPL2(dist_day, eta_theta_e4, rho_theta_e4, 0.01);
      L_sigma_theta_e4 = cholesky_decompose(sigma_theta_e4);
      theta_e4 = L_sigma_theta_e4 * z_theta_e4;
    
      matrix[N_season, N_month] tau_e4;
      matrix[N_site, N_month] psi_e4;
      tau_e4 = mu_tau_e4 + z_tau_e4 * sigma_tau_e4;
      psi_e4 = mu_psi_e4 + z_psi_e4 * sigma_psi_e4;
    
      ///////////////////
    }
    
    model {
      
      ///////////////////////
      /////////////////////// imputed variables
      mu_sb ~ normal(3.5, 1);
      sigma_sb ~ exponential(1);
      SBiomass_merge ~ normal(mu_sb, sigma_sb);
      mu_sbINI ~ normal(3.5, 1);
      sigma_sbINI ~ exponential(1);
      swarmINI_merge ~ normal(mu_sbINI, sigma_sbINI);
      mu_sbFIN ~ normal(3.5, 1);
      sigma_sbFIN ~ exponential(1);
      swarmFIN_merge ~ normal(mu_sbFIN, sigma_sbFIN);
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      vector[N] mu_e1;
      a_e1 ~ normal(0, 1);
      beta_rainW2_e1 ~ normal(0, 0.5);
      sigma_e1 ~ exponential(1);
      
      ///////////////////
      /////////////////// rain_i -> swarm
      to_vector(a_e2) ~ normal(3, 1);
      sigma_e2 ~ exponential(1);
      to_vector(beta_rainW1_e2) ~ normal(0, 0.25);
    
      z_theta_e2 ~ normal(0, 0.5);     
      eta_theta_e2 ~ exponential(3);   
      rho_theta_e2 ~ exponential(0.5); 
      
      to_vector(z_tau_e2) ~ normal(0, 0.5);
      mu_tau_e2 ~ normal(0, 0.5);
      sigma_tau_e2 ~ exponential(1);
    
      to_vector(z_psi_e2) ~ normal(0, 0.5);
      mu_psi_e2 ~ normal(0, 0.5);
      sigma_psi_e2 ~ exponential(1);
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      to_vector(a_e3) ~ normal(2.5, 1);
      sigma_e3 ~ exponential(1);
      to_vector(beta_rainW1_e3) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e3) ~ normal(0, 3);
    
      z_theta_e3 ~ normal(0, 1);
      eta_theta_e3 ~ exponential(3);
      rho_theta_e3 ~ exponential(0.5);
      
      to_vector(z_tau_e3) ~ normal(0, 0.5);
      mu_tau_e3 ~ normal(0, 0.5);
      sigma_tau_e3 ~ exponential(1);
    
      to_vector(z_psi_e3) ~ normal(0, 0.5);
      mu_psi_e3 ~ normal(0, 0.5);
      sigma_psi_e3 ~ exponential(1);
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      to_vector(a_e4) ~ normal(0.25, 0.5);
      sigma_e4 ~ exponential(1);
      to_vector(beta_rainW1_e4) ~ normal(0, 0.5);
      to_vector(beta_sawrA_e4) ~ normal(0, 0.5);
      to_vector(beta_swarmB_e4) ~ normal(0, 0.5);
    
      z_theta_e4 ~ normal(0, 0.5);
      eta_theta_e4 ~ exponential(3);
      rho_theta_e4 ~ exponential(0.5);
      
      to_vector(z_tau_e4) ~ normal(0, 0.5);
      mu_tau_e4 ~ normal(0, 0.5);
      sigma_tau_e4 ~ exponential(1);
    
      to_vector(z_psi_e4) ~ normal(0, 0.5);
      mu_psi_e4 ~ normal(0, 0.5);
      sigma_psi_e4 ~ exponential(1);
      
    
      /////////////////////////
      ///////////////////// rain_i+n -> rain i
      for (i in 1:N) {
        cum_rainfall_W25[i] ~ normal(a_e1 + beta_rainW2_e1 * cum_rainfall_W30[i], 
                                    sigma_e1);
      }
    
      ///////////////////
      /////////////////// rain_i -> swarm
      for (i in 1:N) {
        swarm_area[i] ~ student_t(2,
                                  a_e2[site[i], season[i]] + 
                                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W25[i] +
                                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                                  psi_e2[site[i], month[i]], 
                                  sigma_e2);
      }
    
      ///////////////////////// rain_i -> swarm -> insects biomass
      ///////////////////////// rain_i ->  insects biomass
      for (i in 1:N) {
        SBiomass_merge[i] ~ student_t(2,
                                     a_e3[site[i], season[i]] + 
                                     beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                                     beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                                     theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                                     psi_e3[site[i], month[i]], 
                                     sigma_e3);
      }
    
    
      ///////////////////////// rain_i -> swarm -> insects biomass -> network
      ///////////////////////// rain_i -> network
      ///////////////////////// swarm -> network
      for (i in 1:N) {
         skewness[i] ~ normal(a_e4[site[i], season[i]] + 
                              beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                              beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                              beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                              theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                              psi_e4[site[i], month[i]], sigma_e4);
      }
    
    }
    
    generated quantities {
      vector[N] mu_e1;
      vector[N] mu_e2;
      vector[N] mu_e3;
      vector[N] mu_e4;
      array[N] real ppcheck_e1;
      array[N] real ppcheck_e2;
      array[N] real ppcheck_e3;
      array[N] real ppcheck_e4;
      
      for (i in 1:N) {
        mu_e1[i] = a_e1 + beta_rainW2_e1 * cum_rainfall_W25[i]; 
      }
    
      ppcheck_e1 = normal_rng(mu_e1, sigma_e1);
    
      for (i in 1:N) {
        mu_e2[i] = a_e2[site[i], season[i]] + 
                  beta_rainW1_e2[site[i], season[i]]*cum_rainfall_W30[i] +
                  theta_e2[day[i]] + tau_e2[season[i], month[i]] + 
                  psi_e2[site[i], month[i]];
      }
      
      ppcheck_e2 = student_t_rng(2, mu_e2, sigma_e2);
    
    
      for (i in 1:N) {
        mu_e3[i] = a_e3[site[i], season[i]] + 
                   beta_rainW1_e3[site[i], season[i]]*cum_rainfall_W25[i] +
                   beta_sawrA_e3[site[i], season[i]]*swarm_area[i] +
                   theta_e3[day[i]] + tau_e3[season[i], month[i]] + 
                   psi_e3[site[i], month[i]];
      }
      
      ppcheck_e3 = student_t_rng(2, mu_e3, sigma_e3);
    
      for (i in 1:N) {
         mu_e4[i] = a_e4[site[i], season[i]] + 
                    beta_rainW1_e4[site[i], season[i]]*cum_rainfall_W25[i] +
                    beta_sawrA_e4[site[i], season[i]]*swarm_area[i] +
                    beta_swarmB_e4[site[i], season[i]]*SBiomass_merge[i] +
                    theta_e4[day[i]] + tau_e4[season[i], month[i]] + 
                    psi_e4[site[i], month[i]];
      }
    
      ppcheck_e4 = normal_rng(mu_e4, sigma_e4);
    
    }
    
    ")

file <- paste(getwd(), '/w25_skewness.stan', sep ='')

fit_skewnessW25 <- cmdstan_model(file, compile = T)

mod_skewnessW25_obs <- 
  fit_skewnessW25$sample(
    data = network_metrics_obs2.1, 
    chains = 3, 
    parallel_chains = 3, 
    iter_sampling = 8e3, 
    iter_warmup = 500, 
    thin = 10, 
    seed = 123
  )

mod_skewnessW25_obs$save_object('causal_mod_skewnessW25.rds')

mod_skewnessW25_obs <- readRDS('causal_mod_skewnessW25.rds')

mod_skewnessW25_obs$summary(
  unique(paste('psi_e2[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(
  unique(paste('psi_e3[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(
  unique(paste('psi_e4[',
               network_metrics_obs2.1$site,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(
  unique(paste('tau_e4[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(
  unique(paste('tau_e2[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(
  unique(paste('tau_e3[',
               network_metrics_obs2.1$year,
               ',',
               network_metrics_obs2.1$month, ']',
               sep = ''))) |> print(n = 400)

mod_skewnessW25_obs$summary(paste('theta_e4[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 200)

mod_skewnessW25_obs$summary(paste('theta_e2[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130) # ajustar este parametro

mod_skewnessW25_obs$summary(paste('theta_e3[',
                                  1:network_metrics_obs2.1$N_day, ']',
                                  sep = '')) |> print(n = 130)


png('W25_skewness_diagnostics.png', width = 10, height = 15, 
    res = 500, units = 'cm')
par(mfrow = c(5, 2), mar = c(4, 4, 1.5, 1))
mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('psi_e2[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = '')))) 

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('psi_e3[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('psi_e4[', 
                               network_metrics_obs2.1$site,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('tau_e4[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('tau_e2[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(
                  unique(paste('tau_e3[', 
                               network_metrics_obs2.1$year,
                               ',',
                               network_metrics_obs2.1$month, ']', 
                               sep = ''))))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(paste('theta_e4[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(paste('theta_e3[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))

mod_diagnostics(mod_skewnessW25_obs, 
                mod_skewnessW25_obs$summary(paste('theta_e2[', 
                                                  1:network_metrics_obs2.1$N_day, ']', 
                                                  sep = '')))
dev.off()



summary_skewnessW25 <- mod_skewnessW25_obs$summary(c('beta_rainW2_e1', 
                                                     'beta_rainW1_e2',
                                                     'beta_rainW1_e3',
                                                     'beta_rainW1_e4',
                                                     'beta_sawrA_e3',
                                                     'beta_sawrA_e4',
                                                     'beta_swarmB_e4', 
                                                     'a_e1', 'a_e2', 
                                                     'a_e3', 'a_e4',
                                                     'sigma_e1', 'sigma_e2', 
                                                     'sigma_e3', 'sigma_e4', 
                                                     'swarm_area',
                                                     'SBiomass_merge',
                                                     'lp__'))

summary_skewnessW25 |> print(n = 15)



mod_diagnostics(mod_skewnessW25_obs, summary_skewnessW25)


post_skewnessW25_obs <- mod_skewnessW25_obs$draws(c('beta_rainW2_e1', 
                                                    'beta_rainW1_e2',
                                                    'beta_rainW1_e3',
                                                    'beta_rainW1_e4',
                                                    'beta_sawrA_e3',
                                                    'beta_sawrA_e4',
                                                    'beta_swarmB_e4', 
                                                    'a_e1', 'a_e2', 'a_e3', 'a_e4',
                                                    'sigma_e1', 'sigma_e2', 
                                                    'sigma_e3', 'sigma_e4',
                                                    'swarm_area',
                                                    'SBiomass_merge'
), 
format = 'df')

swarm_area <- apply(post_skewnessW25_obs[, grep('swarm_area', colnames(post_skewnessW25_obs))], 
                    2, mean)

swarm_BM <- 
  apply(post_skewnessW25_obs[, grep('SBiomass_merge', colnames(post_skewnessW25_obs))], 
        2, mean)

ppcheck_swarmA <- mod_skewnessW25_obs$draws('ppcheck_e2', format = 'matrix')
ppcheck_swarmB <- mod_skewnessW25_obs$draws('ppcheck_e3', format = 'matrix')
ppcheck_skewness <- mod_skewnessW25_obs$draws('ppcheck_e4', format = 'matrix')
ppcheck_R1 <- mod_skewnessW25_obs$draws('ppcheck_e1', format = 'matrix')

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
plot(density(network_metrics_obs2.1$cum_rainfall_W25), 
     main = '', xlab = 'Cum rainfall W25')
for(i in 1:100) lines(density(ppcheck_R1[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$cum_rainfall_W25), col = 'red')

plot(density(swarm_area), main = '', xlab = 'Swarm area')
for(i in 1:100) lines(density(ppcheck_swarmA[i, ]), lwd = 0.1)
lines(density(swarm_area), col = 'red')

plot(density(swarm_BM), main = '', xlab = 'Arthropod biomass')
for(i in 1:500) lines(density(ppcheck_swarmB[i, ]), lwd = 0.1)
lines(density(swarm_BM), col = 'red')

plot(density(network_metrics_obs2.1$skewness), 
     main = '', xlab = 'skewness')
for(i in 1:100) lines(density(ppcheck_skewness[i, ]), lwd = 0.1)
lines(density(network_metrics_obs2.1$skewness), col = 'red')

dim(post_skewnessW25_obs)


post_beta_skewnessW25 <- gather(post_skewnessW25_obs[, grep('beta', colnames(post_skewnessW25_obs))])

post_beta_skewnessW25$key <- as.factor(post_beta_skewnessW25$key)


post_beta_skewnessW25 |> 
  group_by(key) |> 
  transmute(mu = mean(value), 
            li = quantile(value, 0.025), 
            ls = quantile(value, 0.975)) |> 
  unique() |> 
  ggplot(aes(mu, key, xmin = li, xmax = ls)) +
  geom_vline(xintercept = 0, color = 'red', linetype = 3) +
  geom_point() +
  geom_errorbar(width = 0)

# ====== plotting and counterfactual skewness ====

# ====== forest plot and conditional effects ====

t <- paste('W', c(1, seq(5, 25, by = 5)), sep = '')

plot_beta_skewness <- vector('list', length(t))

for (i in seq_along(t)) {
  cadena <- paste('^(.*)beta_skewness(.*)', t[i], '$', sep = '')
  cadena2 <- paste('post_skewness', t[i], '_obs', sep = '')
  df <- get(ls()[grepl(cadena, ls())])
  temp <- get(ls()[grepl(cadena2, ls())])
  
  temp <- temp[, grep('^beta', colnames(temp))]
  
  temp <- apply(temp, 2, function(x) mean(x > 0))
  
  temp <- tibble(p = temp, key = names(temp))
  
  df <- 
    df |> 
    group_by(key) |> 
    transmute(li = quantile(value, 0.025), 
              ls = quantile(value, 0.975), 
              mu = mean(value), 
              window = t[i]) |> 
    unique()
  
  n <- c(1, seq(5, 25, by = 5))[i]
  
  df <- df[!grepl(n + 4, df$key), ]
  
  df <- full_join(df, temp, by = 'key')
  
  plot_beta_skewness[[i]] <- df
  print(i)
}

plot_beta_skewness <- do.call('rbind', plot_beta_skewness)

plot_beta_skewness <- plot_beta_skewness[!grepl('W2', plot_beta_skewness$key),]

plot_beta_skewness$window <- as.factor(plot_beta_skewness$window)

plot_beta_skewness$season <- 
  ifelse(grepl('^(.*)(..,1.)$', plot_beta_skewness$key), 'Dry', 'Wet')

plot_beta_skewness$site <- 
  ifelse(grepl('^(.*)(.1,..)$', plot_beta_skewness$key), '1', 
         ifelse(grepl('^(.*)(.2,..)$', plot_beta_skewness$key), '2', 
                ifelse(grepl('^(.*)(.3,..)$', plot_beta_skewness$key), '3', '4')))

plot_beta_skewness$site <- as.factor(plot_beta_skewness$site)

plot_beta_skewness$site <- 
  factor(plot_beta_skewness$site, 
         labels = levels(network_metrics_INDX$obs_all$site))

plot_beta_skewness$rainfall_level <- 
  factor(plot_beta_skewness$site, 
         labels = c('Medium-high SL rainfall', 'Medium-low SL rainfall', 
                    'Low SL rainfall', 'High SL rainfall'))

plot_beta_skewness$rainfall_level <- 
  factor(plot_beta_skewness$rainfall_level, 
         levels = c('Low SL rainfall', 'Medium-low SL rainfall', 
                    'Medium-high SL rainfall', 'High SL rainfall'))


plot_beta_skewness$key <- gsub('^(.*)(.....)$', '\\1', plot_beta_skewness$key)

plot_beta_skewnessWET <- plot_beta_skewness[plot_beta_skewness$season == 'Wet',]

plot_beta_skewnessWET$key <- factor(plot_beta_skewnessWET$key) 

plot_beta_skewnessWET$code <- 
  plot_beta_skewnessWET %$% paste(key, window, rainfall_level, sep = '_')

sum(plot_beta_skewnessWET$code == "beta_rainW1_e2_W15_Medium-low rainfall" |
      plot_beta_skewnessWET$code == "beta_sawrA_e4_W15_Medium-low rainfall")

levels(plot_beta_skewnessWET$key)

levels(plot_beta_skewnessWET$rainfall_level)
levels(plot_beta_skewnessWET$site)

plot_beta_skewnessWET$key <- 
  factor(plot_beta_skewnessWET$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

levels(plot_beta_skewnessWET$window)
plot_beta_skewnessWET$window <- 
  factor(plot_beta_skewnessWET$window, 
         levels = c("W1", "W5", "W10", "W15", "W20", "W25"))


#plot_effects_skewnessWET2 <- 
plot_beta_skewnessWET |> 
  dplyr::filter(site == 'LIMB' | site == 'PLRD') |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Skewness'),
                              expression('Swarm area' %->% 'Skewness'),
                              expression('ST rainfall' %->% 'Skewness'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Arthropod biomass'),
                              expression('ST rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    title = element_text(size = 13),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x.top = element_text(size = 12.5), 
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.15, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    text = element_text(family = 'Times New Roman')
  )

ggsave('wet_season_skewness.jpg', units = 'cm', dpi = 1500,
       height = 15, width = 11)


plot_effects_skewnessWET <- 
  plot_beta_skewnessWET |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Norm. degree'),
                              expression('Rainfall' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = 'Effect', title = 'Wet season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.09), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman')
  )


plot_beta_skewnessDRY <- plot_beta_skewness[plot_beta_skewness$season == 'Dry',]

plot_beta_skewnessDRY$key <- factor(plot_beta_skewnessDRY$key)

levels(plot_beta_skewnessDRY$key)

levels(plot_beta_skewnessDRY$site)

plot_beta_skewnessDRY$key <- 
  factor(plot_beta_skewnessDRY$key, 
         levels = 
           c("beta_rainW1_e2", 
             "beta_rainW1_e3",
             "beta_sawrA_e3", 
             "beta_rainW1_e4",
             "beta_sawrA_e4",
             "beta_swarmB_e4"))

plot_effects_skewnessDRY <- 
  plot_beta_skewnessDRY |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  geom_errorbar(aes(y = key, xmin = li, 
                    xmax = ls, color = window), 
                width = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(mu, key, color = window, shape = window), 
             position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(rep('lightblue3', 2), 
                                'lightblue3', 
                                rep('lightblue3', 3))) +
  facet_wrap(~ rainfall_level, ncol = 1) +
  scale_y_discrete(limits =
                     c(
                       "beta_swarmB_e4",
                       "beta_sawrA_e4",
                       "beta_rainW1_e4",
                       "beta_sawrA_e3",
                       "beta_rainW1_e3",
                       "beta_rainW1_e2"),
                   labels = c(expression('Arthropod biomass' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Norm. degree'),
                              expression('Rainfall' %->% 'Norm. degree'),
                              expression('Swarm area' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Arthropod biomass'),
                              expression('Rainfall' %->% 'Swarm area'))) +
  labs(x = expression(beta), y = NULL, title = 'Dry season') +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.10), 
    legend.box.background = element_blank(), 
    legend.key.size = unit(2.5, 'mm'),
    text = element_text(family = 'Times New Roman'), 
    axis.text.y = element_blank()
  )

plot_effects_skewnessWET | plot_effects_skewnessDRY +
  plot_layout(ncol = 1)

ggsave('netwrok_size_effect.jpg', units = 'cm', 
       height = 20, width = 17, dpi = 500)

plot_effects_skewnessDRY 

plot_effects_skewnessWET2



# ======= conditional effects ======

s <- network_metrics_INDX$obs_all
s <- s[, -c(2, 4)]

s$season <- 
  ifelse(as.numeric(as.character(s$month)) <= 4, 1, 2)

s <- 
  s %$% 
  aggregate(skewness ~ season + site, FUN = length)

s

s2 <- network_metrics_INDX$obs_all
s2 <- s2[, -c(2, 4)]
s2$obs_ID <- as.factor(s2$obs_ID)

unique(tibble(x = network_metrics_INDX$obs_all$site, 
              x1 = as.numeric(network_metrics_INDX$obs_all$site)))

plot_beta_skewness[plot_beta_skewness$p >= 0.9 |
                     plot_beta_skewness$p <= 0.1, ] |> print(n = 100)

effects_skewness <- 
  plot_beta_skewness[plot_beta_skewness$p >= 0.9 |
                       plot_beta_skewness$p <= 0.1, ]

effects_skewness <- split(effects_skewness, 
                          list(effects_skewness$window, 
                               effects_skewness$season, 
                               effects_skewness$site))

effects_skewness <- effects_skewness[unlist(lapply(effects_skewness, function(x) nrow(x) > 0), 
                                            use.names = F)]

# ==== site JUAN ====
for (i in grep('JUAN', names(effects_skewness))) print(effects_skewness[[i]])
# NO effects


# ======= site PLRD =======

for (i in grep('PLRD', names(effects_skewness))) print(effects_skewness[[i]])
# PLRD swarm area --> net size (independent of the window)

swarm_area <- 
  sapply(ls()[grep('post_skewness', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_skewness', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_obs2.1$site, 
         day = network_metrics_obs2.1$day, 
         month = network_metrics_obs2.1$month, 
         season = network_metrics_obs2.1$season)

day <- unique(sites[, c('day', 'site', 'month', 'season')])

day <- day[day$site == 3 & 
             day$season == 2, ]

day2 <- unique(day[, -1])

psi_e4W15LR <- 
  mod_skewnessW15_obs$draws(
    unique(paste('psi_e4[', 
                 3,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

psi_e3W15LR <- 
  mod_skewnessW15_obs$draws(
    unique(paste('psi_e3[', 
                 3,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


tau_e4W15LR <- 
  mod_skewnessW15_obs$draws(
    unique(paste('tau_e4[', 
                 2,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)

tau_e3W15LR <- 
  mod_skewnessW15_obs$draws(
    unique(paste('tau_e3[', 
                 2,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


theta_e4W15LR <- 
  mod_skewnessW15_obs$draws(paste('theta_e4[', 
                                  day$day, ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)

theta_e3W15LR <- 
  mod_skewnessW15_obs$draws(paste('theta_e3[', 
                                  day$day, ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)

x_W15 <- seq(min(network_metrics_obs2.1$cum_rainfall_W15), 
             max(network_metrics_obs2.1$cum_rainfall_W15),
             length.out = 1000)

est_counterNS_e3 <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e3[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e3[3,2]`*x +
               post_skewnessW15_obs$`beta_sawrA_e3[3,2]`*mean(swarm_area) +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
             rstudent(length(NS), mu = NS, sigma = post_skewnessW15_obs$sigma_e3)
             
           })


est_counterNS_e3MU <- 
  sapply(x_W15, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e3[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e3[3,2]`*x +
               post_skewnessW15_obs$`beta_sawrA_e3[3,2]`*mean(swarm_area) +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
             NS
             
           })

est_counterNS_e3 <- 
  do.call('rbind', 
          apply(est_counterNS_e3, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS_e3$x <- x_W15
est_counterNS_e3$season <- 'Wet season'
est_counterNS_e3$mu <- apply(est_counterNS_e3MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_obs2.1$season, 
              site = network_metrics_obs2.1$site, 
              skewness = network_metrics_obs2.1$skewness, 
              rain = network_metrics_obs2.1$cum_rainfall_W15)


plot_rain_skewness_PLR <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 3 & dat$season == 2,],
    aes(rain, y), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS_e3, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tomato3'
  ) +
  geom_line(
    data = est_counterNS_e3, 
    aes(x, mu), linewidth = 1, 
    color = 'tomato3', linetype = 2
  ) +
  labs(x = 'ST rainfall W15', 
       y = 'Arthropod biomass') +
  scale_y_continuous(breaks = seq(2, 10, by = 2)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 16))


x_W15_2 <- seq(min(swarm_area), 
               max(swarm_area),
               length.out = 1000)

est_counterNS_e3_2 <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e3[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e3[3,2]`*0 +
               post_skewnessW15_obs$`beta_sawrA_e3[3,2]`*x +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
             rstudent(length(NS), mu = NS, sigma = post_skewnessW15_obs$sigma_e3)
             
           })


est_counterNS_e3MU2 <- 
  sapply(x_W15_2, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e3[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e3[3,2]`*0 +
               post_skewnessW15_obs$`beta_sawrA_e3[3,2]`*x +
               psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
             
             NS
             
           })

est_counterNS_e3_2 <- 
  do.call('rbind', 
          apply(est_counterNS_e3_2, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS_e3_2$x <- x_W15_2
est_counterNS_e3_2$season <- 'Wet season'
est_counterNS_e3_2$mu <- apply(est_counterNS_e3MU2, 2, mean)


plot_SA_skewness_PLR <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 3 & dat$season == 2,],
    aes(x, y), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS_e3_2, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tomato3'
  ) +
  geom_line(
    data = est_counterNS_e3_2, 
    aes(x, mu), linewidth = 1, 
    color = 'tomato3', linetype = 2
  ) +
  labs(x = 'Swarm area', 
       y = 'Arthropod biomass') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 16))


x_W15_3 <- seq(min(swarm_BM), 
             max(swarm_BM),
             length.out = 1000)

est_counterNS_e3_3 <- 
  sapply(x_W15_3, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e4[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e4[3,2]`*0 +
               post_skewnessW15_obs$`beta_sawrA_e4[3,2]`*mean(swarm_area) +
               post_skewnessW15_obs$`beta_swarmB_e4[3,2]`*x +
               psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
             
             rnorm(length(NS), mean = NS, sd = post_skewnessW15_obs$sigma_e3)
             
           })


est_counterNS_e3MU3 <- 
  sapply(x_W15_3, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW15_obs$`a_e4[3,2]` + 
               post_skewnessW15_obs$`beta_rainW1_e4[3,2]`*0 +
               post_skewnessW15_obs$`beta_sawrA_e4[3,2]`*mean(swarm_area) +
               post_skewnessW15_obs$`beta_swarmB_e4[3,2]`*x + 
               psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
             
             NS
             
           })

est_counterNS_e3_3 <- 
  do.call('rbind', 
          apply(est_counterNS_e3_3, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counterNS_e3_3$x <- x_W15_3
est_counterNS_e3_3$season <- 'Wet season'
est_counterNS_e3_3$mu <- apply(est_counterNS_e3MU3, 2, mean)


plot_sekewness_PLR <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 3 & dat$season == 2,],
    aes(y, skewness), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counterNS_e3_3, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'tomato3'
  ) +
  geom_line(
    data = est_counterNS_e3_3, 
    aes(x, mu), linewidth = 1, 
    color = 'tomato3'
  ) +
  labs(x = 'Arthropod biomass', 
       y = 'Skewness') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 16))

plot_rain_skewness_PLR +
  plot_SA_skewness_PLR + 
  plot_sekewness_PLR +
  plot_layout(ncol = 3)

ggsave('PRL_wet_skewness.jpg', units = 'cm', dpi = 1000, 
       width = 18, height = 7)

# ====== causal effect PRL =====

counterfact_skewness_PRL <- 
  function(.fun = mean, mu = T, year = 2022, var = 'rainfall') {
    
    rain <- network_metrics_obs2.1$cum_rainfall_W15
    
    SA <- swarm_BM
    
    if (var == 'rainfall') {
      
      AB <- 
        post_skewnessW15_obs$`a_e3[3,2]` + 
        post_skewnessW15_obs$`beta_rainW1_e3[3,2]`*.fun(rain) +
        post_skewnessW15_obs$`beta_sawrA_e3[3,2]`*mean(swarm_area) +
        psi_e3W15LR + theta_e3W15LR + tau_e3W15LR
      
      NS <- 
        post_skewnessW15_obs$`a_e4[3,2]` + 
        post_skewnessW15_obs$`beta_rainW1_e4[3,2]`*0 +
        post_skewnessW15_obs$`beta_sawrA_e4[3,2]`*mean(swarm_area) +
        post_skewnessW15_obs$`beta_swarmB_e4[3,2]`*AB +
        psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
      
    } 
    
    if (var == 'n') {
      NS <- 
        post_skewnessW15_obs$`a_e4[3,2]` + 
        post_skewnessW15_obs$`beta_rainW1_e4[3,2]`*0 +
        post_skewnessW15_obs$`beta_sawrA_e4[3,2]`*mean(swarm_area) +
        post_skewnessW15_obs$`beta_swarmB_e4[3,2]`*.fun(swarm_BM) +
        psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
    }
    
    if (var == 'swarm') {
      NS <- 
        post_skewnessW15_obs$`a_e4[3,2]` + 
        post_skewnessW15_obs$`beta_rainW1_e4[3,2]`*0 +
        post_skewnessW15_obs$`beta_sawrA_e4[3,2]`*.fun(swarm_area) +
        post_skewnessW15_obs$`beta_swarmB_e4[3,2]`*mean(swarm_BM) +
        psi_e4W15LR + theta_e4W15LR + tau_e4W15LR
    }
    
    if (mu) {
      NS
    } else {
      set.seed(123)
      rnorm(length(NS), mean = NS, sd = post_skewnessW15_obs$sigma_e4)
    }
  }  

length(counterfact_skewness_PRL(min, var = 'rain'))

plot(density(counterfact_skewness_PRL(min, mu = F, var = 'rainfall')))
lines(density(counterfact_skewness_PRL(mean, mu = F, var = 'rainfall')), col = 'blue')
lines(density(counterfact_skewness_PRL(max, mu = F, var = 'rainfall')), col = 'red')


skewness_counter_PRL <- 
  tibble(mu_val = c(counterfact_skewness_PRL(max, F, var = 'rainfall') - counterfact_skewness_PRL(min, F, var = 'rainfall')), 
         Intervention = 'Causal effect of   \n ST rainfall W15  ')


mean(skewness_counter_PRL$mu_val < 0)
mean(skewness_counter_PRL$mu_val)
sd(skewness_counter_PRL$mu_val)

skewness_counter_PRL2 <- 
  tibble(mu_val = c(counterfact_skewness_PRL(max, F, var = 'n') - counterfact_skewness_PRL(min, F, var = 'n')), 
         Intervention = 'Causal effect of  \n Arthropod biomass')

mean(skewness_counter_PRL2$mu_val < 0)
mean(skewness_counter_PRL2$mu_val)
sd(skewness_counter_PRL2$mu_val)

skewness_counter_PRL3 <- 
  tibble(mu_val = c(counterfact_skewness_PRL(max, F, var = 'swarm') - counterfact_skewness_PRL(min, F, var = 'swarm')), 
         Intervention = 'Causal effect of  \n swarm area')

mean(skewness_counter_PRL3$mu_val < 0)
mean(skewness_counter_PRL3$mu_val)
sd(skewness_counter_PRL3$mu_val)

100 - ((mean(skewness_counter_PRL3$mu_val) * 100) / 
         (-mean(skewness_counter_PRL2$mu_val)))
  


plot(density(skewness_counter_PRL$mu_val / 
               skewness_counter_PRL2$mu_val))
  

100 - ((mean(skewness_counter_PRL$mu_val) * 100) / 
         mean(skewness_counter_PRL2$mu_val))

skewness_counter_PRL <- rbind(skewness_counter_PRL, skewness_counter_PRL2)

plot_contrast_skewness_sawrm_PRL <- 
  ggplot() +
  geom_boxplot(data = skewness_counter_PRL, 
               aes(x = mu_val, y = Intervention), 
               alpha = 0.3, color = 'tomato3', 
               fill = 'tomato3', width = 0.2) +
  geom_vline(xintercept = 0, linetype = 2, color = 'red') +
  labs(x = 'Contrast between\n interventions (min - max)', 
       y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16))

plot_contrast_skewness_sawrm_PRL

ggsave('causal_eff_skewness_PRL.jpg', units = 'cm', dpi = 1000, 
       height = 5, width = 16)


# ================== site LIMB =======================


for (i in grep('LIMB', names(effects_skewness))) print(effects_skewness[[i]])
# LIMB Rain W20 --> swarm area --> skewness


unique(tibble(x = network_metrics_INDX$obs_all$site, 
              x1 = as.numeric(network_metrics_INDX$obs_all$site)))

swarm_area <- 
  sapply(ls()[grep('post_skewness', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('swarm_area', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_area <- as.vector(apply(swarm_area, 1, mean))

swarm_BM <- 
  sapply(ls()[grep('post_skewness', ls())], FUN = 
           function(x) {
             df <- get(x)
             sa <- df[, grep('SBiomass_merge', colnames(df))]
             apply(sa, 2, mean)
           }, simplify = 'matrix')

swarm_BM <- as.vector(apply(swarm_BM, 1, mean))


# counterfactual (medium-high rainfall - JUAN)

sites <- 
  tibble(site = network_metrics_obs2.1$site, 
         day = network_metrics_obs2.1$day, 
         month = network_metrics_obs2.1$month, 
         season = network_metrics_obs2.1$season)

day <- unique(sites[, c('day', 'site', 'month', 'season')])

day <- day[day$site == 2 & 
             day$season == 2, ]

day2 <- unique(day[, -1])

psi_e4W20LR <- 
  mod_skewnessW20_obs$draws(
    unique(paste('psi_e4[', 
                 2,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


tau_e4W20LR <- 
  mod_skewnessW20_obs$draws(
    unique(paste('tau_e4[', 
                 2,
                 ',',
                 day2$month, ']', 
                 sep = '')), format = 'matrix') |> 
  apply(1, mean)


theta_e4W20LR <- 
  mod_skewnessW20_obs$draws(paste('theta_e4[', 
                                  day$day, ']', 
                                  sep = ''), format = 'matrix') |> 
  apply(1, mean)


x_W20 <- seq(min(network_metrics_obs2.1$cum_rainfall_W20), 
             max(network_metrics_obs2.1$cum_rainfall_W20),
             length.out = 1000)

est_counter_skewness_e4 <- 
  sapply(x_W20, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW20_obs$`a_e4[2,2]` + 
               post_skewnessW20_obs$`beta_rainW1_e4[2,2]`*x +
               post_skewnessW20_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
               post_skewnessW20_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               psi_e4W20LR + theta_e4W20LR + tau_e4W20LR
             
             rnorm(length(NS), mean = NS, sd = post_skewnessW15_obs$sigma_e4)
             
           })


est_counter_skewness_e4MU <- 
  sapply(x_W20, FUN = 
           function(x) {
             
             NS <- 
               post_skewnessW20_obs$`a_e4[2,2]` + 
               post_skewnessW20_obs$`beta_rainW1_e4[2,2]`*x +
               post_skewnessW20_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
               post_skewnessW20_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
               psi_e4W20LR + theta_e4W20LR + tau_e4W20LR
             
             NS
             
           })

est_counter_skewness_e4 <- 
  do.call('rbind', 
          apply(est_counter_skewness_e4, 2, function(x) {
            tibble(li = quantile(x, 0.025), 
                   ls = quantile(x, 0.975))
          }, simplify = 'list'))

est_counter_skewness_e4$x <- x_W20
est_counter_skewness_e4$season <- 'Wet season'
est_counter_skewness_e4$mu <- apply(est_counter_skewness_e4MU, 2, mean)

dat <- tibble(x = swarm_area, 
              y = swarm_BM, 
              season = network_metrics_obs2.1$season, 
              site = network_metrics_obs2.1$site, 
              skewness = network_metrics_obs2.1$skewness, 
              rain = network_metrics_obs2.1$cum_rainfall_W20)


#plot_rain_skewness_PLR <- 
  ggplot() +
  geom_point(
    data = dat[dat$site == 2 & dat$season == 2,],
    aes(rain, skewness), color = 'lightblue3'
  ) + 
  geom_ribbon(
    data = est_counter_skewness_e4, 
    aes(x, ymin = li, ymax = ls), alpha = 0.25, 
    fill = 'purple4'
  ) +
  geom_line(
    data = est_counter_skewness_e4, 
    aes(x, mu), linewidth = 1, 
    color = 'purple4'
  ) +
  labs(x = 'ST rainfall W20', 
       y = 'Skewness') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'),
        legend.background = element_blank(), 
        legend.position = 'none', 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))


ggsave('LIMB_wet_skewness.jpg', units = 'cm', dpi = 1000, 
       height = 8, width = 8)

# ====== causal effect LIMB =====

counterfact_skewness_LIMB <- 
  function(.fun = mean, mu = T) {
    
    rain <- network_metrics_obs2.1$cum_rainfall_W25
    
    NS <- 
      post_skewnessW20_obs$`a_e4[2,2]` + 
      post_skewnessW20_obs$`beta_rainW1_e4[2,2]`*.fun(rain) +
      post_skewnessW20_obs$`beta_sawrA_e4[2,2]`*mean(swarm_area) +
      post_skewnessW20_obs$`beta_swarmB_e4[2,2]`*mean(swarm_BM) +
      psi_e4W20LR + theta_e4W20LR + tau_e4W20LR
    
    if (mu) {
      exp(NS)
    } else {
      set.seed(123)
      rnorm(length(NS), mean = NS, sd = post_skewnessW15_obs$sigma_e4)
    }
  } 

length(counterfact_skewness_LIMB(min))

plot(density(counterfact_skewness_LIMB(min, mu = F)))
lines(density(counterfact_skewness_LIMB(mean, mu = F)), col = 'blue')
lines(density(counterfact_skewness_LIMB(max, mu = F)), col = 'red')

skewness_counter_LIMB <- 
  tibble(mu_val = c(counterfact_skewness_LIMB(max, F) - counterfact_skewness_LIMB(min, F)), 
         Intervention = 'Causal effect of \n ST rainfall W20')

mean(skewness_counter_LIMB$mu_val < 0)
mean(skewness_counter_LIMB$mu_val)
sd(skewness_counter_LIMB$mu_val)

plot_contrast_net_sawrm_LIMB <- 
  ggplot() +
  geom_boxplot(data = skewness_counter_LIMB, 
               aes(x = mu_val, y = Intervention), 
               alpha = 0.3, color = 'purple4', 
               fill = 'purple4', width = 0.2) +
  geom_vline(xintercept = 0, linetype = 2, color = 'red') +
  labs(x = 'Contrast between\n interventions (min - max)', 
       y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(family = 'Times New Roman'), 
        legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.background = element_blank(), 
        legend.key.size = unit(2, 'mm'), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16))

plot_contrast_net_sawrm_LIMB

ggsave('causal_eff_skewness_LIMB.jpg', units = 'cm', dpi = 1000, 
       height = 5, width = 16)

# ======= site SHER ======

for (i in grep('SHER', names(effects_skewness))) print(effects_skewness[[i]])

# NOO EFFECTS in this site

# ==== contrast seasons ======

posteriors <- ls()[grep('^(post_skewness)(W[0-9]*)', ls())]

df_seasons <- 
  lapply(posteriors, FUN = 
           function(draws) {
             
             p <- get(draws)
             
             w <- gsub('^(.*)(W[0-9]*)(.*)$', '\\2', draws)
             
             post_season <- 
               p[, colnames(p)[grep('^a_e4', colnames(post_net_sizeW1_obs))]] |> 
               gather() 
             
             post_season <- split(post_season, post_season$key)  
             
             sites <- rep(c('Medium-high rainfall', 
                            'Medium-low rainfall', 
                            'low rainfall', 
                            'High rainfall'), each = 2)
             
             season <- rep(c('Dry', 'Wet'), 4)
             
             for (i in seq_along(sites)) {
               post_season[[i]]$site <- sites[[i]]
               post_season[[i]]$season <- season[[i]]
               post_season[[i]] <- post_season[[i]][, -1]
             }
             
             post_season <- do.call('rbind', post_season)
             post_season <- split(post_season, post_season$site)
             
             post_season <- 
               lapply(post_season, FUN = 
                        function(x) {
                          j <- 
                            x[x$season == 'Dry', ]$value - 
                            x[x$season == 'Wet', ]$value
                          
                          tibble(site = x$site[1], 
                                 contrast = j, 
                                 p = mean(j > 0))
                        })
             
             post_season <- do.call('rbind', post_season)
             
             post_season$model <- w
             
             post_season
           })

df_seasons <- do.call('rbind', df_seasons)

df_seasons |> 
  ggplot(aes(contrast, fill = site)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 3, col = 'red') +
  labs(x = 'Skewness\n (contrast between wet and dry season)') +
  #lims(x = c(-40, 80)) +
  facet_wrap(~model)





