sapply(c('tidyverse', 'readxl', 'magrittr', 'sf', 'animation'), 
       library, character.only = T)

# ======= matrices de distancias ======

# distance days

d <- read_xlsx('birds_clime/2022-23_AntbirdAssociationNetwork_11Nov2023.xlsx', 
                      sheet = 1, col_names = T, na = 'NA')
d <- unique(d[, 1:3])

distance_days <- d

distance_days$date <- as.Date(distance_days$date)
distance_days$date_fct <- as.factor(distance_days$date)

distance_days$date_fct <- factor(distance_days$date_fct, 
                                 levels = as.character(unique(sort(distance_days$date))))
levels(distance_days$date_fct)

as.numeric(distance_days$date[1] - distance_days$date[2])

distance_days$date_code <- as.numeric(distance_days$date_fct)

distance_days$month <- as.numeric(gsub('^(.*)(-)(0[1-9])(-)(.*)$', '\\3', distance_days$date))

distance_days[distance_days$month ==2, ]

sampling <- read_xlsx('birds_clime/antswarm_coordinates_2022_23.xlsx', 
                      sheet = 1)[, 1:3]

distance_days <- full_join(distance_days, sampling, by = 'observation_ID')

apply(distance_days, 2, function(x) sum(is.na(x)))

distance_days[is.na(distance_days$GPS_UTM_easting_bivouac), ]

distance_days <- distance_days[!is.na(distance_days$GPS_UTM_northing_bivouac), ]

# day matrix 

k_days <- matrix(NA, 
                 ncol = length(levels(distance_days$date_fct)), 
                 nrow = length(levels(distance_days$date_fct)))

rownames(k_days) <- levels(distance_days$date_fct)
colnames(k_days) <- levels(distance_days$date_fct)

days <- levels(distance_days$date_fct)
d1 <- double(1)
d2 <- double(1)

for (i in 1:nrow(k_days)) for (j in 1:ncol(k_days)) {
  d1 <- unique(distance_days$date[distance_days$date == days[i]])
  d2 <- unique(distance_days$date[distance_days$date == days[j]])
  
  k_days[i, j] <- abs(d1 - d2)
}

class(month(distance_days$date))

# month matrix

months <- sort(unique(distance_days$month))

k_months <- 
          #2  3. 4. 5. 6. 7. 8. 9
  matrix(c(0, 1, 2, 3, 4, 5, 5, 4, # 2
           1, 0, 1, 2, 3, 4, 5, 6, # 3
           2, 1, 0, 1, 2, 3, 4, 5, # 3
           3, 2, 1, 0, 1, 2, 3, 4, # 5
           4, 3, 2, 1, 0, 1, 2, 3, # 6
           5, 4, 3, 2, 1, 0, 1, 2, # 7
           5, 5, 4, 3, 2, 1, 0, 1, # 8
           4, 6, 5, 4, 3, 2, 1, 0),# 9
         ncol = length(months), nrow = length(months))

colnames(k_months) <- paste('m', months, sep = '')
rownames(k_months) <- paste('m', months, sep = '')

# sampling sites matrix

distance_days$GPS_UTM_easting_bivouac <- as.numeric(distance_days$GPS_UTM_easting_bivouac)
distance_days$GPS_UTM_northing_bivouac <- as.numeric(distance_days$GPS_UTM_northing_bivouac)


distance_days



distance_days2 <- distance_days[order(distance_days$date_code), 
                                c("date", "site", "date_fct", 
                                  "date_code", "month", 
                                  "GPS_UTM_easting_bivouac", 
                                  "GPS_UTM_northing_bivouac")]

distance_days2 <- 
  distance_days2 |> 
  group_by(date, date_fct, date_code, month) |> 
  transmute(north_y = mean(GPS_UTM_northing_bivouac), 
            east_x = mean(GPS_UTM_easting_bivouac)) |> 
  unique()

distance_days3 <- split(distance_days2, distance_days2$date_fct)
distance_days3 <- 
  lapply(distance_days3, FUN = 
           function(x) {
             tempX <- (x$east_x - distance_days2$east_x)^2 +
               (x$north_y - distance_days2$north_y)^2
             tempX <- sqrt(tempX)
             
             tibble(date = distance_days2$date,
                    date_fct = distance_days2$date_fct,
                    date_code = distance_days2$date_code,
                    comp_code = x$date_code, 
                    month = distance_days2$month,
                    dist = tempX)
             
           })

distance_days3 <- do.call('rbind', distance_days3)

dist_spatial <- matrix(distance_days3$dist, 
                       ncol = length(levels(distance_days$date_fct)), 
                       nrow = length(levels(distance_days$date_fct)), 
                       byrow = F)

dim(dist_spatial)

colnames(dist_spatial) <- levels(distance_days3$date_fct)
rownames(dist_spatial) <- levels(distance_days3$date_fct)

saveRDS(
  list(day_matrix = k_days,
       month_matrix = k_months,
       spatial_matrix = dist_spatial,
       df1 = distance_days, 
       df2 = distance_days3),
  'birds_clime/distance_matrices.rds'
)



# ======= fin ======

d <- read_xlsx('birds_clime/2022-23_AntbirdAssociationNetwork_11Nov2023.xlsx', 
               sheet = 1, col_names = T, na = 'NA')

d <- unique(d[, 1:3])

stations_PosName <- 
  read_xlsx('birds_clime/ACP_Station_Names_Locations.xlsx', sheet = 1, 
            col_names = T, na = 'NA')

stations_PosName <- stations_PosName[, c(1:4, 7:8)]

for (i in 5:6) stations_PosName[[i]] <- as.numeric(stations_PosName[[i]])


sampling <- read_xlsx('birds_clime/antswarm_coordinates_2022_23.xlsx', 
                      sheet = 1)[, 1:3]

for (i in 2:3) sampling[[i]] <- as.numeric(sampling[[i]])

colnames(sampling)[2:3] <- c('utm_E', 'utm_N')

apply(d, 2, function(x) sum(is.na(x)))

sampling <- full_join(sampling, d[, -2], by = 'observation_ID')

limsX <- quantile(sampling$utm_E, c(0, 1), na.rm = T)
limsY <- quantile(sampling$utm_N, c(0, 1), na.rm = T)

par(mar = c(4, 4, 2, 2))

stations_PosName %$%
  plot(`UTM (W)`, `UTM (N)`, ylim = c(95e4, 105e4), xlim = c(6e5, 685e3)
  )
sampling %$% points(utm_E, 
                    utm_N, col = ifelse(sampling$site == 'LIMB', 'tan1', 
                                        ifelse(sampling$site == 'SHER', 'cyan4', 
                                               ifelse(sampling$site == 'PLRD', 'yellow', 'green'))))


apply(sampling, 2, function(x) sum(is.na(x)))

sampling[is.na(sampling$utm_N), ]

sampling[grep('LEGEB01_01', sampling$observation_ID), ]
sampling[grep('ARPEB02_28', sampling$observation_ID), ]


sampling <- sampling[!is.na(sampling$utm_E), ]

# ==== choose stations ====

sum(is.na(stations_PosName$`UTM (N)`))

stations_PosName <- stations_PosName[!is.na(stations_PosName$`UTM (N)`) &
                                       !is.na(stations_PosName$`UTM (W)`), ]

apply(stations_PosName, 2, function(x) sum(is.na(x)))

sampling <- split(sampling, sampling$observation_ID)

filt_stations <- 
  lapply(sampling, FUN = 
           function(x) {
             tempX <- (x$utm_E - stations_PosName$`UTM (W)`)^2 +
               (x$utm_N - stations_PosName$`UTM (N)`)^2
             tempX <- sqrt(tempX)
             
             temp <- stations_PosName
             temp$dist <- tempX
             temp <- temp[order(temp$dist)[1:5], ]
             temp$observation_ID <- x$observation_ID
             temp$site <- x$site
             temp
           })

filt_stations$ARPEB02_28JUL2022

filt_stations <- do.call('rbind', filt_stations)
sampling <- do.call('rbind', sampling)

sampling %$% plot(utm_E, 
                    utm_N, col = ifelse(sampling$site == 'LIMB', 'tan1', 
                                        ifelse(sampling$site == 'SHER', 'cyan4', 
                                               ifelse(sampling$site == 'PLRD', 'yellow', 'green'))), 
                  ylim = c(95e4, 105e4), xlim = c(6e5, 685e3),
                  xlab = 'UTM (E)', ylab = 'UTM (N)')

stations_PosName %$%
  points(`UTM (W)`, `UTM (N)`, ylim = c(95e4, 105e4), xlim = c(6e5, 685e3)
  )

filt_stations %$% 
  points(`UTM (W)`, `UTM (N)`, col = 'red', pch = 16, 
                       cex = 0.5)
legend(x = 63e4, y = 98e4, 
       legend = c('LIMB', 'SHER', 'PLRD', 'JUAN', 'Stations', 'Choosen\n stations'), 
       col = c('tan1', 'cyan4', 'yellow', 'green', 'black', 'red'), 
       pch = c(rep(1, 4), 1, 16), ncol = 3)

colnames(d)[2] <- 'sampling_date'

fun_samp_stations <- function(indx = 1) {
  i <- which(sampling$observation_ID[indx] == filt_stations$observation_ID)
  
  x_lims <- filt_stations[i, ] %$% quantile(`UTM (W)`)[c(1, 5)]
  y_lims <- filt_stations[i, ] %$% quantile(`UTM (N)`)[c(1, 5)]
  
  plot(sampling$utm_E[indx], sampling$utm_N[indx], 
       main = paste(sampling$observation_ID[indx], 
                    sampling$site[indx], sep = ' ---> '), 
       xlab = 'UTM (W)', 
       ylab = 'UTM (N)', col = 'black', 
       xlim = c(x_lims[1] - 1e4, x_lims[2] + 1e4), 
       ylim = c(y_lims[1] - 1e4, y_lims[2] + 1e4))
  filt_stations[i, ] %$% points(`UTM (W)`, `UTM (N)`, col = 'red')
  filt_stations[i, ] %$% text(`UTM (W)`, `UTM (N)` + 1500, 
                              col = 'red', labels = NOMBRE)
}

fun_samp_stations(20)

t1 <- Sys.time()
saveGIF(
  {
    for (j in seq_along(sampling$observation_ID)) {
      fun_samp_stations(j)
    }
  }, movie.name = 'stations.gif', interval = 0.5)
Sys.time() - t1

filt_stations <- full_join(filt_stations, d, by = c('observation_ID', 'site'))

apply(filt_stations, 2, function(x) sum(is.na(x)))

filt_stations[is.na(filt_stations$NOMBRE), ]

filt_stations <- filt_stations[!is.na(filt_stations$NOMBRE), ]

filt_stations$sampling_date <- as.Date(filt_stations$sampling_date)

# ====== Functions: getting the data =====

nom_stations <- unique(filt_stations$NOMBRE)
nom_stations <- sort(nom_stations)

file_stations <- dir(paste(getwd(), '/birds_clime/clime_predictors', sep = ''))
file_stations <- paste(getwd(), 
                       '/birds_clime/clime_predictors/', 
                       file_stations,
                       sep = '')

find_months <- function(df = filt_stations, 
                        station = nom_stations[1]) {
 
  n <- unique(gsub('^(.*)(-)(.*)(-)(.*)$', '\\3', 
              df[df$NOMBRE == station, ]$sampling_date))
  message(station)
  sort(as.numeric(n))
}

climate_data <- function(file = file_stations[1], 
                       days = '([0-9]*/)',
                       extract_months = nom_stations[1], # only for our sampled months
                       extract_months2 = FALSE, # all months 
                       months_num = '([0-9]*)', # all months
                       years = '(/202[23])', 
                       name_station = nom_stations[1]) {
  
  if (extract_months2) {
    months <- months_num
  } else {
    months <- find_months(station = extract_months) 
    months <- paste('(0*[', min(months)-1, '-', max(months), '])', sep = '')
  }
  
  df <- as_tibble(read.csv(file))
  
  string <- paste(days, months, years, sep = '')
  
  df <- df[grep(string, df$date), ]
  
  df <- df[df$ra >= 0, ]
  
  df <- 
    df |> 
    group_by(date) |> 
    transmute(ra = sum(ra)) |> 
    unique() 
  
  df$date <- as.Date(df$date, '%d/%m/%Y')
  
  df$station <- name_station
  
  df
  
}

all_clime1 <- 
  lapply(seq_along(file_stations), FUN = 
           function(x) {
             climate_data(file = file_stations[[x]], 
                          extract_months = nom_stations[[x]], 
                          extract_months2 = TRUE, 
                          name_station = nom_stations[[x]], 
                          years = '(/20[12][0-9])')
           })

saveRDS(all_clime1, 'birds_clime/all_clime_per_month.rds')

all_clime1 <- readRDS('birds_clime/all_clime_per_month.rds')

all_clime1 <- 
  lapply(all_clime1, FUN = 
         function(x) {
           x |> 
             mutate(month = month(date), 
                    year = year(date)) |> 
             group_by(station, year, month) |> 
             transmute(ra = sum(ra)) |> 
             unique()
         })

all_clime1 <- do.call('rbind', all_clime1)

filt_stations2 <- unique(filt_stations[, c('observation_ID', "site", "NOMBRE")])

sum(filt_stations2$observation_ID == 'ARPEB06_15SEP2022')
sum(filt_stations2$observation_ID == 'JFCEB04_13SEP2022')

filt_stations2 <- 
  filt_stations2[filt_stations2$observation_ID != 'ARPEB06_15SEP2022' & 
               filt_stations2$observation_ID != 'JFCEB04_13SEP2022', ]

filt_stations2 <- filt_stations2[, -1]

sum(all_clime1$station == "Gatún")

colnames(filt_stations2)[2] <- 'station'

all_clime1 <- unique(full_join(all_clime1, filt_stations2, by = 'station'))

stations <- all_clime1

all_clime1[, c("station", 'site')] |>  
  unique() |> 
  group_by(site) |> 
  transmute(n = length(station)) |> 
  unique()

all_clime1 <- split(all_clime1, list(all_clime1$site, all_clime1$station))

all_clime1 <- all_clime1[unlist(lapply(all_clime1, function(x) nrow(x) != 0), use.names = F)]

all_clime1 <- all_clime1[-grep('SHER', names(all_clime1))[1]]

all_clime1 <- all_clime1[-grep('JUAN', names(all_clime1))[1]]

all_clime1 <- do.call('rbind', all_clime1)

all_clime1 <- 
  all_clime1 |> 
  group_by(site, month, station) |> 
  transmute(mu = mean(ra)) |> 
  ungroup() |> 
  group_by(site, month) |> 
  transmute(mu = sum(mu)) |> 
  unique()

apply(all_clime1, 2, function(x) sum(is.na(x)))

all_clime1[is.na(all_clime1$site), ]

cowplot::plot_grid(all_clime1 |> 
                     ggplot() +
                     geom_point(aes(month, mu, color = site), 
                                position = position_dodge(width = 0.5), 
                                size = 1) +
                     scale_x_continuous(breaks = 1:12) +
                     labs(y = 'Total rainfall (2010-2023)') +
                     geom_line(aes(month, mu, color = site), 
                               linewidth = 0.25, position = position_dodge(width = 0.5)), 
                   all_clime1 |> 
                     ggplot() +
                     stat_summary(aes(fct_reorder(site, mu, .fun = max), mu), 
                                  fun = 'sum', geom = 'bar') +
                     labs(y = 'Total rainfall (2010-2023)', x = NULL), 
                   ncol = 1)

ggsave('rainfall_site.jpg', width = 12, height = 15, units = 'cm', dpi = 500)

# ====== checking results =====

monthly_avg <- as_tibble(read.csv('rainfall_published.csv', 
                                  header = T, dec = ','))

nom_stations1 <- sort(unique(stations$station))

nom_stations2 <- colnames(monthly_avg)[-c(1:3)]

nom_stations2 <- nom_stations2[-c(76:length(nom_stations2))]

checking_stations <- 
  function(our_data = nom_stations1[1], 
           STRI_data = 'AGUAB', year = 2010) {
    d1 <- 
    unique(stations[stations$station == our_data &
                      stations$year == year, -5])$ra
    i <- grep(STRI_data, colnames(monthly_avg))
    
    d2 <- 
      monthly_avg[monthly_avg$Year == year, 
                  i, drop = T]
    
    tibble(year = year, 
           month = 1:12,
           `our data`= d1, 
           `STRI data`= d2, 
           ok = d1 == d2)
  }

n <- toupper(gsub('^(.*)\\s(.*)$', '\\1\\2', nom_stations1))

n[grep('Ú', n)] <- 'GATUN'

n %in% nom_stations2 # no todas las estaciones estan, pero verifiquemos 
                     # con las 13 que coinciden
nom_stations1 <- nom_stations1[n %in% nom_stations2]
n <- n[n %in% nom_stations2]

for (i in seq_along(n)) for (j in 2022:2023) {
  checking_stations(nom_stations1[i], 
                    STRI_data = n[i], 
                    year = j) |> print()
}

checking_stations(nom_stations1[5], STRI_data = 'CULEBRA', year = 2022)
checking_stations(nom_stations1[1], STRI_data = 'AGUAB', year = 2023)

checking_stations(nom_stations1[2], STRI_data = 'AGUASALUD', year = 2022)
checking_stations(nom_stations1[2], STRI_data = 'AGUASALUD', year = 2023)

checking_stations(nom_stations1[2], STRI_data = 'AGUASALUD', year = 2022)
checking_stations(nom_stations1[2], STRI_data = 'AGUASALUD', year = 2023)

d1 <- all_clime1 %$% aggregate(ra ~ site + month + year, FUN = mean)
d1
d1 %$% aggregate(ra ~ site + year, FUN = sum) 

#======== data for the paper ====== 

all_clime <- 
  lapply(seq_along(file_stations), FUN = 
           function(x) {
             climate_data(file = file_stations[[x]], 
                          extract_months = nom_stations[[x]], 
                          name_station = nom_stations[[x]], 
                          years = '(/20[12][0-9])')
           })

saveRDS(all_clime, 'birds_clime/all_clime.rds')

all_stations <- 
  lapply(seq_along(file_stations), FUN = 
           function(x) {
             climate_data(file = file_stations[[x]], 
                          extract_months = nom_stations[[x]], 
                          name_station = nom_stations[[x]])
           })

all_stations <- do.call('rbind', all_stations)

colnames(all_stations)[2] <- 'rainfall_mm'

d$sampling_date <- as.Date(d$sampling_date)

ggplot() +
  geom_line(data = all_stations, 
            aes(date, rainfall_mm, col = station), 
            linewidth = 0.1) +
  labs(x = NULL, y = 'Daily precipitation (mm)') +
  geom_jitter(data = tibble(y = rep(0, length(d$sampling_date)), 
                             x = d$sampling_date), aes(x, y), color = 'red', 
              alpha = 0.2) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid = element_blank())

all_stations$year <- gsub('^(202[23])(-)([0-9]*)(.*)$', '\\1', all_stations$date)
all_stations$month <- gsub('^(202[23])(-)([0-9]*)(.*)$', '\\3', all_stations$date)

write.table(all_stations |> 
              group_by(year, month, station) |> 
              transmute(rainfall_mm = sum(rainfall_mm)) |> 
              unique() |> 
              print(n = 268), 
            'rainfall_month_ant_followers.txt', row.names = F, col.names = T)

saveRDS(filt_stations, 'chosen_stations.rds')
saveRDS(all_stations, 'rainfall_ant_followers.rds')
write.table(all_stations, 'rainfall_ant_followers.txt', row.names = F, col.names = T)







