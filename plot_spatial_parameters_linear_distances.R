library(geoR)
library(geosphere)

#script written by X

#two sets
cols = c(brewer.pal(6, "Dark2"))

# Assume these are kilometers
n_points = 2500 #with this many points (233) we achieve roughly the minimal distance that corresponds to the minimal distance in our small sample data
longitude = seq(from = -90, to = 90, length.out = n_points)  #with this span we achieve roughly the maximum distance between points that corresponds to the one found in our data
latitude  = rep(0, n_points)
df = data.frame(latitude = latitude, 
                longitude = longitude)

parameters = data.frame(kappa = c(1, 1),
                        phi = c(1.25, 17))

parameters$name = c(sprintf("phi%.2fkappa%.2f", parameters$phi, parameters$kappa))

#parameters <- parameters %>%
#  filter(kappa==0.5)


## Covariance matrix

spatial_parameters = map2(parameters$kappa, parameters$phi,function(k, p){
  spatial_covar_mat = varcov.spatial(coords = df,
                                     cov.pars = c(1, p), 
                                     kappa = k, 
                                     cov.model= "matern")$varcov
  spatial_covar_mat
})

## Distance matrix (in km - so divide by 1000)
dist_data = as.matrix(df[,c("longitude", "latitude")], ncol = 2)
dist_matrix = distm(dist_data, fun = distHaversine) / 1000

euclidean_dist = geosphere::distm(df[,c("longitude", "latitude")],
                                  fun = distHaversine) 
dimnames(euclidean_dist) = list(c(1:n_points), c(1:n_points))
# scale 
euclidean_dist = scales::rescale(euclidean_dist)


diag(dist_matrix)

distance = round(dist_matrix[lower.tri(dist_matrix)], 4)
transformed_dist = round(euclidean_dist[lower.tri(euclidean_dist)], 4)

datafr <- as.data.frame(cbind(distance, transformed_dist))
datafr$covariance <- rep(0,1) #a scale from 0 to 1 to make sure we have a "y axis" to which spatial parameter lines will later be plotted 

plot_n = 1000

sample_idx = ceiling(seq(1, nrow(datafr)-1, length.out = plot_n))

plot_ss = datafr[sample_idx,]
plot_ss$index = sample_idx
plot_ss = plot_ss[order(plot_ss$distance),]

spatialkappa_lines = lapply(spatial_parameters, function(x) {
  d = sort(c(x[lower.tri(x)]), decreasing = TRUE)
  sample_idx = seq(1, length(d), length.out = plot_n)  
  d[sample_idx]
})

legend_text = c(paste0("local: kappa = ", parameters[1,1],"; phi = ", parameters[1,2]),
                paste0("regional: kappa = ", parameters[2,1],"; phi = ", parameters[2,2]))

png("output/plot_spatial_pars_km.png", width = 8, height = 8, res = 400, units = "in")
plot(x = plot_ss$distance, y = plot_ss$covariance, 
     type = "l", main = "Spatial parameters", col = "white", #not plotting these lines; just keeping to axis
     ylim = c(0, 1),
     xlim = c(0, 15000),
     xlab = "Distance (km)",
     ylab = "Covariance",
     frame.plot = TRUE,
     cex.main=1.7, 
     axes=FALSE,
     cex.lab=1.5)
axis(1, at = seq(0,15000,by=2500), labels = seq(0,15000,by=2500), tick = TRUE, cex.axis=1.4)
axis(2, at = seq(0,1,by=0.2), labels = seq(0,1,by=0.2), tick = TRUE, cex.axis=1.4)
for(i in seq_along(spatialkappa_lines)){
  lines(x = plot_ss$distance, y = spatialkappa_lines[[i]], col = cols[i], lwd = 2)
}

legend("topright", 
       legend=legend_text,
       col=cols, lty=1, cex=1.5, lwd = 3)
x <- dev.off()


png("output/plot_spatial_pars_km_zoomed.png", width = 8, height = 8, res = 400, units = "in")
plot(x = plot_ss$distance, y = plot_ss$covariance, 
     type = "l", main = "Spatial parameters", col = "white", #not plotting these lines; just keeping to axis
     ylim = c(0, 1),
     xlim = c(0, 10000),
     xlab = "Distance (km)",
     ylab = "Covariance",
     frame.plot = TRUE,
     cex.main=1.7, 
     axes=FALSE,
     cex.lab=1.5
)
axis(1, at = seq(0,10000,by=2000), labels = seq(0,10000,by=2000), tick = TRUE, cex.axis=1.4)
axis(2, at = seq(0,1,by=0.2), labels = seq(0,1,by=0.2), tick = TRUE, cex.axis=1.4)

for(i in seq_along(spatialkappa_lines)){
  lines(x = plot_ss$distance, y = spatialkappa_lines[[i]], col = cols[i], lwd = 2)
}

legend("topright", 
       legend=legend_text,
       col=cols, lty=1, cex=1.5, lwd = 3)
x <- dev.off()


