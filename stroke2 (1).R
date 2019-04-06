# FUNCTIONS START

stroke = function(x1, y1, theta, len, width) {
  x = c(1:16)
  y = c(1:16)
  
  x = x - x1
  y = y - y1
  u1 = x * cos(theta)
  u2 = y * sin(theta)
  #u=ones %*% t(u1) + t(ones %*% t(u2))
  u = matrix(u1, 16, 16) + t(matrix(u2, 16, 16))
  
  
  v1 = -x * sin(theta)
  v2 = y * cos(theta)
  #v=(ones %*% t(v1) + t(ones %*% t(v2)))
  v = matrix(v1, 16, 16) + t(matrix(v2, 16, 16))
  
  m = (v > 0 & v < len)
  exp(-u * u / width) * m
}

dist = function(a, b) {
  sum((a - b) ^ 2)
}

# Randomly generate parameters within the given ranges
generate_params = function(param_space) {
  min_ = param_space[1, ]
  max_ = param_space[2, ]
  
  rands = runif(dim(param_space)[2])
  
  return((max_ - min_) * rands + min_)
  
}

# Clip value between min & max values
clip = function(x, min_val, max_val) {
  return(max(min(x, max_val), min_val))
}

# Element wise clip for vectors
clip_vector = function(x, min_vals, max_vals) {
  indices = 1:length(x)
  return(sapply(
    indices,
    FUN = function(i) {
      clip(x[i], min_vals[i], max_vals[i])
    }
  ))
}

# Draws a picture given the stroke params
draw_strokes = function(strokes) {
  im = strokes_to_image(strokes)
  display_image(im)
}

temp_linear = function(p, cutoff = 0.8) {
  # p: proportion of iterations
  return(max(0, 1 - p / cutoff))
}

temp_exp = function(p, rate=5){
  # TODO
  return(2/(1+exp(p*rate)))
}
  
# Converts given stroke parameters into an image
strokes_to_image = function(strokes) {
    # Images for each stroke
    stroke_pixels = apply(
      strokes,
      MARGIN = 1,
      FUN = function(x) {
        do.call("stroke", as.list(x))
      }
    )
    
    # Combine strokes
    im = matrix(apply(stroke_pixels, MARGIN = 1, max), 16, 16)[, 16:1]
    
    return(im)
  }

# Shows single image. dim: (pixels)
display_image = function(flat_img, new = T, col = grey) {
  if (new) {
    par(mfrow = c(1, 1))
  }
  
  if (is.null(dim(flat_img))) {
    width = sqrt(length(flat_img))
  } else {
    width = sqrt(prod(dim(flat_img)))
  }
  
  colour = col((1:256) / 256)
  image(matrix(flat_img, width, width)[, width:1], col = colour)
}

imfit = function(z,
                 n_iterations,
                 temp_alpha=5,
                 step_size = 0.1,
                 best_params = NULL) {
  # initial starting parameters
  if (is.null(best_params)) {
    for (i in 1:2) {
      best_params = rbind(best_params, generate_params(param_space))
      
    }
  }
  
  best_err = Inf
  
  
  errors = NULL
  for (i in c(1:n_iterations))
  {
    
    
    # Randomly perturb best params
    # Random proportion
    changes = matrix((runif(10) - 0.5) * 2 * step_size,
                     nrow = 2,
                     ncol = 5)
    # Scale to param range
    changes = t(apply(
      changes,
      MARGIN = 1,
      FUN = function(x) {
        (param_space[2, ] - param_space[1, ]) * x
      }
    ))
    
    # Clip them into the given range
    # Note: this does not work well for theta
    cur_params = t(apply(
      best_params + changes,
      MARGIN = 1,
      FUN = function(x) {
        clip_vector(x, param_space[1, ], param_space[2, ])
      }
    ))
    
    
    # Generate image & compute error
    im = strokes_to_image(cur_params)
    cur_err = dist(im, z)
    
    # Was getting some strange NAN errors when testing
    if (is.na(cur_err)) {
      print(im)
    }
    
    
    # If current params are better, or probability of annealing passes,
    # save current params as best
    # anneal_threshold = temp_exp(i / n_iterations, temp_alpha)
    anneal_threshold = exp((best_err - cur_err)/(temp_alpha * (1 - i/n_iterations)))
    if ((cur_err < best_err) | (runif(1) < anneal_threshold)) {
      best_err = cur_err
      best_params = cur_params
      # print(best_err)
      errors = rbind(errors, c(i, best_err))
    }
    
    # anneal_threshold = anneal_threshold * (1 - alpha)
  }
  
  return(list("params" = best_params,
              "error" = best_err,
              "error_hist"=errors))
}


flatten_results = function(res){
  return(c(res$params, res$error))
}

# FUNCTIONS END
library(foreach)
library(doParallel)
num_cores = parallel::detectCores()
doParallel::registerDoParallel(makeCluster(num_cores-1))


d1 = scan("data/digits/Zl1d.dat", nlines = 1000, n = 256000)
d1 = t(matrix(d1, 256, 1000)) / 256


param_space = matrix(c(0, 16, 0, 16, 0, 2 * pi, 5, 22, 1, 10), 2, 5)
# Roughly place starting strokes for 1
initial_params = t(matrix(c(14, 14, pi, 10, 3, 14, 14, pi / 2, 10, 3), 5, 2))


# Single digit
z = matrix(d1[1,], 16, 16)
res = imfit(z, 10000, alpha = 0.005)
draw_strokes(res$params)
display_image(z)


# PARALLEL



# foreach & doParallel
system.time({
  
res = foreach(i = 1:1000, .export = "stroke", .combine=rbind) %dopar% {
      # foreach(a = 1, .combine=cbind) %dopar% {
  z = matrix(d1[i, ], 16, 16)
  r = imfit(z, 10000, step_size = 0.05, best_params = initial_params, temp_alpha = 1)
  
  # Append to file
  # flat = flatten_results(res)
  # write.table(as.matrix(t(flat)), file = "fit_data.csv", append=T, col.names = F, sep=",", row.names=i)
  return (r)
}

})

draw_strokes(res[9,]$params)
display_image(d1[1,])




# Error vs iterations vs alpha
## Set up plot
plot(c(0, 10000), c(0, 100),
     col="white", xlab = "iteration", ylab="Error")
## Draw lines
for (i in 1:20){
  lines(res[i,]$error_hist, col=i, type="s")
}
## Draw legend
legend(10000, 90, (1:20)*0.1, cex=0.5, col=1:20,
       lty=1:20, title="Alpha", xpd = T)


# Plot temperature formula
i = c(1:1000)
plot(i, exp((40 - 41)/(1 * (1-i/1000))), xlab="iteration", ylab="Prob of Taking Worse Params")


# Plot best_error vs temp_alpha
plot(c(-10:10)[c(-10:10) != 0] * 0.1, errors, ylim = c(0, max(errors)), 
     type="h", xlab="Alpha", ylab="Best Error")

# Plot best_error vs n_iterations
barplot(errors, names.arg = c(100, 1000, 5000, 10000, 50000),
         xlab="Iterations", ylab="Best Error")


# Plot error over iterations
plot(c(0, 100000), c(5, 40),
     col="white", xlab = "iteration", ylab="Error")
lines(res[7,]$error_hist, col=1, type="s")


# Plot best_err vs iterations
errors = matrix(as.numeric(res[(1:5)*3-1,]), 5, 20)
errors = apply(errors, MARGIN = 2, mean)

sum(c(100, 1000, 5000, 10000, 50000)) * 10


errors = NULL
for (i in 1:1000){
  errors = c(errors, res[i,]$error)
}
