# FUNCTIONS START # 

# Returns indices for k nearest neighbours
knn = function(dataset, data_point, k=5, dist_func=euclidean2){
  dists = apply(dataset, 1, dist_func, data_point)
  k_nearest = order(dists)[1:k]
  
  return(list(
    "index"=k_nearest,
    "dists2"=dists[k_nearest]
  ))
}

gaussian_dist = function(dist2, sigma2=100000){
  return(exp(-dist2 / sigma2))
}


# Full classification using knn
knn_classify = function(data, classes, point, ...){
  knn_results = knn(data, point, ...)
  indices = knn_results$index
  dists2 = knn_results$dists2
  gau = sapply(dists2, gaussian_dist)
  classes = classes[indices]
  
  # AGGREGATE HERE
  class_weights = data.frame(list("class"=classes, "gau"=gau))
  agg_class_weights = aggregate(class_weights, by=list(class_weights$class), FUN=sum)
  
  # MANUAL MAX
  max_i = 1
  for(i in 1:nrow(agg_class_weights)){
    if (agg_class_weights[i, "gau"] < agg_class_weights[max_i, "gau"]){
      max_i = i
    }
  }
  
  class = agg_class_weights[max_i, "class"]
  
  # class = agg_class_weights[which.max(agg_class_weights["gau"]), "class"]
  
  
  return(class)
}

knn_classify_many = function(data, classes, points, ...){
  swap_knn = function(point, classes, data, ...){
    # Just swaps around arguments for apply to work
    return(knn_classify(data, classes, point, ...))
  }
  
  return(apply(points, 1, swap_knn, classes, data, ...))
  
  
}

euclidean2 = function(x, y){
  return(sum((x - y) ^ 2))
}

cosine_sim = function(x, y){
  return(sum(x*y) / sqrt(euclidean2(x, 0) * euclidean2(y, 0))) 
}

# Generates frequency table
freq = function(data){
  values = unique(data)
  counts = NULL
  for (val in values){
    c = length(data[data==val])
    counts = c(counts, c)
  }
  
  return(list(
    "values"=values, 
    "counts"=counts
    ))
}

# Frequency table using table()
freq2 = function(data){
  t = table(data)
  return(list(
    "values"=as.numeric(names(t)),
    "counts"=t
  ))
}

# Returns value with max frequency
freq_max = function(data){
  freqs = freq2(data)
  return(freqs$values[which.max(freqs$counts)])
}


confusion_matrix = function(targets, predicts){
  
  classes = sort(unique(targets))
  mat = matrix(0, length(classes), length(classes), dimnames=list(classes, classes))
  
  for (i in classes){
    pred_c = predicts[targets==i]
    tab_pred_c = table(pred_c)
    mat[as.character(i), names(tab_pred_c)] = tab_pred_c
    
  }
  
  return(mat)
}


evaluate = function(targets, predicts){
  confusion = confusion_matrix(targets, predicts)
  classes = colnames(confusion)
  # recall
  recall = sapply(classes, function(c) confusion[c, c] / sum(confusion[,c]))
  
  
  
  # precision
  precision = sapply(classes, function(c) confusion[c, c] / sum(confusion[c,]))
  
  
  # f1
  
  return(list("precision"=precision, "recall"=recall))
}

display_image = function(flat_img, new=T){
  if(new){
    par(mfrow=c(1,1))
  }
  
  width = sqrt(length(flat_img))
  image(matrix(flat_img, width, width)[,width:1])
}

display_images = function(flat_imgs){
  n = dim(flat_imgs)[1]
  rows = ceiling(sqrt(n))
  par(mfrow=c(rows, rows), mar=c(1,1,1,1))
  for (i in 1:n){
    display_image(flat_imgs[i,], new=F)
  }
}


gaussian = function(data, point){
  # Returns probability that a given data distribution would generate a point
  d_var = apply(data, 2, var)
  d_mean = apply(data, 2, mean)
  
  norm_point = (point - d_mean) / sqrt(d_var)
  
  # This determines how probability is gotten
  z_avg = ;
  
}
# FUNCTIONS END #






# Read in digits
data_dir = "./data/digits/"
fnames = list.files(path = data_dir, pattern = "*.dat", full.names = TRUE)

nums_per_file = 1000
classes = NULL
digits = NULL
for (i in 1:length(fnames)){
  raw_nums = scan(fnames[i], nlines=nums_per_file, n=256 * nums_per_file)
  digits = cbind(digits, raw_nums)
  classes = c(classes, rep(i-1, nums_per_file))
  
}
digits = t(matrix(digits, 256, 10 * nums_per_file))


# knn single case
knn_classify(digits[-1,], classes, digits[1,])
knn_classify(digits, classes, digits[1,])


# Shuffle & split 
rand_inds = sample(1:nrow(digits))
test_set = 1:2000
train_set = 2001:10000

# knn many cases
p = knn_classify_many(digits[rand_inds[train_set],], classes, digits[rand_inds[test_set],], k=11)

evaluate(classes[rand_inds[test_set]], p)
display_image(confusion_matrix(classes[rand_inds[test_set]], p))

# TEST #
x = data.frame(
list(
  "a"=rep(1:10, 10),
  "b"=101:200
))

apply(x, 2, gaussian_dist)

class(digits[-1,])



