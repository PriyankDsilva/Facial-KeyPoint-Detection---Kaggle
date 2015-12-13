##function to get matrix size
getxy <- function(x,y) {
  
  #x=80.22713
  x.dot <- 25+ (x-floor(x))
  #y=32.22814
  y.dot <- 25 + (y-floor(y))
  
  x1<-(floor(x)-24):((floor(x)+24)-1)
  y1<-(floor(y)-24):((floor(y)+24)-1)
  
  if(min((floor(x)-24):((floor(x)+24)-1)) < 1) {
    x.dot <- 0;    y.dot <- 0
  }
  if(min((floor(y)-24):((floor(y)+24)-1)) < 1) {
    x.dot <- 0;    y.dot <- 0
  }
  
  if(max((floor(x)-24):((floor(x)+24)-1)) > 96) {
    x.dot <- 0;    y.dot <- 0
  }
  if(max((floor(y)-24):((floor(y)+24)-1)) > 96) {
    x.dot <- 0;    y.dot <- 0
  }
  
  return (c(x1,y1,x.dot,y.dot))
}



################################################################################
#Multiply the training data                      patchs
training.data.patch.im <-NULL
training.data.patch.points <- NULL
for (i in 1:dim(training.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  #i=2
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  training.data.patch.im = rbind(training.data.patch.im,as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE)))
  
  point <- training.data[i,1:30]
  training.data.patch.points = rbind(training.data.patch.points,(point/2))
  
  #j=1
  for (j in seq(from = 1, to = 30, by = 2)){
    if (is.na(point[,j:(j+1)])==FALSE) {
      
      xy <- getxy(point[,j],point[,(j+1)])
      
      if (xy[97:98]!=c(0,0)) {
        im2<-im[xy[1:48],xy[49:96]]
        training.data.patch.im = rbind(training.data.patch.im,as.data.frame(matrix(im2, ncol = 2304, byrow = TRUE)))
        new.points <- point
        new.points[,j] <- xy[97]
        new.points[,(j+1)] <- xy[98]
        new.points[,setdiff(1:30,c(j,j+1))] <- NA
        training.data.patch.points = rbind(training.data.patch.points,new.points)
        }
    }
  }
}

dim(training.data.patch.im)

dim(training.data.patch.points)

##display the images
layout = layout(matrix(1:4, nc = 2)) 

for (i in 1:dim(training.data.patch.im[1:8,])[1]){
  im.train <- training.data.patch.im[i,]
  d.train <- training.data.patch.points[i,]
  
  
  im <- matrix(rev(as.integer(im.train)), nrow=48, ncol=48)
  image(1:48, 1:48, im, col=gray((0:255)/255))
  
  ##Given Data for the Image
  points(48-d.train$left_eye_center_x,         48-d.train$left_eye_center_y,         col="green")
  points(48-d.train$right_eye_center_x,         48-d.train$right_eye_center_y,         col="green")
  points(48-d.train$nose_tip_x,         48-d.train$nose_tip_y,         col="green")
  
  points(48-d.train$left_eye_inner_corner_x,         48-d.train$left_eye_inner_corner_y,         col="green")
  points(48-d.train$left_eye_outer_corner_x,         48-d.train$left_eye_outer_corner_y,         col="green")
  
  points(48-d.train$right_eye_inner_corner_x,         48-d.train$right_eye_inner_corner_y,         col="green")
  points(48-d.train$right_eye_outer_corner_x,         48-d.train$right_eye_outer_corner_y,         col="green")
  
  points(48-d.train$left_eyebrow_inner_end_x,         48-d.train$left_eyebrow_inner_end_y,         col="green")
  points(48-d.train$left_eyebrow_outer_end_x,         48-d.train$left_eyebrow_outer_end_y,         col="green")
  
  points(48-d.train$right_eyebrow_inner_end_x,         48-d.train$right_eyebrow_inner_end_y,         col="green")
  points(48-d.train$right_eyebrow_outer_end_x,         48-d.train$right_eyebrow_outer_end_y,         col="green")
  
  
  points(48-d.train$mouth_center_top_lip_x,         48-d.train$mouth_center_top_lip_y,         col="green")
  points(48-d.train$mouth_center_bottom_lip_x,         48-d.train$mouth_center_bottom_lip_y,         col="green")
  points(48-d.train$mouth_left_corner_x,         48-d.train$mouth_left_corner_y,         col="green")
  points(48-d.train$mouth_right_corner_x,         48-d.train$mouth_right_corner_y,         col="green")
}



