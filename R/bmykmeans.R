#' @title K-mean cluster using R
#' @description Calculate the center of K-mean cluster using R and return the cluster of each point
#' @param dataset a data matrix
#' @param k the number of cluster
#' @return Cluster center for each class and the cluster name of each point
#' @examples
#' \dontrun{
#' dataset<-iris[,1:2]
#' k<-3
#' z<-bmykmeans(dataset,k)
#' z
#' }
#' @export
bmykmeans<-function(dataset,k){
  rowdata<-nrow(dataset)
  continue.change=TRUE
  
  #随机挑选三个点作为初始聚类点
  formerPoint<-dataset[sample.int(rowdata,size = k),]
  iterPoint<-matrix(0,nrow = k,ncol = ncol(dataset))
  
  #记录每一个点到每一个类的距离
  error.matrix<-matrix(0,nrow=rowdata,ncol=k)
  while(continue.change){
    #记录每个点所属的类是哪一个
    cluster.matrix<-matrix(0,nrow=rowdata,ncol=k)
    for(i in 1:rowdata){#计算每个点到三个初始中心点的距离
      for(j in 1:k){
        error.matrix[i,j]<-sqrt(sum((dataset[i,]-formerPoint[j,])^2))
      }
    }
    #将每一个点所属的类计算出来
    for(i in 1:rowdata){
      cluster.matrix[i,which.min(error.matrix[i,])]<-1
    }
    
    #更新新的质心位置
    for(i in 1:k){
      iterPoint[i,]<-apply(dataset[which(cluster.matrix[,i] == 1),],2,"mean")
    }
    all.true<-c()
    
    #判断中心点是否已经保持不变
    for(i in 1:k){
      if(all(formerPoint[i,] == iterPoint[i,]) == T){
        all.true[i]<-TRUE
      }
    }
    formerPoint = iterPoint
    continue.change=ifelse(all(all.true) == T,F,T)
  }
  colnames(iterPoint)<-colnames(dataset)
  result=list()
  result[["centers"]]<-iterPoint
  result[["distance"]]<-error.matrix
  result[["cluster"]]<-rep(1,rowdata)
  for(i in 1:rowdata){
    result[["cluster"]][i]<-which(cluster.matrix[i,] == 1)
  }
  #返回结果，包括中心点坐标，每个点离每一个中心点的位置以及每个数据点所属的聚类名称
  return(result)
}

