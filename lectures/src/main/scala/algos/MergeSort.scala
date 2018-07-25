package algos

object MergeSort extends App{

  def mergeSort[T](x: List[T]): List[T] =  {
    if (x.length/2 == 0) x
    else {
      mergeSort()
    }
  }

}
