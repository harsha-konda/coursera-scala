var  l = 1::2::List()
4::l
5::l


def insert(x:Int,l:List[Int]):List[Int] =l match {
  case List() => List(x)
  case h::tail => if(x<h) x::l else h::insert(x,tail)
}

def isort(l:List[Int]) :List[Int] = {
  def isort(x:List[Int],accum:List[Int]):List[Int] = x match {
    case List() => accum
    case x::tail => isort(tail,insert(x,accum))
  }

  isort(l,List())
}

isort(List(4,3,2,1))