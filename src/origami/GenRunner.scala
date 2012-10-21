package origami

// See http://debasishg.blogspot.com/2011/07/datatype-generic-programming-in-scala.html
object GenRunner {

  case class Size [A] (size :A=>Int)
  
  trait MySize extends Generic[Size]{
    def unit = Size (x => 0)
    def int = Size (x => 0)
    def char = Size (x => 0)
    def plus [a,b] = a => b => Size (_.fold (a.size,b.size))
    def prod [a,b] = a => b => Size (x => a.size(x._1) + b.size (x._2))
    def view[a,b] = iso => a => Size (x => a.size (iso.from (x)))
  }
  
  def main(args: Array[String]): Unit = {
    
    
  }

}