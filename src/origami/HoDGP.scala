package origami

object HoDGP {

   def id[a] = (x:a) => x
  
  // support for type-constructor polymorphism in Scala is bad ... 
  // no partial applications allowed!
  //trait Fix[F[_,_],a]
  //case class In[F[_,_],a](out : F[a,Fix[F,a]]) extends Fix[F,a]
  
  case class Fix[F[_,_],a] (out : F[a,Fix[F,a]]) 
  
  trait BiFunctor[F[_,_]] {
    def bimap[a,b,c,d]  : (a => b) => (c => d) => F[a,c] => F[b,d]
    def fmap2[a,c,d]    : (c => d) => F[a,c] => F[a,d] = bimap(id)
  }
  
  def cataBad[a,r,F[_,_]] (f : F[a,r] => r) (implicit ft : BiFunctor[F]) : Fix[F,a] => r = 
    f compose ft.fmap2(cataBad[a,r,F](f)) compose (_.out)
  
  def cata[a,r,F[_,_]] (f : F[a,r] => r) (t : Fix[F,a]) (implicit ft : BiFunctor[F]) : r = 
    f(ft.fmap2(cata[a,r,F](f))(t.out))
  
  def ana[a,r,F[_,_]] (f : r => F[a,r]) (x:r) (implicit ft : BiFunctor[F]) : Fix[F,a] = 
    Fix[F,a](ft.fmap2(ana[a,r,F](f))(f(x))) // why do I need _ here, but not in build
  
  def hylo[a,b,c,F[_,_]] (f : a => F[c,a])(g : F[c,b] => b)(x:a)(implicit ft : BiFunctor[F]) : b =
    g(ft.fmap2(hylo[a,b,c,F](f)(g))(f(x)))
  
  def build[a,F[_,_]] (f : {def apply[b] : (F[a,b] => b) => b}) = // no need for return type
    f.apply(Fix[F,a])
  
  def map[a,b,F[_,_]] (f : a => b) (t : Fix[F,a]) (implicit ft : BiFunctor[F]) : Fix[F,b] =
    Fix[F,b](ft.bimap(f)(map[a,b,F](f))(t.out))
  
  // the use of anonymous case analysis is nice
  
  trait ListF[a,r]
  case class Nil[a,r]() extends ListF[a,r]
  case class Cons[a,r](x : a, xs : r) extends ListF[a,r]
  
  implicit object biList extends BiFunctor[ListF] {
    def bimap[a,b,c,d] = f => g => {
      case Nil()       => Nil()
      case Cons(x,xs)  => Cons(f(x), g(xs))
    }
  }
  
  type List[a] = Fix[ListF,a]
  
  def nil[a]  : List[a] = Fix[ListF,a](Nil())
  def cons[a] = (x:a) => (xs:List[a]) => Fix[ListF,a](Cons(x,xs))
  
  def sumList : List[Int] => Int = cata[Int,Int,ListF] {
     case Nil()      => 0
     case Cons(x,n)  => x + n
  }
  
  // Some tests
  val t : List[Int] = cons(4)(cons(2)(cons(1)(nil)))
  val test : Int = sumList(t)
  
  def enumTo : Int => List[Int] = ana[Int,Int,ListF] (x => if (x==0) Nil() else Cons(x,x-1))
  
  def func[b] : (ListF[Int,b] => b) => b = f => f(Cons(2,f(Cons(1,f(Nil())))))
  
  def funny : List[Int] = build[Int,ListF](new {def apply[b] = func[b]})
  
  def testHylo = hylo[Int,Int,Int,ListF](x => if (x==0) Nil() else Cons(x,x-1))({case Nil() => 0; case Cons(x,n) => x + n})(4)
 
  def main(args: Array[String]): Unit = {
    println(test)
  }

}