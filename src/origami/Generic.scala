package origami

trait Generic[G[_]] {

  def unit :G[Unit]
  def int :G[Int ]
  def char :G[Char]
  def plus [a,b] :G[a]=>G[b]=>G[Either [a,b]]
  def prod [a,b] :G[a]=>G[b]=>G[(a,b)]
  def view[a,b] : Iso [b,a]=>(=>G[a])=>G[b]
  
}