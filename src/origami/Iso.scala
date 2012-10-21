package origami

trait Iso [A,B] { // from and to are inverses
  def from:A=>B
  def to :B=>A
}