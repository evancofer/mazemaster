
package mud

class Exit(val dest:Int, val name:String) {}
object Exit {
  def apply(dest:String, name:String): Exit = {
    val n = dest.toInt;
    new Exit(n, name)
  }
}