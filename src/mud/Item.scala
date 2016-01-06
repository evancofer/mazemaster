
package mud
import scala.xml.Node

class Item(private val mname: String, val description: String) {
  def name:String = mname
  def itemToXML(): Node = {
    <item name={ name }>
      <des>{ description }</des>
    </item>
  }
}
object Item {
  def apply(node: Node): Item = {
    val name = (node \ "@name").text
    val desc = (node \ "des").text
    new Item(name, desc)
  }
}