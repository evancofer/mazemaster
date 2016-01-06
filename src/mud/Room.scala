
package mud

import scala.xml.XML
import scala.xml.Node
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

class Room(val name:String, val description:String, private var mItems:scala.collection.mutable.ListBuffer[Item], private val exits: Map[String,Exit]) extends Actor {
  private var mPlayers = scala.collection.mutable.ListBuffer[ActorRef]()
  def receive = {
    case an_item:Item => mItems+=an_item
    case look:LookReq=> sender ! printScript()
    case it:ItemReq=>{
       val s = sender
       try{
         val temp = mItems(mItems.indexWhere(_.name.trim.equalsIgnoreCase(it.name)))
         s ! temp
         mItems-=temp
       } catch {case e:IndexOutOfBoundsException => s ! "No such item."}
    }
    case dest:MoveReq => {
      val s = sender
      mPlayers-=s
      try {
        Room.mapRooms(exits(dest.name).dest) ! s 
        s ! Room.mapRooms(exits(dest.name).dest)
      } catch {case e:NoSuchElementException => s ! "You cannot go that way."}
      
    }
    case plyr:ActorRef=>{
      plyr ! printScript()
      mPlayers+=plyr
    }
    case chat:String=>mPlayers.map(_ ! chat)
    case _=>println("""*Unknown Message to Room """"+name+"""".* """)
  }
  def printScript():String = {
    var ret = name + ": \n"+description+"\n"+"From here you may go: "+exits.map(x=>x._1+" to "+x._2.name).mkString("\n")
    if(!mItems.isEmpty) ret = ret + "\n"+ "Spread about the area you can see: \n" + mItems.map(_.name).mkString("\n") 
    ret
  }
  def roomToXML(): Node = {
    <room name={ name }>
      <des>{ description }</des>
      { mItems.map(i => i.itemToXML) }
      <exit>{ exits.mapValues(_.dest.toString()).mkString(",")}</exit>
      <directions>{ exits.keys.mkString(",") }</directions>
			<destinations>{exits.mapValues(_.name).mkString(",")}</destinations>
    </room>
  }
}
object Room {
  val mapRooms = readMap()
  def readMap(): Array[ActorRef] = {
    val rooms_xml = XML.loadFile("map.xml")
    (rooms_xml \ "room").map(n => Room(n)).toArray
  }
  def apply(node: Node):ActorRef = {
    val name = (node \ "@name").text; 
    val uid = (node \ "UID").text;
    val des = { node \ "des" }.text;
    val items = (node \ "item").map(n => Item(n)).toList.to[scala.collection.mutable.ListBuffer];
    val destinations = (node \ "destinations").text.split(",").toArray;
    val exits = (node \ "exit").text.split(",").zip(destinations).map(s=>Exit(s._1, s._2));
    val exitmap:Map[String,Exit] = (node \ "directions").text.split(",").toArray.zip(exits).toMap;
    Main.actsystem.actorOf(Props(classOf[Room],name, des, items, exitmap), uid)
  }}
case class LookReq(){}
case class ItemReq(val name:String){}
case class MoveReq(val name:String){}
//case class WhoReq(val name:String){} TODO For finding out who's in a room.
case class RenameReq(val name:String, val newName:String, val player:ActorRef){}
case class Chat(val name:String, val message:String){}
case class Listing(val name:String, val player:ActorRef){}
case class Command(val name:String){}
case class UpdateReq(){}