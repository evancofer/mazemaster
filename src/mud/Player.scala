
package mud
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.ActorSystem
import java.net.Socket
import java.io.PrintStream
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

class Player(private var mName:String, private var mRoom:ActorRef, private val sock:Socket) extends Actor{
   mRoom ! LookReq()
   private val ticker = context.system.scheduler.schedule(100 millis, 100 millis, self, UpdateReq())
   def name:String = mName
   private var mItems = scala.collection.mutable.ListBuffer[Item]()
   private val ps = new PrintStream(sock.getOutputStream());
   private val is = sock.getInputStream();
   def print(s:String):Unit = ps.printf("<"+new java.util.Date+">:"+s+"\n")
   def receive = {
      case cmnd:Command => takeCommand(cmnd.name)
      case it:Item => mItems+=it
      case i:ActorRef => {
        mRoom ! ("""Player """"+name+"""" is leaving zone. """)
        mRoom = i
        mRoom ! ("""Player """"+name+"""" is entering zone.""")
      }
      case s:String => print(s)
      case u:UpdateReq=> readLine().foreach(s=> takeCommand(s))
      case _=>println("")
  }
   
  private def drop(item:String):Unit = {
    try{
     mRoom ! mItems(mItems.indexWhere(_.name.equalsIgnoreCase(item)))
     mItems.remove(mItems.indexWhere(_.name.equalsIgnoreCase(item)))
    } catch {case e:IndexOutOfBoundsException => print("No such item.")}
  }
  
  private def pickup(item:String):Unit = { mRoom ! ItemReq(item) }
  
  private def rename(name: String): Unit = {
    name.replace(" ", "_")
    mRoom ! (mName + " has changed their name to " + name)
    Main.chatServe ! RenameReq(mName,name,self)
    mName = name
  }
  
  private val command = Map[String,(String)=>Unit]( 
        "Help" -> ((args:String)=>print("Commands:\nGo direction = travel from this zone in the specified direction.\ndirection = travel from this zone in the specified direction.\nLook = reprints the description of the current room.\nInventory = lists current player inventory\nInv = lists current player inventory\nGet item = get an item from the room and add it to your inventory.\nDrop item = drop an item from your inventory into the current room.\nQuit = quits the game\nExit = quits the game\nSave = saves the current game.\nLoad = loads a different game. Make sure to save before changing games.")),
        "Look" ->((args:String)=>mRoom ! LookReq()),
        "Drop" ->((args:String)=>drop(args)),
        "Get" ->((args:String)=>pickup(args)),
        "Inv" ->((args:String)=>print(mItems.map(x=>"\n"+x.name+":"+x.description).mkString+"\n")),//Player.currentPlayer.printInventory
        "Inventory" ->((args:String)=>print(mItems.map(x=>"\n"+x.name+":"+x.description).mkString+"\n")),
        "Exit"->((args:String)=>System.exit(0)),
        "East"->((args:String)=>mRoom ! MoveReq("East")),
        "West"->((args:String)=>mRoom ! MoveReq("West")),
        "North"->((args:String)=>mRoom ! MoveReq("North")),
        "Up"->((args:String)=>mRoom ! MoveReq("Up")),
        "Down"->((args:String)=>mRoom ! MoveReq("Down")),
        "South"->((args:String)=>mRoom ! MoveReq("South")),
        "Back"->((args:String)=>mRoom ! MoveReq("Back")),
        "Say"->((args:String)=> mRoom ! args),
        "Tell"->((args:String) => Main.chatServe ! Chat(args.split(" ").map(e=>e.trim).head.trim.capitalize, name+" says: "+args.split(" ").map(e=>e.trim).tail.mkString(" "))),
        "Name"->((args:String) => rename(args)),
        "CheckName"->((args:String)=>print(name)))

  private def readLine(): Option[String] = {
    if (is.available > 0) {
      val buf = new Array[Byte](is.available); //XXX Think about what's going on here.
      is.read(buf)
      Some(new String(buf)) //XXX Why does this not work with [] brackets?
    } else None
   }        
  private def takeCommand(s: String): Unit = {
    if ((s contains " ") || (s contains "\n") || (s contains "\r")) { s.trim() }
    if (!((s contains " ") || (s contains "\n") || (s contains "\r"))) {
      try {
        command(s)(" ")
      } catch {
        case e: NoSuchElementException =>
          println("You tried the command:" + s + """. Type "Help" for assistance.""")
          println("1 -" + s + "- \n")
      }
      return ;
    } else {
      var temp = s.split(" ").map(e => e.capitalize.trim)
      try {
        if (temp.length > 2) {
          temp(1) = temp.tail.mkString(" ")
          command(temp(0))(temp(1))
        } else if (temp.length == 2) {
          command(temp(0))(temp(1))
        } else {
          command(temp(0))(" ")
        }
      } catch { case e: NoSuchElementException => print("You tried the command:" + temp(0) + """. Type "Help" for assistance.""") }
    }}
}


