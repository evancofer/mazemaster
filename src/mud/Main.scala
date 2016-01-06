
package mud


import java.net.ServerSocket
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

object Main {
    val actsystem = ActorSystem("ActorSystem")
    private var playerSources: List[Player] = Nil
    val ss = new ServerSocket(4885)
    val chatServe = actsystem.actorOf(Props(classOf[PlayerServer]),"ChatServe")
    
    def main(args:Array[String]): Unit = {
        Future{
          while(true){
            val sock = ss.accept()
            var tempName = "Player"+scala.util.Random.nextInt
            Main.chatServe ! Listing(tempName,actsystem.actorOf(Props(classOf[Player],tempName,Room.mapRooms(12),sock)))
            println("New connect: "+tempName+". ")
          }}
}}