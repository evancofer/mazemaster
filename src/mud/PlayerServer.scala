
package mud

import akka.actor.Actor
import akka.actor.ActorRef

class PlayerServer extends Actor {
  private var mPlayers:scala.collection.mutable.Map[String, ActorRef] = scala.collection.mutable.Map()
  def receive = {
    case msg:Chat => {
      try{
        mPlayers(msg.name) ! msg.message  
      } catch {
        case e:NoSuchElementException => sender ! "Player not found."
      }}
    case listing:RenameReq => { if(mPlayers.contains(listing.name)) {
      mPlayers-=listing.name 
      mPlayers+=(listing.newName->listing.player) //else {println("Rename failure on"+name)}
      }}
    case listing:Listing => mPlayers+=(listing.name->listing.player)
    case _=>println("Failure.")
  }

  //private val ticker = context.system.scheduler.schedule(100.millis, 100.millis, self, Tick)//Must use this to bring in players.
}

