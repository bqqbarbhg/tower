package game.system.gameplay

import core._
import game.system._
import game.system.gameplay._
import game.system.gameplay.CableSystem.Cable
import game.system.gameplay.TowerSystem.Slot

sealed trait ConnectionSystem {

  /** Add a logical connection between two slots */
  def addConnection(src: Slot, dst: Slot, cable: Cable): Unit

}


final class ConnectionSystemImpl extends ConnectionSystem {

  override def addConnection(src: Slot, dst: Slot, cable: Cable): Unit = {
  }

}

