package game.system.gameplay

import core._
import game.system._
import game.system.gameplay._
import game.system.gameplay.CableSystem.Cable
import game.system.gameplay.TowerSystem._

import ConnectionSystem._
import ConnectionSystemImpl._

object ConnectionSystem {

  abstract class Connection {
    def sendQueued(message: Message): Unit
    def sendIfEmpty(message: Message): Unit
    def receive(): Message
  }

}

sealed trait ConnectionSystem {

  /** Add a logical connection between two slots */
  def addConnection(src: Slot, dst: Slot, cable: Cable): Connection

  /** Remove a logical connection between two slots */
  def removeConnection(connection: Connection): Unit

  /** Update transmitted messages */
  def update(dt: Double): Unit

}

object ConnectionSystemImpl {

  final class ConnectionImpl(val cable: Cable, val length: Double) extends Connection with CompactArrayPool.Element {

    var pendingMessage: Message = MessageNone
    var transmittingMessage: Message = MessageNone
    var receivedMessage: Message = MessageNone
    var transmitTime: Double = length

    override def sendQueued(message: Message): Unit = {
      pendingMessage = message
      if (compactPoolIndex < 0) {
        val system = connectionSystem.asInstanceOf[ConnectionSystemImpl]
        system.activeConnetions.add(this)
      }
    }

    override def sendIfEmpty(message: Message): Unit = {
      if (pendingMessage == MessageNone && transmittingMessage == MessageNone) {
        sendQueued(message)
      }
    }

    override def receive(): Message = receivedMessage

    /** Returns true when finished */
    def updateTransmit(dt: Double): Boolean = {

      transmitTime += dt * 30.0

      if (transmitTime >= length) {
        receivedMessage = transmittingMessage

        transmittingMessage = pendingMessage
        pendingMessage = MessageNone
        if (transmittingMessage != MessageNone) {
          transmitTime = 0.0
          cable.setPulse(0.0)
          false
        } else {
          cable.clearPulse()
          true
        }
      } else {
        cable.setPulse(transmitTime / length)
        false
      }
    }

  }

}

final class ConnectionSystemImpl extends ConnectionSystem {

  val activeConnetions = new CompactArrayPool[ConnectionImpl]

  override def addConnection(src: Slot, dst: Slot, cable: Cable): Connection = {
    val dist = src.entity.position.distanceTo(dst.entity.position)
    val connection = new ConnectionImpl(cable, dist)
    connection
  }

  override def update(dt: Double): Unit = {
    var ix = 0
    while (ix < activeConnetions.length) {
      val conn = activeConnetions.arr(ix)

      if (conn.updateTransmit(dt)) {
        activeConnetions.remove(ix)
      } else {
        ix += 1
      }
    }
  }

  def removeConnection(connection: Connection): Unit = {
    activeConnetions.tryRemove(connection.asInstanceOf[ConnectionImpl])
  }
}

