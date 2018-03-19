package game.state

import scala.collection.mutable

object GameState {

  object InitialState extends GameState {
    override def start(): Unit = { }
    override def update(): Unit = {
      throw new AssertionError("Should never be called!")
    }
    override def stop(): Unit = { }
    override def done: Boolean = false
  }

  private var stateStack = mutable.ArrayStack[GameState](InitialState)
  private var stateHasBeenStarted = mutable.ArrayStack[Boolean](false)
  private var nextState: Option[GameState] = None

  def push(state: GameState): Unit = {
    nextState = Some(state)
  }

  def current: GameState = stateStack.top

  def update(): Unit = {

    var needsUpdate = true
    while (needsUpdate) {
      needsUpdate = false

      // Pop finished states
      while (current.done) {
        current.stop()
        stateStack.pop()
        stateHasBeenStarted.pop()
      }

      // Switch to next state if available
      if (nextState.isDefined) {
        stateStack.push(nextState.get)
        stateHasBeenStarted.push(false)
        nextState = None
        current.load()
        needsUpdate = true
      }
    }

    if (!stateHasBeenStarted.top) {
      current.start()
      stateHasBeenStarted.pop()
      stateHasBeenStarted.push(true)
    }

    current.update()
  }
}

trait GameState {

  /**
    * Called when the state is first switched on.
    */
  def load(): Unit = { }

  /**
    * Called when the state is first switched on and loaded.
    */
  def start(): Unit

  /**
    * Called every frame while the state is active.
    */
  def update(): Unit

  /**
    * Called when the state is exited.
    */
  def stop(): Unit

  /**
    * Should this state be removed.
    */
  def done: Boolean

}
