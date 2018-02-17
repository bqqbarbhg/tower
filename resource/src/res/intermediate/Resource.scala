package res.intermediate

trait Resource {

  /** Release the memory used by the loaded resource */
  def unload(): Unit

}
