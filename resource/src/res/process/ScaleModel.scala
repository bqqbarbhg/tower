package res.process

import core._
import res.intermediate._

/**
  * Scale the model globally.
  * Motivation: Every first-level node in Blender exports is 100x too big, so scale them
  * down to avoid having weirdly scaled root and nodes.
  */
object ScaleModel {

  def scaleModel(model: Model, config: Config.Res.Model): Unit = {
    for (child <- model.root.children) {
      child.transform = Matrix43.scale(config.scale) * child.transform
    }
  }

}

