package res.process

import res.intermediate._

/** Override model configuration stuff from config files */
object PatchAnimationProperties {


  def patchAnimationProperties(animations: Seq[Animation], config: Config.Res.Model): Unit = {

    def findAnimationConfigs(name: String): Seq[Config.Res.Model.Animation] = config.animations.filter(cfg => {
      cfg.nameRegex match {
        case Some(regex) => regex.findFirstIn(name).isDefined
        case None => true
      }
    })

    for (anim <- animations) {
      val configs = findAnimationConfigs(anim.name)
      for (config <- configs) {
        if (config.lengthInFrames > 0.0) {
          anim.duration = config.lengthInFrames / anim.ticksPerSecond
        }
      }
    }

  }

}

