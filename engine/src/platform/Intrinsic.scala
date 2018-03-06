package platform

import org.lwjgl.util.simd.SSE._
import org.lwjgl.util.simd.SSE3._

object Intrinsic {

  def disableDenormals(): Unit = {
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON)
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON)
  }

}

