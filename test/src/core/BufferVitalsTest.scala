package core

import java.nio.{ByteBuffer, ByteOrder}

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class BufferVitalsTest extends FlatSpec with Matchers {

  "getBuffer" should "return partition the buffer" in {
    val buffer = ByteBuffer.allocateDirect(64)
    buffer.position(16)
    val get = buffer.getBuffer(16)

    assert(get.position === 0)
    assert(get.remaining === 16)
    assert(get.capacity === 48)
  }

  it should "have the same endianness as the original buffer" in {
    {
      val buffer = ByteBuffer.allocateDirect(64).order(ByteOrder.LITTLE_ENDIAN)
      val get = buffer.getBuffer(16)
      assert(get.order === ByteOrder.LITTLE_ENDIAN)
    }
    {
      val buffer = ByteBuffer.allocateDirect(64).order(ByteOrder.BIG_ENDIAN)
      val get = buffer.getBuffer(16)
      assert(get.order === ByteOrder.BIG_ENDIAN)
    }
  }

}
