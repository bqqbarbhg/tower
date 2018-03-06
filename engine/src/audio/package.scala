package object audio {

  def dbToAmp(db: Float): Float = {
    math.pow(10.0f, db * 0.1f).toFloat
  }

  def ampToDb(amp: Float): Float = {
    10.0f * math.log10(amp).toFloat
  }

}
