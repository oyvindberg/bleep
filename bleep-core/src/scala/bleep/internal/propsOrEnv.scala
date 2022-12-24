package bleep.internal

object propsOrEnv {
  def apply(key: String): Option[String] = sys.env.get(key).orElse(sys.props.get(key))
}
