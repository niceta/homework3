implicit class RichOption(opt: Option[Int]) {
  @inline def default: Int = opt match {
    case None => 0
    case Some(x) => x
  }
}

val k = Option.empty[Int].default
val z = Some(23).default