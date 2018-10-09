sealed abstract class Players(name: String) {
  def showName: String = name
}

object Players {

  case object Player extends Players("あなた")

  case object Enemy extends Players("あいて")

}
