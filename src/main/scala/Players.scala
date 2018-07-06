sealed trait Players

object Players {

  case object Player extends Players

  case object Enemy extends Players

  def showPlayerName(player: Players): String = player match {
    case Player => "あなた"
    case Enemy  => "あいて"
  }

}
