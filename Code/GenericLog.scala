class GenericLog[A]{
  trait Event
  case class Call(op: A) extends Event
  case class Return(


}
