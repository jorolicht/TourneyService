package scalajs.usecase.component

// Define a new enumeration with a type alias and work with the full set of enumerated values
object DlgOption extends Enumeration {
  type DlgOption = Value
  val New, View, Edit = Value
}