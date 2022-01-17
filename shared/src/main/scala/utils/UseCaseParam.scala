package shared.utils

case class UseCaseParam(
  idBase: String, 
  msgPrefix:String, 
  objName: String, 
  dataAttr: String, 
  msgfunc: (String, Seq[String])=>String
)

