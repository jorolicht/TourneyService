package shared.utils

case class UseCaseParam(
  idBase: String, 
  msgPrefix:String, 
  objName: String, 
  dataAttr: String, 
  msgfunc: (String, Seq[String])=>String
)

object UseCaseParam {
  def apply():UseCaseParam = UseCaseParam("","","","", (x:String,y:Seq[String])=>"")
}  

case class UCP(ucp: UseCaseParam)


object UCP {
  def apply():UseCaseParam = UseCaseParam("","","","", (x:String,y:Seq[String])=>"")
  def apply(base: String):UseCaseParam = UseCaseParam(base,"","","", (x:String,y:Seq[String])=>"")
  def apply(base: String, msgBase: String ):UseCaseParam = UseCaseParam(base,msgBase,"","", (x:String,y:Seq[String])=>"")
}  

 
