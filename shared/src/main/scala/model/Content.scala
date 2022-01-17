
package shared.model

/*
 * Directory Content, creating navigation Item
 * from it
 */
case class DirEntry(
  title:    String,      // display name
  file:     String,      // file to display
  entry:    List[ContentEntry]
) 
  
case class ContentEntry(
  title:   String,
  file:    String,
  path:    String
)                  


object DirEntry {
  import scala.collection.mutable.ListBuffer
  
  def redo(name: String) = name.replaceAll("\\.","-")
  def repDot(input: DirEntry) : DirEntry = {
    var cEL = new ListBuffer[ContentEntry]()
    input.entry.foreach { e => cEL += ContentEntry(e.title, redo(e.file), e.path) }
    DirEntry(input.title, redo(input.file), cEL.toList)
  }
  
}

//  content-ttcprog-config-json
//    name = TTC Turnierprogramm
//    file = info.html
//   
//  
//  id = content + path + file