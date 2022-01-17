
package shared.model

/*
 * Directory Content, creating navigation Item
 * from it
 */
case class SidebarConfig(
  sidebars: List[Sidebar]
) 

case class Sidebar(
  lang:    String,
  layout:  String,
  entries: List[SidebarEntry]
) 

case class SidebarEntry(
  name:    String,
  ident:   String,
  target:  String,
  tooltip: String,
  icon:    String
)  

  
// {
//    "entries" : [
//      { 
//        "lang":  "de",
//        "layout: "standard", 
//        "entries:  [
//          { name:"Kapitel1" target:"file:index.html" tooltip:"Anzeige von Screenshots" },
//          { name:"Kapitel2" target:"virtual:Home"    tooltip:"Anzeige der Homepage" },
//          { name:"Kapitel3" target:"dir:chapter3"    tooltip:"Kapitel3" }      
//        ]
//      }  
//    ] 
//  }
// 
// 
// object DirEntry {
//   import scala.collection.mutable.ListBuffer
//   
//   def redo(name: String) = name.replaceAll("\\.","-")
//   def repDot(input: DirEntry) : DirEntry = {
//     var cEL = new ListBuffer[ContentEntry]()
//     input.entry.foreach { e => cEL += ContentEntry(e.title, redo(e.file), e.path) }
//     DirEntry(input.title, redo(input.file), cEL.toList)
//   }
