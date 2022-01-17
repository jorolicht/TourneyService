package shared.utils

import upickle.default._
import shared.utils.UseCaseParam

package object twirlhelper {
  //case class UseCaseParam(idBase: String, msgPrefix:String, objName: String, dataAttr: String, msgfunc: (String, Seq[String])=>String)

  
  def attrName(name: String)(implicit ucp: UseCaseParam) = { s""" name='${ucp.idBase}__${name}' """ }
  def attrId(name: String)(implicit ucp: UseCaseParam)      = { s""" id='${ucp.idBase}__${name}' """ }
  def attrList(name: String)(implicit ucp: UseCaseParam)      = { s""" list='${ucp.idBase}__${name}' """ }
  def attrPlh(name: String)(implicit ucp: UseCaseParam) = { val pname = s"plh.${name}"; s"placeholder='${msg(pname)}'"}
  def attrFor(name: String)(implicit ucp: UseCaseParam) = { s"for='${ucp.idBase}__${name}'"}
  
  def _name(name: String)(implicit ucp: UseCaseParam)    = { s"${ucp.idBase}__${name}"}
  def _data(attrName: String, attrValue: String)(implicit ucp: UseCaseParam) = { s"data-${ucp.dataAttr}-${attrName}='${attrValue}'"}
  
  def msg(name: String, arg: String*)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.${name}", arg)
  def msg_(name: String, arg: String*)(implicit ucp: UseCaseParam): String = ucp.msgfunc(name, arg)

  def msgLabel(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.label.${name}", Seq())
  def msgHelp(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.help.${name}", Seq())
  def msgCheck(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.check.${name}", Seq())
  
  def msgPlh(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.plh.${name}",Seq())
  def msgLbl(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.lbl.${name}", Seq())
  def msgHlp(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.hlp.${name}", Seq())
  def msgTit(name: String)(implicit ucp: UseCaseParam): String = ucp.msgfunc(s"${ucp.msgPrefix}.tit.${name}", Seq())

  def collapse(name: String)(implicit ucp: UseCaseParam): String = {
    s""" href="javascript:InputCtrl.collapse('${ucp.idBase}__${name}')" """
  }

  def _onfocusout(name: String)(implicit ucp: UseCaseParam): String = {
    s"""onfocusout='${ucp.objName}.onfocusout('${name}', this.value);' """
  }

  def _onInputChange(name: String)(implicit ucp: UseCaseParam): String = {
    s"""onchange="${ucp.objName}.onchange('${name}', this.value);" """
  }

  def _onchange(_func: String, args: String*)(implicit ucp: UseCaseParam): String = {
    import scala.collection.mutable.ListBuffer
    var argSeq = new ListBuffer[String]()
    args.foreach( arg => argSeq += "'" + arg + "'" )
    s"""onchange="${ucp.objName}.onchange${_func}(${argSeq.mkString(",")});" """
  }

  def _onclick(_func: String, args: String*)(implicit ucp: UseCaseParam): String = {
    import scala.collection.mutable.ListBuffer
    var argSeq = ListBuffer("this")
    args.foreach( arg => argSeq += "'" + arg + "'" )
    s"""onclick="${ucp.objName}.onclick${_func}(${argSeq.mkString(",")});return false" """
  }


  def _onsubmit(_func: String, args: String*)(implicit ucp: UseCaseParam): String = {
    import scala.collection.mutable.ListBuffer
    var argSeq = ListBuffer("this")
    args.foreach( arg => argSeq += "'" + arg + "'" )
    s"""onsubmit="${ucp.objName}.onsubmit${_func}(${argSeq.mkString(",")});return false" """
  }

  def _onclickNoReturn(_func: String, args: String*)(implicit ucp: UseCaseParam): String = {
    import scala.collection.mutable.ListBuffer
    var argSeq = ListBuffer("this")
    args.foreach( arg => argSeq += "'" + arg + "'" )
    s"""onclick="${ucp.objName}.onclick${_func}(${argSeq.mkString(",")})" """
  }

  def _button(caption: String, id: String, _class: String, _func:String, args:String*)(implicit ucp: UseCaseParam): String = {
    import scala.collection.mutable.ListBuffer
    var argSeq = new ListBuffer[String]()
    args.foreach( arg => argSeq += "'" + arg + "'" )
    val bName  = if (caption.startsWith("std.btn.")) msg_(caption) else msg(caption)
    val bClass = if (_class == "close") _class else s"btn btn-outline-secondary ${_class}"

    if (_func =="") {
      if (id == "") {
        s"""<button type='button' class='${bClass}'>${bName}</button>"""
      } else {
        s"""<button id='${ucp.idBase + "__" + id}' type='button' class='${bClass}'>${bName}</button>"""
      }
    } else {
      if (id == "") {
        s"""<button onclick="${ucp.objName}.button${_func}(${argSeq.mkString(",")})" type='button' class='${bClass}'>${bName}</button>"""
      } else {
        s"""<button id='${ucp.idBase + "__" + id}' onclick="${ucp.objName}.button${_func}(${argSeq.mkString(",")})" type='button' class='${bClass}'>${bName}</button>"""
      }      
    }
  }


  def _getHtml(name: String)(implicit ucp: UseCaseParam): String = {
    msg(name)
  }

  def _tit(name: String)(implicit ucp: UseCaseParam): String = {
    s"""title='${msgTit(name)}'"""
  }

  def _lbl(name: String, classAttr:String)(implicit ucp: UseCaseParam): String = {
    s"""<label class='${classAttr}' for='${ucp.idBase}__${name}'>${msgLbl(name)}</label>"""
  }

  def _label(name: String, classAttr:String)(implicit ucp: UseCaseParam): String = {
    s"""<label class='${classAttr}' for='${ucp.idBase}__${name}'>${msgLabel(name)}</label>"""
  }

  def _helpText(name: String)(implicit ucp: UseCaseParam): String = {
    s"""<small class="form-text text-light bg-danger text-white" style='display:none' id='${ucp.idBase}__${name}Hlp'>${msgHlp(name)}</small>"""
  }


  def _style(name: String, value: String): String = {
    s""" style="${name}:${value}" """
  }  

  def _actionEvent(key: String, inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="${ucp.objName}.actionEvent('${key}', this, event);" """
  }
  
  def _actionHref(key: String)(implicit ucp: UseCaseParam): String = {
    s"""href="javascript:${ucp.objName}.actionEvent('${key}', this, null);"  """
  }


  def _actionButton(key: String, caption: String, _class: String, dataAttr:String, dataValue: String)(implicit ucp: UseCaseParam): String = {
    val bName  = if (caption.startsWith("std.btn.")) msg_(caption) else msg("btn." + caption)
    val bClass = if (_class == "close") _class else s"btn btn-outline-secondary ${_class}"
    val data   = if (dataAttr != "") s"data-${dataAttr}='${dataValue}'" else ""
    s"""<button id='${ucp.idBase + "__Btn" + key}' ${data} type='button' class='${bClass}'  ${_actionEvent(key, "onclick")}>${bName}</button> """   
  }

  /*
  ** input Routines
  */
  def _inputTextCtx(name: String, _class: String, ctx: ujson.Value)(implicit ucp: UseCaseParam): String = {
    s"""<input type='text' class='form-control ${_class}' ${attrId(name)} ${attrPlh(name)} value='${ctx(name).value}' ${_actionEvent(name,"onchange")} ${_actionEvent(name, "oninput")} >"""
  }  

  def _inputText(name: String, _class: String, _default: String)(implicit ucp: UseCaseParam): String = {
    s"""<input type='text' class='form-control ${_class}' ${attrId(name)} value='${_default}' ${_actionEvent(name,"onchange")} ${_actionEvent(name, "oninput")} >"""
  }  

  def _inputClub(name: String, init: String, plho: String, _class: String="", readonly:Boolean=false, list:Boolean=false)(implicit ucp: UseCaseParam): String = {
    val roFlag = if (readonly) "readonly" else ""
    val lParam = if (list) s"list='${ucp.idBase}__${name}List'" else ""

    plho match {
      case "_" =>  s"""<input type='text' class='form-control ${_class}' ${attrId(name)} placeholder='${msgPlh(name)}' value='${init}' ${_eventClub("onchange")} ${_eventClub("oninput")} ${roFlag} ${lParam}>"""
      case ""  =>  s"""<input type='text' class='form-control ${_class}' ${attrId(name)} value='${init}' ${_eventClub("onchange")} ${_eventClub("oninput")} ${roFlag} ${lParam}>"""
      case _   =>  s"""<input type='text' class='form-control ${_class}' ${attrId(name)} placeholder='${msg_(plho)}' value='${init}' ${_eventClub("onchange")} ${_eventClub("oninput")} ${roFlag} ${lParam}>"""
    }
  }

  def _eventClub(inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="InputCtrl.eventClub('${inEvent}', this);" """
  }


  def _inputTextPlh(name: String, init: String, placeholder: String, _class: String="", minlength:Int=0)(implicit ucp: UseCaseParam): String = {
    val mlp = if (minlength>0) s"minlength='${minlength}'" else ""
    val clp = if (_class!="") s"class='form-control ${_class}'" else "class='form-control'"
    placeholder match {
      case "_" =>  s"""<input type='text' ${clp} ${mlp} ${attrId(name)} placeholder='${msgPlh(name)}' value='${init}' ${_eventText("onchange")} ${_eventText("oninput")} >"""
      case ""  =>  s"""<input type='text' ${clp} ${mlp} ${attrId(name)} value='${init}' ${_eventText("onchange")} ${_eventText("oninput")} >"""
      case _   =>  s"""<input type='text' ${clp} ${mlp} ${attrId(name)} placeholder='${msg_(placeholder)}' value='${init}' ${_eventText("onchange")} ${_eventText("oninput")} >"""
    }
  }

  def _eventText(inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="InputCtrl.eventText('${inEvent}', this);" """
  }


  def _inputRadio(name: String, grpName: String, classAttr:String)(implicit ucp: UseCaseParam): String = {
    s"""<input type='radio' class='${classAttr}' ${attrId(name)}  name='${grpName}' ${_actionEvent(name, "onchange")} >"""
  }  


  /*
  ** Password routines
  */

  def _eventPassword(inEvent: String, checkEqualWithId: String="")(implicit ucp: UseCaseParam): String = {
    val checkId = if (checkEqualWithId=="") "" else ucp.idBase + "__" + checkEqualWithId
    s""" ${inEvent}="InputCtrl.eventPassword('${inEvent}', this, '${checkId}');" """
  }

  def _eventShowPassword(id: String="")(implicit ucp: UseCaseParam): String = {
    s""" onclick="InputCtrl.eventPassword('show', this, '${ucp.idBase}__${id}');" """
  }



  /*
  ** Country routines
  */  
  def _eventCountry(inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="InputCtrl.eventCountry('${inEvent}', this);" """
  }


  /*
  ** Name routines
  */
  def _inputName(id: String, _class: String, initValue: String, readonly: Boolean, list: Boolean)(implicit ucp: UseCaseParam): String = {
    val roFlag = if (readonly) "readonly" else ""
    val lParam = if (list) s"list='${ucp.idBase}__${id}List'" else ""
    s"""<input type='text' class='form-control ${_class}' ${attrId(id)} placeholder='${msg_("input.name.plh")}' value='${initValue}' ${_eventName("onchange")} ${_eventName("oninput")} ${roFlag} ${lParam}>"""
  }
  
  def _eventName(inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="InputCtrl.eventName('${inEvent}', this);" """
  }


  /*
  ** Email routines
  */
  def _inputEmail(id: String, _class: String, initValue: String, readonly: Boolean)(implicit ucp: UseCaseParam): String = {
    if (readonly) {
     s"""<input type='email' class='form-control ${_class}' ${attrId(id)} placeholder='${msg_("input.email.plh")}' value='${initValue}' ${_eventEmail("onchange")} ${_eventEmail("oninput")} readonly>"""
    } else {
      s"""<input type='email' class='form-control ${_class}' ${attrId(id)} placeholder='${msg_("input.email.plh")}' value='${initValue}' ${_eventEmail("onchange")} ${_eventEmail("oninput")} >"""
    }
  }
  
  def _eventEmail(inEvent: String)(implicit ucp: UseCaseParam): String = {
    s""" ${inEvent}="InputCtrl.eventEmail('${inEvent}', this);" """
  }


  /*
  ** Label and help elements
  */

  def _lblFor(id: String, classAttr:String, msgId: String)(implicit ucp: UseCaseParam): String = {
    s"""<label class='${classAttr}' for='${ucp.idBase}__${id}'>${msg_(msgId)}</label>"""
  }

  def _hlpFor(id: String, msgId: String)(implicit ucp: UseCaseParam): String = {
    val helpId = id + "Hlp"
    s"""<small ${attrId(helpId)} class="form-text text-light bg-danger text-white" style='display:none'>${msg_(msgId)}</small>"""
  }


}