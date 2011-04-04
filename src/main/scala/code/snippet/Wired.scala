package code
package snippet

import net.liftweb._
import common._
import util._
import http._
import Helpers._
import js._
import JsCmds._
import JE._

class MyInfo {
  val i1: ValueCell[Int] = new ValueCell(0)
  val i2: ValueCell[Int] = new ValueCell(0)

  val lst: ValueCell[List[Int]] = new ValueCell(Nil)

  val sum: Cell[Int] = lst.lift(_.foldLeft(0)(_ + _))

  val mult: Cell[Int] = i1.lift(i2)(_ * _)
  val add: Cell[Int] = i1.lift(i2)(_ + _)
  val glorp: Cell[Int] = add.lift(mult)(_ * _)

  val currency: ValueCell[(Box[Int], Box[Int])] = ValueCell(Empty -> Full(1))
  
  val euro: Cell[Box[Int]] = currency.lift{
    case (Full(dollar), _) => Full(dollar / 2)
    case _ => Empty
  }

  val dollar: Cell[Box[Int]] = currency.lift{
    case (_, Full(euro)) => Full(euro * 2)
    case _ => Empty
  }
}

object MyInfoHolder extends SessionVar(new MyInfo)

object Wired {
  def i = MyInfoHolder.get

  def prepend(v: Int) {i.lst.atomicUpdate(old => v :: old)}

  def render = {
    WiringUI.addJsFunc(i.dollar,
                       (vb: Box[Int], ignore) =>
                         vb.map(v =>
                           JsRaw("document.getElementById('dollar').value = "+
                                 v.toString.encJs): JsCmd).openOr(Noop: JsCmd))

    WiringUI.addJsFunc(i.euro,
                       (vb: Box[Int], ignore) =>
                         vb.map(v =>
                           JsRaw("document.getElementById('euro').value = "+
                                 v.toString.encJs): JsCmd).openOr(Noop: JsCmd))


    "@mult" #> WiringUI.asText(i.mult) &
    "@add" #> WiringUI.asText(i.add) &
    "@sum" #> WiringUI.asText(i.sum) &
    "@glorp" #> WiringUI.asText(i.glorp) &
    "@i1" #> SHtml.ajaxText(i.i1.get.toString,
                            s => asInt(s).foreach(v => {
                              i.i1.set(v)
                              prepend(v)
                            })) &
    "@i2" #> SHtml.ajaxText(i.i2.get.toString, s => asInt(s).foreach(i.i2.set)) &
    "#euro" #> SHtml.ajaxText(i.euro.get.map(_.toString) openOr "",
                              s => asInt(s).foreach(v => 
                                i.currency.set(Empty -> Full(v)))) &
    "#dollar" #> SHtml.ajaxText(i.dollar.get.map(_.toString) openOr "",
                                s => asInt(s).foreach(v => 
                                  i.currency.set(Full(v) -> Empty)))
  }
  
  
}
