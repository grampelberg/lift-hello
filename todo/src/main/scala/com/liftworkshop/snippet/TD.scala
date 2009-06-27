package com.liftworkshop.snippet

import com.liftworkshop._
import model._
import net.liftweb._
import http._
import SHtml._
import S._
import js._
import JsCmds._
import mapper._
import util._
import Helpers._
import scala.xml.{NodeSeq, Text}


class TD {
  def add(form: NodeSeq) = {
    val todo = Todo.create.owner(User.currentUser)

    def checkAndSave(): Unit =
      todo.validate match {
        case Nil => todo.save ; S.notice("Added "+todo.desc)
        case xs => S.error(xs) ; S.mapSnippet("TD.add", doBind)
      }

    def doBind(form: NodeSeq) =
      bind("todo", form,
           "priority" -> todo.priority.toForm,
           "desc" -> todo.desc.toForm,
           "submit" -> submit("New", checkAndSave))
    doBind(form)
  }

  private def toShow =
    Todo.findAll(By(Todo.owner, User.currentUser),
                 if (QueryNotDone) By(Todo.done, false)
                 else Ignore[Todo],
                 OrderBy(Todo.done, Ascending),
                 OrderBy(Todo.priority, Descending),
                 OrderBy(Todo.desc, Ascending))

  private def desc(td: Todo, reDraw: () => JsCmd) =
    swappable(<span>{td.desc}</span>,
              <span>{ajaxText(td.desc,
                              v => {td.desc(v).save; reDraw()})}
              </span>)

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq =
    toShow.
  flatMap(td =>
    bind("todo", html,
         "check" -> ajaxCheckbox(td.done,
                                 v => {td.done(v).save; reDraw()}),
         "priority" ->
         ajaxSelect(Todo.priorityList, Full(td.priority.toString),
                    v => {td.priority(v.toInt).save; reDraw()}),
         "desc" -> desc(td, reDraw)
       ))

  def list(html: NodeSeq) = {
    val id = S.attr("all_id").open_!
    def inner(): NodeSeq = {
      def reDraw() = SetHtml(id, inner())
      bind("todo", html,
           "exclude" ->
           ajaxCheckbox(QueryNotDone, v => {QueryNotDone(v); reDraw}),
           "list" -> doList(reDraw) _)
    }
    inner()
  }

}

object QueryNotDone extends SessionVar(false)
