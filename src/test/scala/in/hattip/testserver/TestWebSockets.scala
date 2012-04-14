package in.hattip.testserver

import org.atmosphere.cpr.Meteor
import org.junit.runner.RunWith
import org.scalatra.ScalatraServlet
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample
import org.atmosphere.cpr.BroadcastFilter
import org.atmosphere.cpr.AtmosphereResourceEventListener
import org.atmosphere.cpr.AtmosphereResourceEvent
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

@RunWith(classOf[JUnitRunner])
class TestWebSockets extends SpecificationWithJUnit with BeforeExample with AfterExample {
  def before = {}
  def after = {}
}

class ChannelListener extends AtmosphereResourceEventListener {
  def onThrowable(event: AtmosphereResourceEvent[HttpServletRequest, HttpServletResponse]) {
    println("onThrowable(): %s" format event)
  }

  def onBroadcast(event: AtmosphereResourceEvent[HttpServletRequest, HttpServletResponse]) {
    println("onBroadcast(): %s" format event)
  }

  def onDisconnect(event: AtmosphereResourceEvent[HttpServletRequest, HttpServletResponse]) {
    println("onDisconnect(): %s" format event)
  }

  def onResume(event: AtmosphereResourceEvent[HttpServletRequest, HttpServletResponse]) {
    println("onResume(): %s" format event)
  }

  def onSuspend(event: AtmosphereResourceEvent[HttpServletRequest, HttpServletResponse]) {
    println("onSuspend(): %s" format event)
  }
}

class WebsocketServlet extends ScalatraServlet {
  val filters = new java.util.LinkedList[BroadcastFilter]()

  get("/?*") {
    val m: Meteor = Meteor.build(request, filters, null)
    m.addListener(new ChannelListener())
    session += "meteor" -> m
    contentType = "text/html;charset=ISO-8859-1"
    m suspend -1
    m broadcast (request.getServerName + "__has suspended a connection from " + request.getRemoteAddr)
  }

  post("/?*") {
    val m = session("meteor").asInstanceOf[Meteor]
    response.setCharacterEncoding("UTF-8")
    val action = params('action)
    val name = params('name)
    action match {
      case "login" => {
        session += "name" -> name
        m broadcast ("System message from " + request.getServerName + "__" + name + " has joined.")
      }
      case "post" => {
        val msg = params('message)
        m.broadcast(name + "__" + msg)
      }
      case _ => {
        status = 422
      }
    }
    "success"
  }
}
