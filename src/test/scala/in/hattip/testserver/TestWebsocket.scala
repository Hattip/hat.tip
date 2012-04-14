package in.hattip.testserver

import org.scalatra._
import javax.servlet.http.{ HttpServletResponse, HttpServletRequest }
import org.eclipse.jetty.websocket.{ WebSocket => ServletWebSocket, WebSocketFactory }
import java.io.UnsupportedEncodingException
import collection.mutable.{ HashSet, SynchronizedSet }
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample
import org.specs2.mutable.SpecificationWithJUnit
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import com.weiglewilczek.slf4s.Logging
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.handler.ResourceHandler
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocketHandler
import org.eclipse.jetty.websocket.WebSocket
import in.hattip.client.Hattip._

@RunWith(classOf[JUnitRunner])
class TestWebsocket extends SpecificationWithJUnit with BeforeExample with AfterExample {
  val wsServer = new WebsocketServer()
  def before = wsServer.start()
  def after = wsServer.stop()
  val wsHost = "ws://localhost:9999"
  "A websocket client" should {
    "be able to send a websocket text message" in {
      val conn = open(wsHost, "in.hattip.hattip")

      conn ! "message .. message .. message"
      success
    }
    "be able to read a websocket text message" in {
      val conn = open(wsHost, "in.hattip.hattip");
      var receivedPong = false
      conn setTextHandler { msg =>
        receivedPong = msg == "pong"
      }
      conn ! "ping"
      // sleep to wait to get the response back
      Thread.sleep(2000)
      receivedPong must_== (true)
    }

    "be able to read a websocket binary message" in {
      val conn = open(wsHost, "in.hattip.hattip");
      var receivedPong = false
      val sendBytes: Array[Byte] = Array[Byte](0, 65, 127, 0, 66, 127, 0, 67, 127)
      val recvBytes = sendBytes reverse;
      conn setBinaryHandler { data =>
        receivedPong = java.util.Arrays.equals(data, recvBytes)
      }
      conn ! sendBytes
      // sleep to wait to get the response back
      Thread.sleep(2000)
      receivedPong must_== (true)
    }
  }
}

class Socket extends WebSocket with WebSocket.OnTextMessage with WebSocket.OnBinaryMessage {
  var connection: Option[Connection] = None

  def onOpen(connection: Connection) {
    this.connection = Some(connection)
  }
  def onClose(code: Int, message: String) {

  }
  def onMessage(message: String) {
    if (message == "ping") {
      this.connection foreach { _.sendMessage("pong") }
    }
  }
  def onMessage(data: Array[Byte], offset: Int, length: Int) {
    this.connection foreach {
      _.sendMessage(data.slice(offset, offset + length).reverse, 0, length)
    }
  }
}

class WebsocketServer extends Server {
  val connector = new SelectChannelConnector()
  connector.setPort(9999)
  this.addConnector(connector)

  val handler = new WebSocketHandler() {
    def doWebSocketConnect(request: HttpServletRequest, protocol: String): WebSocket = {
      new Socket();
    }
  }
  setHandler(handler)

  val rHandler = new ResourceHandler()
  rHandler.setDirectoriesListed(false)
  rHandler.setResourceBase(System.getProperty("user.dir"))
  handler.setHandler(rHandler)

}