package in.hattip

import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.http.HttpFields
import org.eclipse.jetty.io.ByteArrayBuffer
import Function.tupled
import xml.XML
import com.codecommit.antixml
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.websocket.WebSocketClientFactory
import org.eclipse.jetty.websocket.WebSocketClient
import java.net.URI
import org.eclipse.jetty.websocket.WebSocket
import java.util.concurrent.TimeUnit
import org.eclipse.jetty.websocket.WebSocketConnection
import org.eclipse.jetty.websocket.WebSocket.Connection

class HttpResponse(val code: Int, fields: HttpFields, val str: String) {
  override def toString = "Response[" + code + "]" + str

  // FIX: Something seems wrong here.
  def apply(f: HttpResponse => Unit) = {
    if (code == 200) {
      f(this)
    }
    this
  }

  def asXml = XML.loadString(str)
  def asAntiXml = antixml.XML.fromString(str)
  def orElse(f: HttpResponse => Unit) = f(this)
  def onErrorCode(f: PartialFunction[Int, Unit]) = f(code)
}

object WsConnection {
  val factory = new WebSocketClientFactory()
  factory start
  def apply(protocol: String) : WebSocketClient = {
    val client = factory.newWebSocketClient()
    client setProtocol protocol
    client
  } 
}

class WrappedWebSocket extends WebSocket with WebSocket.OnTextMessage with WebSocket.OnBinaryMessage {
  var connection: Option[Connection] = None
  var closed = false
  
  var textHandler: Option[String=>Unit] = None

  def onMessage(f: String => Unit) {
    textHandler = Some(f)
  }

  def onOpen(connection: Connection) {
    this.connection = Some(connection)
  }
  def onClose(code: Int, message: String) {
    connection = None
  }
  def onMessage(message: String) {
    textHandler foreach (_(message))
  } 
  def onMessage(data: Array[Byte], offset: Int, length: Int) {
    println("Client receives data " + length + " bytes long")
  }
}

class WrappedConnection(connection: Connection, wSocket: WrappedWebSocket) {
  def onMessage(f: String => Unit) {
    wSocket.onMessage(f)
  }
  def !(message: String) = {
    connection.sendMessage(message)
  }
}
// Phantom types to ensure proper creation of HttpEndpoint.
sealed trait HttpEndpointConstructionStage
trait UriStage extends HttpEndpointConstructionStage
trait QueryParamStage extends HttpEndpointConstructionStage

trait HttpEndpoint { outer =>

  type S <: HttpEndpointConstructionStage

  // Initialization code
  val httpClient = new HttpClient
  httpClient.start
  httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL

  require(
    Set("http://", "https://", "ws://").exists(str.startsWith),
    "Invalid Http Endpoint String " + str
  )

  def str: String

  def secure(realm: String, principal: String, credentials: String) = {
    val resolver = new HashRealmResolver();
    resolver.addSecurityRealm(
        new Realm() {
          override def getId() = realm
          override def getPrincipal() = principal
          override def getCredentials() = credentials
      })
    httpClient.setRealmResolver(resolver)
    this
  }
  def get: HttpResponse = {
    val ex = new ContentExchange
    ex.setURL(str)
    httpClient.send(ex)
    ex.waitForDone
    new HttpResponse(ex.getResponseStatus, ex.getResponseFields, ex.getResponseContent)
  }

  def post(data: String): HttpResponse = {
    val ex = new ContentExchange
    ex.setMethod("POST")
    ex.setURL(str)

    ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8");
    ex.setRequestContent(new ByteArrayBuffer(data.getBytes))
    httpClient.send(ex)
    ex.waitForDone
    new HttpResponse(ex.getResponseStatus, ex.getResponseFields, ex.getResponseContent)
  }

  def /(additional: String)(implicit ev: S =:= UriStage) = HttpEndpoint(str + "/" + additional)

  def ?(elements: (String, String)*) = {
    val res = elements map tupled(_ + "=" + _) mkString "&"
    new HttpEndpoint {
      type S = QueryParamStage
      def str = outer.str + "?" + res
    }
  }
  
  def open(protocol: String) : WrappedConnection = {
    val client = WsConnection(protocol)
    val wSocket = new WrappedWebSocket()
    val future = client.open(new URI(str),wSocket);
    val connection = future.get(10,TimeUnit.SECONDS);  
    new WrappedConnection(connection,wSocket)
  }
}

object HttpEndpoint {
  def apply(s: String) = new HttpEndpoint {
    type S = UriStage
    def str = s
  }
}

object Hattip {
  implicit def str2HttpEndpoint(str: String) = HttpEndpoint(str)
}
