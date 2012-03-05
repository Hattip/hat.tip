package in.hattip

import java.net.URI
import java.net.URLEncoder
import java.util.concurrent.TimeUnit

import scala.Function.tupled
import scala.collection.mutable.ListBuffer
import scala.xml.XML

import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.http.HttpFields
import org.eclipse.jetty.io.{Buffer => JettyBuffer}
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketClient
import org.eclipse.jetty.websocket.WebSocketClientFactory

import com.codecommit.antixml

object Hattip {
  type HttpResponseCode = Int

  case class HttpResponseCodeClass(f: HttpResponseCode => Boolean) {
    def unapply(code: HttpResponseCode): Boolean = f(code)
  }

  lazy val AccessControlFailure = HttpResponseCodeClass(Set(401,402,403,405,406))
  lazy val Success = HttpResponseCodeClass(code => code >= 200 && code <= 299)
  lazy val Failure = HttpResponseCodeClass(code => code < 200 || code > 299)
  lazy val Redirection = HttpResponseCodeClass(code => code >= 300 && code <= 399)
  lazy val Moved = HttpResponseCodeClass(Set(301,302,303,307))
  lazy val ClientError = HttpResponseCodeClass(code => code >= 400 && code <= 499)
  lazy val ServerError = HttpResponseCodeClass(code => code >= 500 && code <= 599)
  lazy val NotFound = HttpResponseCodeClass(404 ==)

  case class HttpResponse(code: HttpResponseCode, headers: Map[String,String], contents: String) {
    def asXml = XML.loadString(contents)
    def asAntiXml = antixml.XML.fromString(contents)
    def process(f: PartialFunction[HttpResponseCode, Unit]) = f(code)
    override def toString = "Response(code = %d, contents = %s)" format (code, contents)
  }

  object WsConnection {
    private val factory = new WebSocketClientFactory
    factory.start

    def apply(protocol: String): WebSocketClient = {
      val client = factory.newWebSocketClient
      client setProtocol protocol
      client
    }
  }

  private class WrappedWebSocket extends WebSocket with WebSocket.OnTextMessage with WebSocket.OnBinaryMessage {
    var connection: Option[Connection] = None
    var closed = false

    var textHandler: Option[String => Unit] = None

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
    def setMessageHandler(f: String => Unit): Unit = {
      wSocket.onMessage(f)
    }
    
    def !(message: String): Unit = {
      connection.sendMessage(message)
    }

    def close(): Unit = {
      connection.close
    }
  }

  class HattipContentExchange extends ContentExchange {
    val headerBuffer = ListBuffer.empty[(String,String)]

    def headers = headerBuffer.toMap

    override def onResponseHeader(name: JettyBuffer, value: JettyBuffer): Unit = {
      headerBuffer.append((name.toString, value.toString))
      super.onResponseHeader(name, value)
    }
  }

  // Phantom types to ensure proper creation of HttpEndpoint.
  // TODO: Has flaws. Rectify.
  sealed trait HttpEndpointConstructionStage
  trait UriStage extends HttpEndpointConstructionStage
  trait QueryParamStage extends HttpEndpointConstructionStage

  trait HttpEndpoint { outer =>
    type S <: HttpEndpointConstructionStage

    // FIX: This does not belong here. Should be listed as a constant elsewhere.
    val movedCodes = Set(301,302,303,307)

    // Initialization code
    private val httpClient = new HttpClient
    httpClient.start
    httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL

    require(
      Set("http://", "https://", "ws://").exists(uri.startsWith),
      "Invalid Http Endpoint String " + uri
    )

    def uri: String

    private val headers = ListBuffer.empty[(String,String)]

    def as(realm: String, principal: String, credentials: String): HttpEndpoint = {
      val resolver = new HashRealmResolver
      resolver addSecurityRealm {
        new Realm {
          override def getId = realm
          override def getPrincipal = principal
          override def getCredentials = credentials
        }
      }
      httpClient.setRealmResolver(resolver)
      this
    }

    def get: HttpResponse = getInternal(uri, 5, Nil)

    // TODO: Recursion should be avoided.
    // FIX: return is considered a code smell in Scala code. Should be avoided.
    private[this] def getInternal(
      uri: String,
      tries: Int,
      traversed: List[String]
    ): HttpResponse = {
      val ex = new HattipContentExchange
      ex.setURL(uri)
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      ex.waitForDone
      val status = ex.getResponseStatus
      val content = ex.getResponseContent
      // is the status - page moved?
      if ((movedCodes contains status) && tries> 0) {
        // page has moved so get the new page
        ex.headers.get("Location") foreach { newUri =>
          // check for page already traversed (loops)
          if(!(traversed contains newUri)) {
            return getInternal(newUri, tries -1, newUri :: traversed)
          }
        }
      }
      return new HttpResponse(status, ex.headers, content)
    }

    def post(data: String): HttpResponse = {
      val ex = new HattipContentExchange
      ex.setMethod("POST")
      ex.setURL(uri)

      // TODO: remove the hardcoding of content-type
      ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
      ex.setRequestContent(new ByteArrayBuffer(data.getBytes))
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      ex.waitForDone
      new HttpResponse(ex.getResponseStatus, ex.headers, ex.getResponseContent)
    }

    def /(additional: String)(implicit ev: S =:= UriStage) = HttpEndpoint(uri + "/" + additional)

    def ?(elements: (String, String)*) = {
      val res = elements map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
      new HttpEndpoint {
        type S = QueryParamStage
        def uri = outer.uri + "?" + res
      }
    }

    def withHeaders(headers: (String,String)*) = {
      this.headers ++= headers
      this
    }

    def open(protocol: String): WrappedConnection = {
      val client = WsConnection(protocol)
      val wSocket = new WrappedWebSocket
      val future = client.open(new URI(uri), wSocket)
      val connection = future.get(10, TimeUnit.SECONDS)
      new WrappedConnection(connection, wSocket)
    }
  }

  object HttpEndpoint {
    def apply(s: String) = new HttpEndpoint {
      type S = UriStage
      def uri = s
    }
  }

  implicit def str2HttpEndpoint(str: String) = HttpEndpoint(str)
}
