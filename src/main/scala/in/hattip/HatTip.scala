package in.hattip

import java.io.FileOutputStream
import java.net.URI
import java.net.URLEncoder
import java.util.concurrent.TimeUnit
import scala.Function.tupled
import scala.actors.Futures.future
import scala.collection.mutable.ListBuffer
import scala.xml.XML
import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.io.{Buffer => JettyBuffer}
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketClient
import org.eclipse.jetty.websocket.WebSocketClientFactory
import com.codecommit.antixml
import scala.actors.Future

object Hattip {
  type HttpResponseCode = Int

  case class HttpResponseCodeClass(f: HttpResponseCode => Boolean) {
    def unapply(code: HttpResponseCode): Boolean = f(code)
  }

  lazy val AccessControlFailure = HttpResponseCodeClass(Set(401, 402, 403, 405, 406))
  lazy val Success = HttpResponseCodeClass(code => code >= 200 && code <= 299)
  lazy val Failure = HttpResponseCodeClass(code => code < 200 || code > 299)
  lazy val Redirection = HttpResponseCodeClass(code => code >= 300 && code <= 399)
  lazy val Moved = HttpResponseCodeClass(Set(301, 302, 303, 307))
  lazy val ClientError = HttpResponseCodeClass(code => code >= 400 && code <= 499)
  lazy val ServerError = HttpResponseCodeClass(code => code >= 500 && code <= 599)
  lazy val NotFound = HttpResponseCodeClass(404 ==)

  case class HttpResponse(code: HttpResponseCode, 
      headers: Map[String,String], 
      bytes: Array[Byte]) {
    def string(encoding: String) = new String(bytes, encoding)
    def string = new String(bytes, "utf-8")
    def asXml = XML.loadString(string)
    def asAntiXml = antixml.XML.fromString(string)
    def process(f: PartialFunction[HttpResponseCode, Unit]) = f(code)
    def toFile(path: String) = {
      val fos = new FileOutputStream(path)
      fos.write(bytes)
      fos.close()
    }
    def >>> = toFile _
    
    override def toString = "Response(code = %d, contents = %s)" format (code, bytes)
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

    def onMessage(f: String => Unit): Unit = {
      textHandler = Some(f)
    }

    def onOpen(connection: Connection): Unit = {
      this.connection = Some(connection)
    }

    def onClose(code: Int, message: String): Unit = {
      connection = None
    }

    def onMessage(message: String): Unit = {
      textHandler foreach (_(message))
    }

    def onMessage(data: Array[Byte], offset: Int, length: Int): Unit = {
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
    private val headerBuffer = ListBuffer.empty[(String,String)]

    def headers = headerBuffer.toMap

    override def onResponseHeader(name: JettyBuffer, value: JettyBuffer): Unit = {
      headerBuffer.append((name.toString, value.toString))
      super.onResponseHeader(name, value)
    }
  }

  // Phantom types to ensure proper creation of HttpEndpoint.
  trait HttpEndpointConstructionStage {
    trait Open extends HttpEndpointConstructionStage
    trait Closed extends HttpEndpointConstructionStage
  }

  trait HttpEndpoint { outer =>
    type E <: HttpEndpointConstructionStage

    // FIX: This does not belong here. Should be listed as a constant elsewhere.
    val movedCodes = Set(301, 302, 303, 307)

    // Initialization code
    private val httpClient = new HttpClient
    httpClient.start
    httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL

    require(
      Set("http://", "https://", "ws://") exists uri.startsWith,
      "Invalid Http Endpoint String " + uri
    )

    def uri: String

    private val headers = ListBuffer.empty[(String,String)]

    def as(realm: String, principal: String, credentials: String): HttpEndpoint = {
      val resolver = new HashRealmResolver
      resolver addSecurityRealm {
        new Realm {
          def getId = realm
          def getPrincipal = principal
          def getCredentials = credentials
        }
      }
      httpClient.setRealmResolver(resolver)
      this
    }

    def gett : Future[HttpResponse] = {
      val ex = new HattipContentExchange
      ex.setURL(uri)
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      future {
        ex.waitForDone();
        new HttpResponse(
            ex.getResponseStatus, 
            ex.headers,
            ex.getResponseContentBytes)
      }
    }
    
    def get: HttpResponse = getInternal(uri, 5, Nil)

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
      val content = ex.getResponseContentBytes
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
      new HttpResponse(ex.getResponseStatus, ex.headers, ex.getResponseContentBytes)
    }

    def /(additional: String)(implicit ev: E =:= HttpEndpointConstructionStage#Open) = {
      HttpEndpoint(uri + "/" + additional)
    }

    def ?(elements: (String, String)*)(implicit ev: E =:= HttpEndpointConstructionStage#Open) = {
      val res = elements map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
      new HttpEndpoint {
        type E = HttpEndpointConstructionStage#Closed
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
      type E = HttpEndpointConstructionStage#Open
      def uri = s
    }
  }

  implicit def str2HttpEndpoint(str: String) = HttpEndpoint(str)
}
