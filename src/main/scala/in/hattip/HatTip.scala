package in.hattip

import java.net.URI
import java.util.concurrent.TimeUnit
import scala.Function.tupled
import scala.collection.mutable.ListBuffer
import scala.xml.XML
import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.http.HttpFields
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketClient
import org.eclipse.jetty.websocket.WebSocketClientFactory
import scala.collection.mutable
import com.codecommit.antixml
import java.net.URLEncoder

trait HttpCode{
  val code: Int
}

case object _200 extends HttpCode{val code = 200}
case object _201 extends HttpCode{val code = 201}
case object _202 extends HttpCode{val code = 202}
case object _203 extends HttpCode{val code = 203}
case object _204 extends HttpCode{val code = 204}
case object _205 extends HttpCode{val code = 205}
case object _206 extends HttpCode{val code = 206}

case object _300 extends HttpCode{val code = 300}
case object _301 extends HttpCode{val code = 301}
case object _302 extends HttpCode{val code = 302}
case object _303 extends HttpCode{val code = 303}
case object _304 extends HttpCode{val code = 304}
case object _305 extends HttpCode{val code = 305}
case object _306 extends HttpCode{val code = 306}
case object _307 extends HttpCode{val code = 307}

case object _400 extends HttpCode{val code = 400}
case object _401 extends HttpCode{val code = 401}
case object _402 extends HttpCode{val code = 402}
case object _403 extends HttpCode{val code = 403}
case object _404 extends HttpCode{val code = 404}
case object _405 extends HttpCode{val code = 405}
case object _406 extends HttpCode{val code = 406}
case object _407 extends HttpCode{val code = 407}
case object _408 extends HttpCode{val code = 408}
case object _409 extends HttpCode{val code = 409}
case object _410 extends HttpCode{val code = 410}
case object _411 extends HttpCode{val code = 411}
case object _412 extends HttpCode{val code = 412}
case object _413 extends HttpCode{val code = 413}
case object _414 extends HttpCode{val code = 414}
case object _415 extends HttpCode{val code = 415}
case object _416 extends HttpCode{val code = 416}
case object _417 extends HttpCode{val code = 417}

case object _500 extends HttpCode{val code = 500}
case object _501 extends HttpCode{val code = 501}
case object _502 extends HttpCode{val code = 502}
case object _503 extends HttpCode{val code = 503}
case object _504 extends HttpCode{val code = 504}
case object _505 extends HttpCode{val code = 505}

case object _999 extends HttpCode{val code = 999}

object HttpCode {
  var map = mutable.Map[Int,HttpCode]()
  def registerAll() = {
    List[HttpCode](
        _200,_201,_202,_203,_204,_205,_206,
        _300,_301,_302,_303,_304,_305,_306, _307,
        _400,_401,_402,_403,_404,_405,_406, _407, _408, _409,
        _410,_411,_412,_413,_414,_415,_416, _417, 
        _500,_501,_502,_503,_504,_505
    ) foreach { register _}
  }
  def register(instance: HttpCode) = {
    map += (instance.code -> instance)
  }
  def apply(c: Int):HttpCode = {
    map.getOrElse(c,_999)
  }
  registerAll()
}


object Success {
  def unapply(hc: HttpCode) : Boolean = {
    hc.code >= 200 && hc.code <= 299
  }
}
object Redirection {
  def unapply(hc: HttpCode) : Boolean = {
    hc.code >= 300 && hc.code <= 399
  }
}
object ClientError {
  def unapply(hc: HttpCode) : Boolean = {
    hc.code >= 400 && hc.code <= 499
  }
}
object ServerError {
  def unapply(hc: HttpCode) : Boolean = {
    hc.code >= 500 && hc.code <= 599
  }
}

object NotFound {
  def unapply(hc: HttpCode) : Boolean = {
    hc.code == 404
  }
}

class HttpResponse(c: Int, fields: HttpFields, val str: String) {
  val code = HttpCode(c)
  override def toString = "Response[" + code + "]" + str
  def asXml = XML.loadString(str)
  def asAntiXml = antixml.XML.fromString(str)
  def process(f: PartialFunction[HttpCode, Unit]) = f(code)
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
  var headers = ListBuffer[(String,String)]()

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
    headers map { 
      hdr => 
        ex.addRequestHeader(hdr._1, hdr._2)
    }
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
    headers map { hdr => ex.addRequestHeader(hdr._1, hdr._2)}
    httpClient.send(ex)
    ex.waitForDone
    new HttpResponse(ex.getResponseStatus, ex.getResponseFields, ex.getResponseContent)
  }

  def /(additional: String)(implicit ev: S =:= UriStage) = HttpEndpoint(str + "/" + additional)

  def ?(elements: (String, String)*) = {
    val res = elements map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
    new HttpEndpoint {
      type S = QueryParamStage
      def str = outer.str + "?" + res
    }
  }
  
  def withHeaders(headers: (String,String)*) = {
    headers map { this.headers += }
    this
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
