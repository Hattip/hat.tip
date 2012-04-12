package in.hattip

import java.io.FileOutputStream
import java.net.URI
import java.net.URLEncoder
import java.util.concurrent.TimeUnit
import scala.Function.tupled
import scala.actors.Futures.future
import scala.actors.Future
import scala.collection.mutable.ListBuffer
import scala.xml.XML
import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.security.SimpleRealmResolver
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.io.{Buffer => JettyBuffer}
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.util.StringUtil
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketClient
import org.eclipse.jetty.websocket.WebSocketClientFactory
import com.codecommit.antixml
import com.weiglewilczek.slf4s.Logger
import java.io.File

object Hattip {
  val log = Logger(getClass)

  object FormData {
    def apply(key: String, value: String) = new FormData(key,value)
    def apply(key: String, value: File) = {
      val fileName = value.getName()
    }
  }
  class FormData(key: String, value: String, bytes: Array[Byte] = Array[Byte](), mimetype: String = "application/octet-stream") {
    
  }
  object DataBuffer {
    // code courtesy: MultiPartOutputStream in jetty
    val CRLF = "\015\012".getBytes(StringUtil.__ISO_8859_1);
    val DASHDASH = "--".getBytes(StringUtil.__ISO_8859_1);
    val CONTENT_TYPE_LENGTH = ("Content-Type: ").getBytes(StringUtil.__ISO_8859_1).length
  }

  class DataBuffer {
    // TODO: How could one use java nio buffers ?
    private [this] val boundaryString = "hattip" + System.identityHashCode(this) + java.lang.Long.toString(System.currentTimeMillis(),36)
    private var data = ListBuffer[(String, Array[Byte], Array[String])]()
    def boundary = boundaryString.getBytes(StringUtil.__ISO_8859_1);
    def add(contentType: String, bytes: Array[Byte], headers: Array[String]) = {
      data += Tuple3(contentType, bytes, headers)
    }
    def toBytes {
      val totalLength = data.foldLeft(0) { (len, t3) =>
        val hdrLen = t3._3.foldLeft(0) { (hlen, str) =>
          hlen + str.getBytes(StringUtil.__ISO_8859_1).length + DataBuffer.CRLF.length
        }
        len + (DataBuffer.CONTENT_TYPE_LENGTH * 4) + DataBuffer.DASHDASH.length + hdrLen
      } - DataBuffer.CRLF.length  // The 4 is subtracted because there is no __CRLF at the very beginning
      var outBuffer = new Array[Byte](totalLength)
      var start = 0 
      def write(source: Array[Byte]): Unit = {
        System.arraycopy(source,0,outBuffer,start, source.length)
      }
      data foreach {t3 =>
        if (start != 0) {
          write(DataBuffer.CRLF)
        }
        write(DataBuffer.DASHDASH)
        write(boundary)
        write(DataBuffer.CRLF)
        write(("Content-Type: " + t3._1).getBytes(StringUtil.__ISO_8859_1))
        write(DataBuffer.CRLF)
        t3._3 foreach { header =>
          write(header.getBytes(StringUtil.__ISO_8859_1))
          write(DataBuffer.CRLF)
        }
        write(DataBuffer.CRLF)
        write(t3._2)
      }
      outBuffer
    }
  }

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
      log.debug("Writing content to file:" + path)
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
    var binaryHandler: Option[Array[Byte] => Unit] = None

    def setTextHandler(f: String => Unit): Unit = {
      textHandler = Some(f)
    }

    def setBinaryHandler(f: Array[Byte] => Unit) = {
      binaryHandler = Some(f)
    }

    def onOpen(connection: Connection): Unit = {
      this.connection = Some(connection)
    }

    def onClose(code: Int, message: String): Unit = {
      connection = None
    }

    def onMessage(message: String): Unit = {
      log.debug("Received text message of length " + message.length)
      textHandler foreach (_(message))
    }

    def onMessage(data: Array[Byte], offset: Int, length: Int): Unit = {
      log.debug("Received binary message of length " + length)
      binaryHandler foreach(_(data.slice(offset,offset+length)))
    }
    
  }

  class WrappedConnection(connection: Connection, wSocket: WrappedWebSocket) {
    def setTextHandler(f: String => Unit): Unit = {
      wSocket.setTextHandler(f)
    }
    
    def setBinaryHandler(f: Array[Byte] => Unit): Unit = {
      wSocket.setBinaryHandler(f)
    }
    
    def !(message: String): Unit = {
      log.debug("Sending binary message of length " + message.length)
      connection.sendMessage(message)
    }

    def !(message: Array[Byte]): Unit = {
      log.debug("Sending binary message of length " + message.length)
      connection.sendMessage(message,0,message.length)
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
    
    def as(principal: String, credentials: String): HttpEndpoint = {
      val resolver = new SimpleRealmResolver(
        new Realm {
          def getId = null
          def getPrincipal = principal
          def getCredentials = credentials
        }
      )
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
      log.debug("Issuing GET at URI " + uri)
      httpClient.send(ex)
      ex.waitForDone
      val status = ex.getResponseStatus
      val content = ex.getResponseContentBytes
      log.debug("Received response with status " + status + "," + (if (content == null) 0 else content.length) + " bytes long")
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

      ex.setRequestContent(new ByteArrayBuffer(data.getBytes))
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      ex.waitForDone
      new HttpResponse(ex.getResponseStatus, ex.headers, ex.getResponseContentBytes)
    }

    def post(data: Map[String,String]): HttpResponse = {
      val ex = new HattipContentExchange
      ex.setMethod("POST")
      ex.setURL(uri)

      ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
      val res = data map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
      log.debug("===>" + res)
      ex.setRequestContent(new ByteArrayBuffer(res.getBytes))
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      ex.waitForDone
      new HttpResponse(ex.getResponseStatus, ex.headers, ex.getResponseContentBytes)
    }

    def post(data: (String, String)*): HttpResponse = {
      val ex = new HattipContentExchange
      ex.setMethod("POST")
      ex.setURL(uri)

      ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
      val res = data map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
      log.debug("===>" + res)
      ex.setRequestContent(new ByteArrayBuffer(res.getBytes))
      headers foreach tupled(ex.addRequestHeader)
      httpClient.send(ex)
      ex.waitForDone
      new HttpResponse(ex.getResponseStatus, ex.headers, ex.getResponseContentBytes)
    }

    def postMulti(fields: (String, String)*): HttpResponse = {
      val ex = new HattipContentExchange
      ex.setMethod("POST")
      ex.setURL(uri)
//      val multi = new MultiPartOutputStream()
      ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
      val db = new DataBuffer()
//      val res = data map tupled(URLEncoder.encode(_,"UTF-8") + "=" + URLEncoder.encode(_,"UTF-8")) mkString "&"
//      log.debug("===>" + res)
//      ex.setRequestContent(new ByteArrayBuffer(res.getBytes))
//      headers foreach tupled(ex.addRequestHeader)
//      httpClient.send(ex)
//      ex.waitForDone
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
