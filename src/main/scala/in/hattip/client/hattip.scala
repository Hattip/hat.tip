package in.hattip.client
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileOutputStream
import java.net.URLEncoder
import java.nio.charset.Charset
import java.util.regex.Pattern
import scala.Function.tupled
import scala.collection.mutable.ListBuffer
import scala.xml.XML
import org.apache.http.entity.mime.content.FileBody
import org.apache.http.entity.mime.content.StringBody
import org.apache.http.entity.mime.HttpMultipartMode
import org.apache.http.entity.mime.MultipartEntity
import org.eclipse.jetty.client.security.HashRealmResolver
import org.eclipse.jetty.client.security.Realm
import org.eclipse.jetty.client.security.SimpleRealmResolver
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.io.{ Buffer => JettyBuffer }
import org.eclipse.jetty.io.ByteArrayBuffer
import org.eclipse.jetty.websocket.WebSocket
import com.codecommit.antixml
import com.weiglewilczek.slf4s.Logger
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocketClientFactory
import org.eclipse.jetty.websocket.WebSocketClient
import java.net.URI
import java.util.concurrent.TimeUnit

object Hattip {
  val log = Logger(getClass)

  trait HttpRequestTrait { self =>
    val scheme: String
    val host: String
    val port: Option[Int]
    val path: Option[String]
    val parameters: List[(String, String)]
    val headers: Map[String, String]
    val credentials: Option[(String, String, Option[String])]
    def uri: String = {
      val ret = scheme + "://" + host +
        (port map { ":" + _.toString }).getOrElse("") +
        (path map { "/" + _.toString }).getOrElse("")
      ret
    }

    def queryString: String = (parameters map tupled {
      URLEncoder.encode(_, "UTF-8") + "=" +
        URLEncoder.encode(_, "UTF-8")
    }).mkString("&")

    override def toString = {
      scheme + "://" + (credentials map { _._1 + "@" }) +
        host + (path map { "/" + _ }) +
        ("?" + ((parameters map { p => p._1 + "=" + p._2 }).mkString("&")))
    }

    def queryUri = if (parameters == Nil) uri else uri + "?" + queryString
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

  trait HttpResponseTrait {
    val code: HttpResponseCode
    val headers: Map[String, String]
    val bytes: Array[Byte]

    def string(encoding: String) = new String(bytes, encoding)
    def string = new String(bytes, "utf-8")
    def asXml = XML.loadString(string)
    def asAntiXml = antixml.XML.fromString(string)
    def process[T](f: PartialFunction[HttpResponseCode, T]) = f(code)
    def toFile(path: String) = {
      log.debug("Writing content to file:" + path)
      val fos = new FileOutputStream(path)
      fos.write(bytes)
      fos.close()
    }
    def >>> = toFile _

    override def toString = "Response(code = %d, contents = %s)" format (code, bytes)

  }

  class HattipContentExchange extends ContentExchange {
    private[this] val buffer = ListBuffer.empty[(String, String)]

    def headers = buffer.toMap

    override def onResponseHeader(name: JettyBuffer, value: JettyBuffer): Unit = {
      buffer.append((name.toString, value.toString))
      super.onResponseHeader(name, value)
    }
  }

  case class HttpRequest(
    val scheme: String,
    val host: String,
    val port: Option[Int],
    val path: Option[String],
    val credentials: Option[(String, String, Option[String])] = None,
    val parameters: List[(String, String)] = Nil,
    val headers: Map[String, String] = Map[String, String]())
    extends HttpRequestTrait {
    def /(p: String) = this.clone(
      pat = Some(path.getOrElse("") + p))
    def ?(name: String, value: String) = this.clone(
      par = (name -> value) :: parameters)
    def ?(pairs: (String, String)*) = this.clone(
      par = (parameters /: pairs)((pl, pair) => pair :: pl))

    def header(name: String, value: String) = this.clone(
      hea = headers + (name -> value))
    def header(pairs: (String, String)*) = this.clone(
      hea = (headers /: pairs)((hl, pair) => hl + pair))
    def as(userid: String, password: String) = this.clone(
      cre = Some(userid, password, None))
    def as(realm: String, userid: String, password: String) =
      this.clone(
        cre = Some(userid, password, Some(realm)))

    def clone(sch: String = scheme,
      hos: String = host,
      por: Option[Int] = port,
      pat: Option[String] = path,
      cre: Option[(String, String, Option[String])] = credentials,
      par: List[(String, String)] = parameters,
      hea: Map[String, String] = headers): HttpRequest = {
      new HttpRequest(sch, hos, por, pat, cre, par, hea)
    }
  }

  class HttpResponse(val code: HttpResponseCode,
    val headers: Map[String, String],
    val bytes: Array[Byte]) extends HttpResponseTrait {
    def text(encoding: String) = if (bytes != null) new String(bytes, encoding) else ""
    def text = if (bytes != null) new String(bytes, "utf-8") else ""
  }

  object Parser {
    val httpClient = new HttpClient
    httpClient.start
    httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL
    val schemePat = "([a-z][\\w-]+)"
    val schemeSep = ":/{1,3}"
    val domainPat = "([a-z0-9.\\-]+)"
    val portPat = "(?::([0-9]+))?"
    val domainSep = "/?"
    val pathPat = "([a-zA-Z0-9_.\\-()/]+)?"
    val varPat = "([a-zA-Z0-9_.\\-]+)"
    val kvPairPat = varPat + "=" + varPat
    val qsPat = "\\?(" + kvPairPat + ")"
    val qsRemPat = "((\\&(" + kvPairPat + "))*)"
    val pattern: Pattern = Pattern.compile("(?:" + schemePat + schemeSep + domainPat + portPat + domainSep + pathPat + ").*")
    def parse(str: String): HttpRequest = {
      // This method definitely needs further strengthening
      val m = pattern.matcher(str)
      if (m.matches) {
        val result: Array[String] = (for (i <- 1 to m.groupCount()) yield (m.group(i))) toArray;
        if (result.length == 4) {
          if (result(0) != null && result(1) != null) {
            val port = if (result(2) != null) Some(result(2).toInt) else None
            val path = if (result(3) != null) Some(result(3)) else None
            return HttpRequest(result(0), result(1), port, path)
          }
        }
      }
      throw new IllegalArgumentException("Invalid URI string")
    }
  }

  implicit def strToHttpRequest(str: String): HttpRequest = Parser.parse(str)

  def getClient = Parser.httpClient
  
  def get(r: HttpRequestTrait): HttpResponse = {
    var retry = true
    var retryCount = 0
    var req = r
    var response: HttpResponse = null
    while (retry && retryCount < 5) {
      retry = false
      retryCount += 1
      response = getOnce(req)
      response process {
        case Redirection() =>
          response.headers.get("Location") foreach { l =>
            req = Parser.parse(l)
            retry = true
          }
        case _ =>
      }
    }
    return response
  }

  private def setCredentials(r: HttpRequestTrait, ex: HattipContentExchange, httpClient: HttpClient) {
    r.headers foreach tupled(ex.addRequestHeader)

    r.credentials foreach { c =>
      val resolver = c._3 match {
        case None => new SimpleRealmResolver(
          new Realm {
            def getId = null
            def getPrincipal = c._1
            def getCredentials = c._2
          })
        case Some(realm) =>
          val res = new HashRealmResolver()
          res.addSecurityRealm {
            new Realm {
              def getId = realm
              def getPrincipal = c._1
              def getCredentials = c._2
            }
          }
          res
      }
      httpClient.setRealmResolver(resolver)
    }

  }
  def getOnce(r: HttpRequestTrait): HttpResponse = {
    val httpClient = getClient
    val ex = new HattipContentExchange
    ex.setURL(r.queryUri)
    setCredentials(r, ex, httpClient)
    httpClient.send(ex)
    ex.waitForDone()
    new HttpResponse(ex.getResponseStatus,
      ex.headers,
      ex.getResponseContentBytes)
  }

  private def prepare(method: String, r: HttpRequestTrait, httpClient: HttpClient): HattipContentExchange = {
    val ex = new HattipContentExchange
    ex.setMethod(method)
    ex.setURL(r.uri)
    setCredentials(r, ex, httpClient)
    ex
  }

  private def postPrepare(r: HttpRequestTrait, httpClient: HttpClient) = prepare("POST", r, httpClient)

  private def complete(ex: HattipContentExchange): HttpResponse = {
    log.debug("Completing content exchange")
    ex.waitForDone()
    new HttpResponse(ex.getResponseStatus,
      ex.headers,
      ex.getResponseContentBytes)
  }

  def post(r: HttpRequestTrait, data: Array[Byte]): HttpResponse = {
    val httpClient = getClient
    val ex = postPrepare(r, httpClient)
    ex.setRequestContent(new ByteArrayBuffer(data))
    httpClient.send(ex)
    complete(ex)
  }

  def post(r: HttpRequestTrait, data: (String, String)*): HttpResponse = {
    val httpClient = getClient
    val ex = postPrepare(r, httpClient)
    ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
    val content = (data map tupled { (key: String, value: String) =>
      URLEncoder.encode(key, "UTF-8") + "=" + URLEncoder.encode(value, "UTF-8")
    } toList).mkString("&");
    ex.setRequestContent(new ByteArrayBuffer(content getBytes))
    httpClient.send(ex)
    complete(ex)
  }

  def post(r: HttpRequestTrait, data: Map[String, String]): HttpResponse = {
    val httpClient = getClient
    val ex = postPrepare(r, httpClient)
    ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8")
    val content = (data map tupled { (key: String, value: String) =>
      URLEncoder.encode(key, "UTF-8") + "=" + URLEncoder.encode(value, "UTF-8")
    } toList).mkString("&");
    ex.setRequestContent(new ByteArrayBuffer(content getBytes))
    httpClient.send(ex)
    complete(ex)
  }
  private def toMultipart(boundary: String, fields: Map[String, String], files: Map[String, String]): MultipartEntity = {
    val entity = new MultipartEntity(HttpMultipartMode.STRICT, boundary, Charset.forName("UTF-8"));
    fields foreach tupled { (name, value) =>
      entity.addPart(name, new StringBody(value, Charset.forName("UTF-8")));
    }
    files foreach tupled { (name, path) =>
      entity.addPart(name, new FileBody(new File(path)))
    }
    entity
  }
  def post(r: HttpRequestTrait, fields: Map[String, String], files: Map[String, String]): HttpResponse = {
    val boundary = "hattip" + System.identityHashCode(this) + java.lang.Long.toString(System.currentTimeMillis(), 36)
    val multipart = toMultipart(boundary, fields, files)
    val httpClient = getClient
    val ex = postPrepare(r, httpClient)
    ex.setRequestContentType("multipart/form-data; boundary=" + boundary)
    val outputStream = new ByteArrayOutputStream()
    multipart.writeTo(outputStream)
    ex.setRequestContent(new ByteArrayBuffer(outputStream.toByteArray()))
    log.debug("Content length:" + multipart.getContentLength())
    // log.debug("================ Content ================")
    // log.debug(outputStream.toString())
    httpClient.send(ex)
    complete(ex)
  }

  def put(r: HttpRequestTrait, data: Array[Byte]): HttpResponse = {
    val httpClient = getClient
    val ex = prepare("PUT", r, httpClient)
    ex.setRequestContent(new ByteArrayBuffer(data))
    httpClient.send(ex)
    complete(ex)
  }

  def delete(r: HttpRequestTrait): HttpResponse = {
    val httpClient = getClient
    val ex = prepare("DELETE", r, httpClient)
    httpClient.send(ex)
    complete(ex)
  }

  object WsConnection {
    private val factory = new WebSocketClientFactory
    factory.start

    def apply(protocol: String): WebSocketClient = {
      val client = factory.newWebSocketClient
      client setProtocol protocol
      client setMaxIdleTime 1800000
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
      binaryHandler foreach (_(data.slice(offset, offset + length)))
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
      connection.sendMessage(message, 0, message.length)
    }

    def close(): Unit = {
      connection.close
    }
  }

  def open(r: HttpRequestTrait, protocol: String) = {
    val client = WsConnection(protocol)
    val wSocket = new WrappedWebSocket
    val future = client.open(new URI(r.uri), wSocket)
    val connection = future.get(10, TimeUnit.SECONDS)
    new WrappedConnection(connection, wSocket)
  }
}

