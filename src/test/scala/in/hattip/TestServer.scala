package in.hattip
import scala.collection.JavaConverters._
import scala.collection.SortedMap
import org.eclipse.jetty.security.authentication.BasicAuthenticator
import org.eclipse.jetty.security.ConstraintMapping
import org.eclipse.jetty.security.ConstraintSecurityHandler
import org.eclipse.jetty.security.HashLoginService
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.handler.ResourceHandler
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.ssl.SslSocketConnector
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.toolchain.test.MavenTestingUtils
import org.eclipse.jetty.util.security.Constraint
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketHandler
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Connector

class RequestListener {
  var lastRequest: Option[Req] = None
  def send(req: Req) {
    lastRequest = Some(req)
  }
  def clear() {
    lastRequest = None
  }
  def get = lastRequest
}

case class Req(val method: String, val uri: String, map: SortedMap[String,Array[String]], headers: List[(String,String)], data: String = "", secure: Boolean = false) {
  override def equals(other: Any): Boolean = {
    if(other.isInstanceOf[Req]) {
      val that = other.asInstanceOf[Req]
      if(this.method == that.method && this.uri == that.uri && this.headers == that.headers && this.data == that.data) {
        (this.map zip that.map) forall { pair =>
           pair._1._1 == pair._2._1 && (pair._1._2.zip(pair._2._2).forall {ipair => ipair._1 == ipair._2})
        }
      } else {
        return false
      }
    }
    else return false
  }
}

class TestHandler(l: RequestListener) extends AbstractHandler {
    def handle(target : String, baseReq : Request, request : HttpServletRequest, response : HttpServletResponse) {
      // set by default. Change later if required
      response.setContentType("text/html;charset=utf-8")
      response.setStatus(HttpServletResponse.SC_OK)
      val headers = request.getHeaderNames().asScala map { hdrname =>
        (hdrname, request.getHeader(hdrname))
      } toList
      var bytes = new Array[Byte](4096)
      val count = request.getInputStream().read(bytes)
      val params = SortedMap(request.getParameterMap.asScala.toArray:_*)
      
      val req = if (count <= 0) 
              Req(baseReq.getMethod(), baseReq.getRequestURI(), params, headers sorted, secure = request.isSecure())
            else {
              Req(baseReq.getMethod(), baseReq.getRequestURI(), params, headers sorted, new String(bytes.take(count)),request.isSecure())
            }
              
      l.send(req)
      
      req match {
        case Req("GET","/",_,_,_,_) =>
          response.getWriter().println("<html>Hello World!</html>")
        case Req("GET","/index.html",_,_,_,_) =>
          response.getWriter().println("<html>Hello World!</html>")
        case Req("GET","/index.xml",_,_,_,_) =>
          response.setContentType("text/xml;charset=utf-8")
          response.getWriter().println("""
              <project name="hat.tip">
                  <dependencies>
                      <dependency group="org.eclipse.jetty.aggregate" name="jetty-websocket"/>
                      <dependency group="org.eclipse.jetty.aggregate" name="jetty-client"/>
                      <dependency group="com.codecommit" name="anti-xml"/>
                  </dependencies>
              </project>
              """)
        case Req("GET","/nonexistent.file",_,_,_,_) =>
          response.setStatus(HttpServletResponse.SC_NOT_FOUND)
        case Req("POST","/post.do",_,_,_,_) =>
          response.getWriter().println(req.data)
        case Req("GET","/301.html",_,_,_,_) =>
          response.setStatus(301)
          response.setHeader("Location", "http://localhost:8088/index.html")
          response.setContentType("text/html")
          response.getWriter().println("<html><head><title>Moved</title></head></html>")
        case Req("GET","/secure/index.html",_,_,_,_) =>
          response.getWriter().println("<html>Hello Secure World!</html>")
      }
      baseReq.setHandled(true)
    }
}

object TestServer {
    val server = new Server(8088)

    def start(l: RequestListener) {
      server.setHandler(new TestHandler(l))
      server.start()
    }
    def stop() {
      server.stop()
    }
    
}

object SslServer {
	val server = new Server()
	val connector = new SslSocketConnector()
	connector.setPort(8443)
	connector.setKeystore("src/test/resources/keystore")
	connector.setKeyPassword("keystore")
	connector.setPassword("keystore")
	connector.setTruststore("src/test/resources/keystore")
	connector.setTrustPassword("keystore")
	server.addConnector(connector)
	
    def start(l: RequestListener) {
      server.setHandler(new TestHandler(l))
      server.start()
    }
    def stop() {
      connector.stop()
      server.stop()
    }
}

object SecureServer {
    // have trouble installing multiple handlers on the same server
    // hence currently starting two servers
  val server2 = new Server(9088)
  def start(l: RequestListener) {
    server2.setHandler(getSecureHandler(l: RequestListener))
    server2.start()
  }
  def stop() {
    server2.stop()
  }

  def getSecureHandler(l: RequestListener): Handler = {
  val constraint = new Constraint()
  constraint setName Constraint.__BASIC_AUTH
  constraint setRoles Array[String]("user","admin","moderator")
  constraint setAuthenticate true
  
  val cm = new ConstraintMapping()
  cm.setConstraint(constraint);
  cm.setPathSpec("/secure/*");
  
  val sh = new ConstraintSecurityHandler()
  sh.setConstraintMappings(Array[ConstraintMapping](cm));
  sh.setHandler(new TestHandler(l))
  sh.setRealmName("realm")
  
  val realmPropFile = MavenTestingUtils.getTestResourceFile("realm.properties");
  val loginService = new HashLoginService("realm",realmPropFile.getAbsolutePath());
  sh.setLoginService(loginService);
  sh.setAuthenticator(new BasicAuthenticator());
  sh
  }
}

class TestWebSocket extends WebSocket.OnTextMessage with WebSocket.OnBinaryMessage {
  var connection: Option[Connection] = None
  
  def onOpen(connection: Connection) {
    this.connection = Some(connection)
  }
  def onClose(code: Int, message: String) {
    
  }
  def onMessage(message: String) {
    if (message == "ping") {
      this.connection foreach { _.sendMessage("pong")}
    }
  } 
  def onMessage(data: Array[Byte], offset: Int, length: Int) {
    this.connection foreach {
      _.sendMessage(data.slice(offset,offset+length).reverse,0,length)
    }
  }
}
object WebsocketServer extends Server {
  val connector = new SelectChannelConnector()
  connector.setPort(9999)
  addConnector(connector)
  
  val handler = new WebSocketHandler() {
    def doWebSocketConnect(request: HttpServletRequest, protocol: String): WebSocket = {
      new TestWebSocket();
    }
  }
  setHandler(handler)
  
  val rHandler = new ResourceHandler()
  rHandler.setDirectoriesListed(false)
  rHandler.setResourceBase(System.getProperty("user.dir"))
  handler.setHandler(rHandler)
  
  
}
