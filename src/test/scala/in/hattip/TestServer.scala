package in.hattip
import scala.collection.mutable.ListBuffer
import org.eclipse.jetty.security.ConstraintMapping
import org.eclipse.jetty.security.ConstraintSecurityHandler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.Connector
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.util.security.Constraint
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.server.handler.HandlerCollection
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.security.authentication.BasicAuthenticator
import org.eclipse.jetty.security.HashLoginService
import org.eclipse.jetty.toolchain.test.MavenTestingUtils

case class Req(val method: String, val uri: String)

class TestHandler extends AbstractHandler {
    def handle(target : String, baseReq : Request, request : HttpServletRequest, response : HttpServletResponse) {
      // set by default. Change later if required
      response.setContentType("text/html;charset=utf-8")
      response.setStatus(HttpServletResponse.SC_OK)


      val req = Req(baseReq.getMethod(), baseReq.getRequestURI())
      req match {
        case Req("GET","/") =>
          response.getWriter().println("<html>Hello World!</html>")
        case Req("GET","/index.html") =>
          response.getWriter().println("<html>Hello World!</html>")
        case Req("GET","/index.xml") =>
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
        case Req("GET","/nonexistent.file") =>
          response.setStatus(HttpServletResponse.SC_NOT_FOUND)
        case Req("POST","/post.do") =>
          var content = new ListBuffer[String]()
          val reader = request.getReader()
          var line = reader.readLine()
          while (line != null)
          {
              line = reader.readLine()
              content += line
          }
          val body = content.toList
          response.getWriter().println(body)
        case Req("GET","/secure/index.html") =>
          response.getWriter().println("<html>Hello Secure World!</html>")
      }
      baseReq.setHandled(true)
    }
}

object TestServer {
    val server = new Server(8088)

    def start() {
      server.setHandler(new TestHandler())
      server.start()
    }
    def stop() {
      server.stop()
    }

}

object SecureServer {
    // have trouble installing multiple handlers on the same server
    // hence currently starting two servers
    val server2 = new Server(9088)
    def start() {
      server2.setHandler(getSecureHandler())
      server2.start()
    }
    def stop() {
      server2.stop()
    }

    def getSecureHandler(): Handler = {

    val constraint = new Constraint()
    constraint setName Constraint.__BASIC_AUTH
    constraint setRoles Array[String]("user","admin","moderator")
    constraint setAuthenticate true

    val cm = new ConstraintMapping()
    cm.setConstraint(constraint);
    cm.setPathSpec("/secure/*");

    val sh = new ConstraintSecurityHandler()
    sh.setConstraintMappings(Array[ConstraintMapping](cm));
    sh.setHandler(new TestHandler())
    sh.setRealmName("realm")
      val realmPropFile = MavenTestingUtils.getTestResourceFile("realm.properties");
      val loginService = new HashLoginService("realm",realmPropFile.getAbsolutePath());
      sh.setLoginService(loginService);
      sh.setAuthenticator(new BasicAuthenticator());

    sh
  }
}
