package in.nene.hattip
import javax.servlet.http.HttpServletRequest
import org.eclipse.jetty.server.Server
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.Request
import scala.collection.mutable.ListBuffer

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
      }
      baseReq.setHandled(true)
    }
}        

object TestServer {
    val server = new Server(8088)
    server.setHandler(new TestHandler());
    def start() {
        server.start()
    }
    def stop() {
      server.stop()
    }
}

