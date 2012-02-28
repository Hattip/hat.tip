package in.nene.hattip
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.Server
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import in.nene.hattip.Hattip._
import java.nio.CharBuffer
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

@RunWith(classOf[JUnitRunner])
class TestHattip extends SpecificationWithJUnit with BeforeExample with AfterExample {
  def before = TestServer.start()
  def after = TestServer.stop()
  val host = "http://localhost:8088"
    
  "The hattip client library" should { 
    "fetch a page successfully" in { 
      val resp = host get;
      resp.code must_== 200
      resp.str must contain("<html>Hello World!</html>")
    }
    
    "detect a 404 or other http codes appropriately" in {
      val resp = host / "nonexistent.file" get;
      resp.code must_== 404
    }
    
    "skip the normal handler block in case of non 200, yet reach the error handler" in {
      var enteredSuccessBlock = false;
      var capturedInErrorBlock = false;
      val resp = host / "nonexistent.file" get {
        rsp => enteredSuccessBlock = true;
      } onErrorCode {
        case(404) => capturedInErrorBlock = true;
        case _ => ;
      }
      enteredSuccessBlock must_==(false)
      capturedInErrorBlock must_==(true)
    }
    
    "post data successfully" in { 
      val resp = host / "post.do" post("""This is 
            very very long
              a long string""");
      resp.code must_== 200
      resp.str must contain("very very long")
    }
    
    "parse data as xml" in {
      val xml = (host / "index.xml" get) asXml;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
    }
    
    "parse data as anti-xml" in {
      val xml = (host / "index.xml" get) asAnti;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
    }
  }
}
