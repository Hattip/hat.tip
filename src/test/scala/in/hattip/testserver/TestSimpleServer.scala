package in.hattip.testserver
import scala.collection.JavaConverters._

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.servlet.ServletHolder
import org.junit.runner.RunWith
import org.scalatra.fileupload.FileUploadSupport
import org.scalatra.ScalatraServlet
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample

import in.hattip.client.Hattip._

@RunWith(classOf[JUnitRunner])
class TestSimpleServer extends SpecificationWithJUnit  with BeforeExample with AfterExample {
  val server = new SimpleServer()
  
  def before = server.start()
  def after = server.stop()
  
  "Simple Getter" should {
    "be able to get a simple get" in {
      val response = get("http://localhost:8080")
      response process {
        case Success() => 
          response.text must_== "This is the test server"
        case _ => failure
      }
      success
    }
    "be able to read a xml" in {
      val response = get("http://localhost:8080/dummy.xml")
      response process {
        case Success() => 
          val root = response.asAntiXml
          root.name must_== "dummyxml"
          root.attrs.isEmpty must_== true
          val nodes = root \ "node" map { node =>
            (node.name, node.attrs.get("attr1"), node.attrs.get("attr2"))
          } toList;
          nodes must_== List(("node",Some("val11"),Some("val12")), 
        		  			 ("node",Some("val21"),Some("val22")))
          success
        case _ => failure
      }
    }
    "be able to deal with a 404" in {
      val response = get("http://localhost:8080/nonexistent.html")
      response process {
        case Success() => failure
        case NotFound() => success
        case _ => failure
      }
    }
    "redirect to a file successfully" in {
      get("http://localhost:8080") >>> "foo.txt"
      val lines = scala.io.Source.fromFile("foo.txt").mkString
      lines must_== "This is the test server"
      success
    }
    "handle redirects successfully" in {
      val response = get("http://localhost:8080/moved.txt") 
      response process {
        case Success() => 
          response.text must_== "This is the relocated file"
          success
        case _ => failure
      }
    }
    "passes custom headers appropriately" in {
      val response = get("http://localhost:8080/echoheaders.xml" header("foo","bar")) 
      response process {
        case Success() => 
          response.text must_== "<headers><header name=\"Host\" values=\"localhost:8080\"/><header name=\"foo\" values=\"bar\"/></headers>"
          success
        case _ => failure
      }
    }
    "passes query string parameters appropriately" in {
      val response = get("http://localhost:8080/echoquerystring.xml" ? ("foo"->"bar","fizz" -> "buzz", "fizz" -> "fazz"))
      val foo = response.text
      response process {
        case Success() => 
          response.text must_== "<parameters><parameter name=\"foo\" values=\"bar\"/><parameter name=\"fizz\" values=\"fazz,buzz\"/></parameters>"
          success
        case _ => failure
      }
    }
    "urlencode query string parameters appropriately" in {
      val response = get("http://localhost:8080/echoquerystring.xml" ? ("f+o o" -> "b a r", "baz" -> "jaz"))
      val foo = response.text
      response process {
        case Success() => 
          response.text must_== "<parameters><parameter name=\"f+o o\" values=\"b a r\"/><parameter name=\"baz\" values=\"jaz\"/></parameters>"
          success
        case _ => failure
      }
    }
  }
}

class SimpleServlet extends ScalatraServlet with FileUploadSupport {
  get("/") {
    "This is the test server"
  }
  
  get("/dummy.xml") {
    response.setHeader("Content-type","text/xml")
    <dummyxml>
	  <node attr1="val11" attr2="val12"/>
	  <node attr1="val21" attr2="val22"/>
    </dummyxml>
  }
  
  get("/movedhere.txt") {
    "This is the relocated file"
  }
  
  get("/moved.txt") {
    redirect("/movedhere.txt")
  }
  
  get("/echoheaders.xml") {
    val p = request.getHeaderNames().asScala map { name: String =>
      "<header name=\"" + name + "\" values=\"" + request.getHeaders(name).asScala.mkString(",") + "\"/>"
    }
    p.toList.mkString("<headers>","","</headers>")
  }

  get("/echoquerystring.xml") {
    println(request.getParameterNames().asScala)
    val p = request.getParameterNames().asScala map { name: String =>
      "<parameter name=\"" + name + "\" values=\"" + request.getParameterValues(name).mkString(",") + "\"/>"
    }
    p.toList.mkString("<parameters>","","</parameters>")
  }
//  get("/") {
//    <form method="post" enctype="multipart/form-data">
//      <p>Old File: <input type="file" name="oldFile" /></p>
//      <p>New File: <input type="file" name="newFile" /></p>
//      <input type="submit" />
//    </form>
//  }
}

class SimpleServer {
  val server = new Server(8080)
  def start() = {
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS);
    context setContextPath "/"
    server setHandler context
 
    context addServlet(new ServletHolder(new SimpleServlet()),"/*");
    server.start()
  }
  def stop() = server.stop()
}

object SimpleServer {
  def main(args: Array[String]) = {
    val server = new SimpleServer()
    server.start
    Thread.sleep(30000)
    server.stop
  }
}