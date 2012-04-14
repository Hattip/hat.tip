package in.hattip.testserver
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample
import org.specs2.runner.JUnitRunner
import org.scalatra.ScalatraServlet
import org.scalatra.fileupload.FileUploadSupport
import org.eclipse.jetty.server.ssl.SslSocketConnector
import org.eclipse.jetty.server.Server
import com.weiglewilczek.slf4s.Logger
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.servlet.ServletHolder
import in.hattip.client.Hattip._
import org.eclipse.jetty.util.ssl.SslContextFactory

@RunWith(classOf[JUnitRunner])
class TestSslServer extends SpecificationWithJUnit with BeforeExample with AfterExample {
  val server = new SslServer()
  def before = server.start()
  def after = server.stop()
  "Simple Getter" should {
    "be able to get a simple get" in {
      val response = get("https://localhost:8443")
      response process {
        case Success() =>
          response.text must_== "This is the https server"
          success
        case _ => failure
      }
    }
  }
}

class SslServlet extends ScalatraServlet with FileUploadSupport {
  val log = Logger(getClass)
  get("/") {
    "This is the https server"
  }
}

class SslServer {
  val server = new Server()
  val factory = new SslContextFactory()
  factory.setKeyStorePath("src/test/resources/keystore")
  factory.setKeyStorePassword("keystore")
  factory.setKeyManagerPassword("keystore")
  factory.setTrustStore("src/test/resources/keystore")
  factory.setTrustStorePassword("keystore")

  val connector = new SslSocketConnector(factory)
  connector.setPort(8443)
  server.addConnector(connector)

  val context = new ServletContextHandler(ServletContextHandler.NO_SESSIONS);
  context setContextPath "/"
  server setHandler context

  context addServlet (new ServletHolder(new SslServlet()), "/*");

  def start() = server.start()

  def stop() = server.stop()
}