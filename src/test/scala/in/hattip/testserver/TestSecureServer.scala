package in.hattip.testserver
import scala.collection.JavaConverters._

import org.eclipse.jetty.security.authentication.BasicAuthenticator
import org.eclipse.jetty.security.ConstraintMapping
import org.eclipse.jetty.security.ConstraintSecurityHandler
import org.eclipse.jetty.security.HashLoginService
import org.eclipse.jetty.security.SecurityHandler
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.toolchain.test.MavenTestingUtils
import org.eclipse.jetty.util.security.Constraint
import org.eclipse.jetty.util.security.Credential
import org.junit.runner.RunWith
import org.scalatra.auth.strategy.BasicAuthSupport
import org.scalatra.fileupload.FileUploadSupport
import org.scalatra.ScalatraServlet
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample

import in.hattip.client.Hattip._

@RunWith(classOf[JUnitRunner])
class TestSecureServer extends SpecificationWithJUnit with BeforeExample with AfterExample {
  val server = new SecureServer()

  def before = server.start()
  def after = server.stop()

  "Secure Getter" should {
    "be able to perform a secure get" in {
      val response = get("http://localhost:8080/index.txt" as ("scott", "tiger"))
      response process {
        case Success() =>
          response.text must_== "This is secured content"
          success
        case _ => failure
      }
    }
    "be able to perform a secure get with realm" in {
      val response = get("http://localhost:8080/index.txt" as ("hattip.test", "scott", "tiger"))
      response process {
        case Success() =>
          response.text must_== "This is secured content"
          success
        case _ => failure
      }
    }
    "fail when incorrect password supplied to perform a secure get" in {
      val response = get("http://localhost:8080/index.txt" as ("scott", "toger"))
      response process {
        case AccessControlFailure() =>
          success
        case _ => failure
      }
    }
    "fail when incorrect realm supplied to perform a secure get" in {
      val response = get("http://localhost:8080/index.txt" as ("hattip.fail", "scott", "tiger"))
      response process {
        case AccessControlFailure() =>
          success
        case _ => failure
      }
    }
  }
}

class SecureServlet extends ScalatraServlet with FileUploadSupport {
  get("/index.txt") {
    "This is secured content"
  }
}

class SecureServer {
  val server = new Server(8080)
  val context = new ServletContextHandler(ServletContextHandler.SECURITY);
  context.setSecurityHandler(basicAuth("scott", "tiger", "hattip.test"));
  context setContextPath "/"
  server setHandler context

  context addServlet (new ServletHolder(new SecureServlet()), "/*");

  def basicAuth(username: String, password: String, realm: String): SecurityHandler = {
    val l = new HashLoginService();
    l.putUser(username, Credential.getCredential(password), Array[String]("user"));
    l.setName(realm);

    val constraint = new Constraint();
    constraint.setName(Constraint.__BASIC_AUTH);
    constraint.setRoles(Array[String]("user"));
    constraint.setAuthenticate(true);

    val cm = new ConstraintMapping();
    cm.setConstraint(constraint);
    cm.setPathSpec("/*");

    val csh = new ConstraintSecurityHandler();
    csh.setAuthenticator(new BasicAuthenticator());
    csh.setRealmName(realm);
    csh.addConstraintMapping(cm);
    csh.setLoginService(l);

    return csh;
  }

  def start() = server.start()

  def stop() = server.stop()
}
