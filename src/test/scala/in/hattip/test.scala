package in.hattip
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample

import com.codecommit.antixml.Selector.stringToSelector

import in.hattip.Hattip.str2HttpEndpoint

@RunWith(classOf[JUnitRunner])
class TestHattip extends SpecificationWithJUnit with BeforeExample with AfterExample {
  sequential
  def before = {
    TestServer.start()
    SecureServer.start()
    WebsocketServer.start()
  }
  def after = {
    TestServer.stop()
    SecureServer.stop()
    WebsocketServer.stop()
  }
  val host = "http://localhost:8088"
  val host2 = "http://localhost:9088"
  val wsHost = "ws://localhost:9999"

  /* doesn't compile, as expected.
  "The HttpEndpoint" should {
    "should fail to construct" in {
      val x = ("http://foo.com" / "ko") ? ("89" -> "34") / "ko"
      true must_== true
    }
  }
  */

  "The hattip client library" should {
    "fetch a page successfully" in {
      val resp = host get;
      resp.code must_== 200
      resp.str must contain("<html>Hello World!</html>")
    }

    "be able to pass headers successfully" in {
      val resp = host withHeaders(
          "foo" -> "bar", 
          "baz"->"jaz",
          "Content-type" -> "application/xml",
          "Accept"->"application/xml") get;

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
      val xml = (host / "index.xml" get) asAntiXml;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
    }

    "be able to detect 401 against secure resources" in {
      var authErrorDetected = false;
      host2 / "secure" / "index.html" get { resp => ;} onErrorCode {
        case(401) => authErrorDetected = true
      }

      authErrorDetected must_==(true)
    }

    "be able perform basic authentication" in {
      var authenticated = false;
      host2 / "secure" / "index.html" secure("realm", "jetty", "jetty") get { resp =>
        authenticated = true
      }

      authenticated must_==(true)
    }
    
    "be able to send a websocket text message" in {
      val conn = wsHost open "in.hattip.hattip";
      conn ! "message .. message .. message"
      true must_==(true)
    }
    "be able to read a websocket text message" in {
      val conn = wsHost open "in.hattip.hattip";
      var receivedPong = false
      conn onMessage { msg =>
        receivedPong = msg == "pong"
      }
      conn ! "ping"
      // sleep to wait to get the response back
      Thread.sleep(2000)
      receivedPong must_==(true)
    }
  }
}
