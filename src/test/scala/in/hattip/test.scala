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
  val listener = new RequestListener()
  def before = {
    TestServer.start(listener)
    SecureServer.start(listener)
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
      listener.clear()
      val resp = host.get
      resp.code must_== _200
      resp.str must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/",
            List[(String,String)](("Host","localhost:8088"))))
    }

    "be able to pass headers successfully" in {
      listener.clear()
      val resp = host withHeaders(
          "foo" -> "bar", 
          "baz"->"jaz") get

      resp.code must_== _200
      resp.str must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/",
            List[(String,String)](
                ("Host","localhost:8088"), 
                ("foo","bar"),
                ("baz","jaz")) sorted))
    }

    "detect a 404 or other http codes appropriately" in {
      var matchedNotFound = false
      listener.clear()
      val resp = (host / "nonexistent.file").get
      resp.code must_== _404
      
      resp process {
        case Success() => ;
        case NotFound() => matchedNotFound = true
        case _ => ;
      }
      
      matchedNotFound must_==(true)
      listener.get must_== Some(Req("GET","/nonexistent.file",
            List[(String,String)](("Host","localhost:8088"))))
    }

    "skip the normal handler block in case of non 200, yet reach the error handler" in {
      listener.clear()
      var enteredSuccessBlock = false;
      var capturedInErrorBlock = false;
      val resp = (host / "nonexistent.file" get) process {
        case(Success()) => enteredSuccessBlock = true;
        case(NotFound()) => capturedInErrorBlock = true;
        case _ => ;
      }
      enteredSuccessBlock must_==(false)
      capturedInErrorBlock must_==(true)
      listener.get must_== Some(Req("GET","/nonexistent.file",
                                    List[(String,String)](("Host","localhost:8088"))))
   }

    "post data successfully" in {
      listener.clear()
      val longString = """This is
            very very long
              a long string"""
      val resp = host / "post.do" post(longString);
      resp.code must_== _200
      resp.str must contain("very very long")
      val data = listener.get.get.data
      listener.get must_== Some(Req("POST","/post.do",List[(String,String)](
          ("Host","localhost:8088"),
          ("Content-Length", longString.length().toString()),
          ("Content-Type","application/x-www-form-urlencoded;charset=utf-8")
          ) sorted,longString))
   }

    "parse data as xml" in {
      listener.clear()
      val xml = (host / "index.xml" get) asXml;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
      listener.get must_== Some(Req("GET","/index.xml",
                                    List[(String,String)](("Host","localhost:8088"))))
    }

    "parse data as anti-xml" in {
      listener.clear()
      val xml = (host / "index.xml" get) asAntiXml;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
      listener.get must_== Some(Req("GET","/index.xml",
                                    List[(String,String)](("Host","localhost:8088"))))
    }

    "be able to detect 401 against secure resources" in {
      listener.clear()
      var authErrorDetected = false;
      (host2 / "secure" / "index.html" get) process {
        case(_401) => authErrorDetected = true
      }

      authErrorDetected must_==(true)
      listener.get must_== None
    }

    "be able perform basic authentication" in {
      listener.clear()
      var authenticated = false;
      (host2 / "secure" / "index.html" secure("realm", "jetty", "jetty") get) process { 
        case(Success()) => authenticated = true
      }

      authenticated must_==(true)
      listener.get must_== Some(Req("GET","/secure/index.html",
                                    List[(String,String)](
                                        ("Host","localhost:9088"),
                                        ("Authorization","Basic amV0dHk6amV0dHk=")) sorted))
    }
    
    "be able to send a websocket text message" in {
      listener.clear()
      val conn = wsHost open "in.hattip.hattip";
      conn ! "message .. message .. message"
      true must_==(true)
    }
    "be able to read a websocket text message" in {
      listener.clear()
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
