package in.hattip
import scala.collection.immutable.SortedMap

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample

import com.codecommit.antixml.Selector.stringToSelector

import in.hattip.Hattip._

@RunWith(classOf[JUnitRunner])
class TestHattip extends SpecificationWithJUnit with BeforeExample with AfterExample {
  sequential
  val listener = new RequestListener()
  val emptyQueryStringMap = SortedMap[String,Array[String]]()
  def before = {
    TestServer.start(listener)
    SecureServer.start(listener)
    WebsocketServer.start()
    SslServer.start(listener)
  }
  def after = {
    TestServer.stop()
    SecureServer.stop()
    WebsocketServer.stop()
    SslServer.stop()
  }
  val host = "http://localhost:8088"
  val host2 = "http://localhost:9088"
  val wsHost = "ws://localhost:9999"
  val sslHost = "https://localhost:8443"
    
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
      val response = host.get
      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/",emptyQueryStringMap,
            List[(String,String)](("Host","localhost:8088"))))
    }

    "be able to handle redirects successfully" in {
      listener.clear()
      val response = host / "301.html" get;
      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/index.html",emptyQueryStringMap,
            List[(String,String)](("Host","localhost:8088"))))
    }

    "be able to pass headers successfully" in {
      listener.clear()
      val response = host.withHeaders(
          "foo" -> "bar", 
          "baz"->"jaz").get

      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/",emptyQueryStringMap,
            List[(String,String)](
                ("Host","localhost:8088"), 
                ("foo","bar"),
                ("baz","jaz")) sorted))
    }

    "build query string successfully" in {
      listener.clear()
      val response = ((host / "index.html") ? ("foo" -> "bar", "baz" -> "jaz")).get
      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/index.html",
          SortedMap("foo" -> Array[String]("bar"), "baz" -> Array[String]("jaz")),
            List[(String,String)](("Host","localhost:8088"))))
    }

    "urlencode a query string successfully" in {
      listener.clear()
      val response = ((host / "index.html") ? ("f+o o" -> "b a r", "baz" -> "jaz")).get
      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/index.html",
          SortedMap("f+o o" -> Array[String]("b a r"), "baz" -> Array[String]("jaz")),
            List[(String,String)](("Host","localhost:8088"))))
    }

    "detect a 404 or other http codes appropriately" in {
      var matchedNotFound = false
      listener.clear()
      val response = (host / "nonexistent.file").get
      response.code must_== 404
      
      response process {
        case Success() => ;
        case NotFound() => matchedNotFound = true
        case _ => ;
      }
      
      matchedNotFound must_==(true)
      listener.get must_== Some(Req("GET","/nonexistent.file",emptyQueryStringMap,
            List[(String,String)](("Host","localhost:8088"))))
    }

    "skip the normal handler block in case of non 200, yet reach the error handler" in {
      listener.clear()
      var enteredSuccessBlock = false;
      var capturedInErrorBlock = false;
      val response = (host / "nonexistent.file" get) process {
        case(Success()) => enteredSuccessBlock = true;
        case(NotFound()) => capturedInErrorBlock = true;
        case _ => ;
      }
      enteredSuccessBlock must_==(false)
      capturedInErrorBlock must_==(true)
      listener.get must_== Some(Req("GET","/nonexistent.file",emptyQueryStringMap,
                                    List[(String,String)](("Host","localhost:8088"))))
   }

    "post data successfully" in {
      listener.clear()
      val longString = """This is
            very very long
              a long string"""
      val response = host / "post.do" post(longString);
      response.code must_== 200
      response.contents must contain("very very long")
      val data = listener.get.get.data
      listener.get must_== Some(Req("POST","/post.do",emptyQueryStringMap,List[(String,String)](
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
      listener.get must_== Some(Req("GET","/index.xml",emptyQueryStringMap,
                                    List[(String,String)](("Host","localhost:8088"))))
    }

    "parse data as anti-xml" in {
      listener.clear()
      val xml = (host / "index.xml" get) asAntiXml;
      val dependencies = xml\\"dependencies"\"dependency"
      dependencies.length must_==(3)
      listener.get must_== Some(Req("GET","/index.xml",emptyQueryStringMap,
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

    "fetch a page successfully over https" in {
      listener.clear()
      val response = sslHost.get
      response.code must_== 200
      response.contents must contain("<html>Hello World!</html>")
      listener.get must_== Some(Req("GET","/",emptyQueryStringMap,
            List[(String,String)](("Host","localhost:8443")),secure=true))
    }

    "be able perform basic authentication" in {
      listener.clear()
      var authenticated = false;
      (host2 / "secure" / "index.html" as("realm", "jetty", "jetty") get) process { 
        case(Success()) => authenticated = true
      }

      authenticated must_==(true)
      listener.get must_== Some(Req("GET","/secure/index.html",emptyQueryStringMap,
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
      conn setMessageHandler { msg =>
        receivedPong = msg == "pong"
      }
      conn ! "ping"
      // sleep to wait to get the response back
      Thread.sleep(2000)
      receivedPong must_==(true)
    }
  }
}
