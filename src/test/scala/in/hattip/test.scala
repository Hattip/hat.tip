package in.hattip
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.AfterExample
import org.specs2.specification.BeforeExample
import com.codecommit.antixml.Selector.stringToSelector
import in.hattip.Hattip.str2HttpEndpoint
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestHattip extends SpecificationWithJUnit with BeforeExample with AfterExample {
  def before = TestServer.start()
  def after = TestServer.stop()
  val host = "http://localhost:8088"

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
  }
}
