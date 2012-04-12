package in.hattip.testserver

import org.specs2.mutable.SpecificationWithJUnit
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import in.hattip.client.Hattip._
import in.hattip.client.Hattip.Parser._

@RunWith(classOf[JUnitRunner])
class TestParser extends SpecificationWithJUnit {
  "Http URI Parser" should {
    "Parse URI correctly" in {
      parse("http://abcd.efgh.com:8080/abcd/efgh/index.html?foo=bar&baz=buz") must_==
        HttpRequest("http","abcd.efgh.com",Some(8080),Some("abcd/efgh/index.html"))
    }
  
    "Parse IP Address correctly" in {
      parse("http://127.0.0.1:8080/abcd/efgh/index.html") must_==
        HttpRequest("http","127.0.0.1",Some(8080),Some("abcd/efgh/index.html"))
    }
  
    "Parse non file ending URI correctly" in {
      parse("http://localhost:8080/abcd/efgh/") must_==
        HttpRequest("http","localhost",Some(8080),Some("abcd/efgh/"))
    }
    
    "Parse non port URI correctly" in {
      parse("http://localhost/abcd/efgh/") must_==
        HttpRequest("http","localhost",None,Some("abcd/efgh/"))
    }
    
    "Parse non trailing slash dir path correctly" in {
      parse("http://localhost:8080/abcd/efgh") must_==
        HttpRequest("http","localhost",Some(8080),Some("abcd/efgh"))
    }
    "Parse non path URI correctly" in {
      parse("http://localhost:8080/") must_==
        HttpRequest("http","localhost",Some(8080),None)
    }
    "Parse non path non trailing slash URI correctly" in {
      parse("http://localhost:8080") must_==
        HttpRequest("http","localhost",Some(8080),None)
    }
    "Parse non path non port URI correctly" in {
      parse("http://localhost") must_==
        HttpRequest("http","localhost",None,None)
    }
  }
}
