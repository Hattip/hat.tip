package in.nene.hattip {

	import org.eclipse.jetty.client.ContentExchange
	import org.eclipse.jetty.client.HttpClient
	import org.eclipse.jetty.http.HttpFields
	import org.eclipse.jetty.io.ByteArrayBuffer
	
	class HttpResponse(val code: Int, fields: HttpFields, val str: String) {
	  override def toString = "Response[" + code + "]" + str
	  def apply(f: HttpResponse => Unit) = {
		if (code == 200) {
		  f(this)
		}
		this
	  }
	  def asXml() = scala.xml.XML.loadString(str)
	  def asAnti() = com.codecommit.antixml.XML.fromString(str)
	  def orElse(f: HttpResponse => Unit) = f(this)
	  def onErrorCode(f: PartialFunction[Int,Unit]) = f(code)
	}
	
	class HttpEndpoint(str: String, mode: Int = 0) {
	  val httpClient = new HttpClient()
	  httpClient.start 
	  httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL
	  
	  if (! str.startsWith("http://") && ! str.startsWith("https://"))
	  {
	    throw new RuntimeException("Invalid Http Endpoint String " + str)
	  }
	  
	  def /(additional: String) = 
	    if (mode == 0) 
	      new HttpEndpoint(str + "/" + additional,0) 
	    else
	      throw new RuntimeException("Looking for query parameters not URI path")
	  
	  def ?(t: Tuple2[String,String]) =  
	    // TODO: Instead of mode the class could be split into endpoint and another one 
	    //       which takes over after the ? method
	    if(mode == 0)
	      new HttpEndpoint(str + "?" + t._1 + "=" + t._2,1)
	    else 
	      throw new RuntimeException("Already in query parameter mode")
	  
	  def ?(elems: Tuple2[String,String]*) =  
	    if(mode == 0){
	      val res = elems map { e => e._1 + "=" + e._2 } mkString "&"
	      new HttpEndpoint(str + "?" + res,1)
	    }
	    else 
	      throw new RuntimeException("Already in query parameter mode")
	  
	  
	  def get : HttpResponse = {
	    val exchange = new ContentExchange()
	    exchange.setURL(str)
	    httpClient.send(exchange)
	    exchange.waitForDone
	    new HttpResponse(
		    exchange.getResponseStatus(),
		    exchange.getResponseFields(),
		    exchange.getResponseContent())
	  }
	  
	  def post(data: String): HttpResponse = {
	    val exchange = new ContentExchange()
	    exchange.setMethod("POST")
	    exchange.setURL(str)
	    exchange.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8"); 
	    exchange.setRequestContent(new ByteArrayBuffer(data.getBytes()))
	    httpClient.send(exchange)
	    exchange.waitForDone
	    new HttpResponse(
		    exchange.getResponseStatus(),
		    exchange.getResponseFields(),
		    exchange.getResponseContent())
	  }
	  
	}
	
	object Hattip {
	  implicit def str2HttpEndpoint(str: String) = new HttpEndpoint(str)
	}
}