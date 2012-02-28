package in.nene.hattip {
import java.nio.charset.Charset
import scala.collection.mutable
import scala.xml.XML
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.http.HttpFields
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocketClient
import java.nio.CharBuffer
import org.eclipse.jetty.io.ByteArrayBuffer

trait TextHandler { def onMessage(data: String): Unit }
trait BinaryHandler { def onMessage(data: Array[Byte], offset: Int, length: Int): Unit }

class WebsocketHandler() extends WebSocket with WebSocket.OnTextMessage with WebSocket.OnBinaryMessage {
  var textHandlers = mutable.Map[String,TextHandler]()
  var binaryHandlers = mutable.Map[String,BinaryHandler]()
  
  override def onOpen(connection: Connection): Unit = {
    
  }
  override def onClose(code: Int, message: String): Unit = {
    
  }
  override def onMessage(data: String): Unit = {
    
  }
  override def onMessage(data: Array[Byte], offset: Int, length: Int) = {
    
  }
  
  def addTextHandler(t: TextHandler) {
  }
}

class WebsocketConnection() {
	def !(msg: String) = {}
	def receiveText(f: String => Unit) = {}
	def receiveBinary(f: (Array[Byte], Int, Int) => Unit) = {}
}

class HttpResponse(val code: Int, fields: HttpFields, val str: String) {
  val c = new WebSocketClient()
  override def toString = "Response[" + code + "]" + str
  def apply(f: HttpResponse => Unit) = {
	if (code == 200) {
	  f(this)
	}
	this
  }
  def asXml() = XML.loadString(str)
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
  
  def open = new WebsocketConnection()
}

object Hattip {
  implicit def str2HttpEndpoint(str: String) = new HttpEndpoint(str)
  
//  def main(args: Array[String]): Unit = {
//    "http://localhost:8888" / "launch/profile" / "chip" / "0.0.1-SNAPSHOT" get { resp =>
//      println(resp)
//    }
//    
//    val xmlResponse = ("http://localhost:8888" / "launch/profile" / "chip" / "0.0.1-SNAPSHOT" get) asXml;
//    xmlResponse\"file" foreach { file =>
//	    println(file.attribute("name"))
//    };
//    
//    "http://localhost:8888" / "launch/profile" / "chip" / "0.0.1-SNAPSHOT-NOTFOUND" get { resp =>
//      println("I shouldn't be here")
//    } orElse { resp =>
//      println("I should be here " + resp.code)
//    }
//
//    "http://localhost:8888" / "launch/profile" / "chip" / "0.0.1-SNAPSHOT-NOTFOUND" get { resp =>
//      println("I shouldn't be here")
//    } onErrorCode { 
//       case 404 => println("Got the right code")
//       case _ => println("Something went wrong")
//    }
//
//    (("http://localhost:8888" / "foo") ? ("bar" -> "baz") get) onErrorCode {
//       case 404 => println("Got the right code")
//       case _ => println("Something went wrong")
//    }
//    (("http://localhost:8888" / "foo") ? ("bar" -> "baz", "greetings" -> "martians", "hello" -> "world") get) onErrorCode {
//       case 404 => println("Got the right code")
//       case _ => println("Something went wrong")
//    }
//  }
}
}