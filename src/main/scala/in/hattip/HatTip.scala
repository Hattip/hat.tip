package in.hattip

import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.http.HttpFields
import org.eclipse.jetty.io.ByteArrayBuffer
import Function.tupled

import xml.XML
import com.codecommit.antixml

class HttpResponse(val code: Int, fields: HttpFields, val str: String) {
  override def toString = "Response[" + code + "]" + str

  def apply(f: HttpResponse => Unit) = {
    if (code == 200) {
      f(this)
    }
    this
  }

  def asXml = xml.XML.loadString(str)
  def asAntiXml = antixml.XML.fromString(str)
  def orElse(f: HttpResponse => Unit) = f(this)
  def onErrorCode(f: PartialFunction[Int, Unit]) = f(code)
}

abstract class HttpEndpoint protected(str: String) {
  val httpClient = new HttpClient()
  httpClient.start
  httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL

  if(!Set("http://", "https://").exists(str.startsWith)) {
    throw new RuntimeException("Invalid Http Endpoint String " + str)
  }
    
  def get: HttpResponse = {
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

  def ?(elements: (String, String)*) = {
    val res = elements map tupled(_ + "=" + _) mkString "&"
    new HttpEndpointInQueryParamMode(str + "?" + res)
  }
}

object HttpEndpoint {
  def apply(str: String) = new HttpEndpointInUriMode(str)
}

class HttpEndpointInUriMode(str: String) extends HttpEndpoint(str) {
  def /(additional: String) = new HttpEndpointInUriMode(str + "/" + additional)
}

class HttpEndpointInQueryParamMode(str: String) extends HttpEndpoint(str)

object Hattip {
  implicit def str2HttpEndpoint(str: String) = HttpEndpoint(str)
}