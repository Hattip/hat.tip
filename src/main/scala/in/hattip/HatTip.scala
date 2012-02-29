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

  // FIX: Something seems wrong here.
  def apply(f: HttpResponse => Unit) = {
    if (code == 200) {
      f(this)
    }
    this
  }

  def asXml = XML.loadString(str)
  def asAntiXml = antixml.XML.fromString(str)
  def orElse(f: HttpResponse => Unit) = f(this)
  def onErrorCode(f: PartialFunction[Int, Unit]) = f(code)
}

// Phantom types to ensure proper creation of HttpEndpoint.
sealed trait HttpEndpointConstructionStage
trait UriStage extends HttpEndpointConstructionStage
trait QueryParamStage extends HttpEndpointConstructionStage

trait HttpEndpoint { outer =>

  type S <: HttpEndpointConstructionStage

  // Initialization code
  val httpClient = new HttpClient
  httpClient.start
  httpClient setConnectorType HttpClient.CONNECTOR_SELECT_CHANNEL

  require(
    Set("http://", "https://").exists(str.startsWith),
    "Invalid Http Endpoint String " + str
  )

  def str: String

  def get: HttpResponse = {
    val ex = new ContentExchange
    ex.setURL(str)
    httpClient.send(ex)
    ex.waitForDone
    new HttpResponse(ex.getResponseStatus, ex.getResponseFields, ex.getResponseContent)
  }

  def post(data: String): HttpResponse = {
    val ex = new ContentExchange
    ex.setMethod("POST")
    ex.setURL(str)
    ex.setRequestContentType("application/x-www-form-urlencoded;charset=utf-8");
    ex.setRequestContent(new ByteArrayBuffer(data.getBytes))
    httpClient.send(ex)
    ex.waitForDone
    new HttpResponse(ex.getResponseStatus, ex.getResponseFields, ex.getResponseContent)
  }

  def /(additional: String)(implicit ev: S =:= UriStage) = HttpEndpoint(str + "/" + additional)

  def ?(elements: (String, String)*) = {
    val res = elements map tupled(_ + "=" + _) mkString "&"
    new HttpEndpoint {
      type S = QueryParamStage
      def str = outer.str + "?" + res
    }
  }
}

object HttpEndpoint {
  def apply(s: String) = new HttpEndpoint {
    type S = UriStage
    def str = s
  }
}

object Hattip {
  implicit def str2HttpEndpoint(str: String) = HttpEndpoint(str)
}