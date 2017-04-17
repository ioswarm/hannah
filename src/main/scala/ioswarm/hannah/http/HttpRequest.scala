package ioswarm.hannah.http

import java.nio.ByteBuffer

import io.netty.handler.codec.http
import io.netty.handler.codec.http.DefaultFullHttpRequest
import ioswarm.hannah.net.URI

/**
  * Created by andreas on 17.04.17.
  */
object HttpRequest {
  def apply(uri: URI, method: HttpMethod, version: HttpVersion, content: ByteBuffer, headers: (String, String)*) =
    new HttpRequest(uri, method, version, content, headers.toList.groupBy(_._1).map{case (k,v) => (k,v.map(_._2))})
  def apply(uri: URI, method: HttpMethod, content: ByteBuffer, headers: (String, String)*): HttpRequest = apply(uri,method,HTTP_1_1,content, headers:_*)
  def apply(uri: URI, headers: (String, String)*): HttpRequest = apply(uri, GET, HTTP_1_1, ByteBuffer.allocate(0), headers:_*)

  def apply(uri: String, method: HttpMethod, version: HttpVersion, content: ByteBuffer, headers: (String, String)*): HttpRequest =
    apply(URI(uri), method, version, content, headers:_*)
  def apply(uri: String, method: HttpMethod, content: ByteBuffer, headers: (String, String)*): HttpRequest = apply(URI(uri), method, content, headers:_*)
  def apply(uri: String, headers: (String, String)*): HttpRequest = apply(URI(uri), headers:_*)

  def apply(host: String = "localhost", port: Int = 80, path: String = "/", ssl: Boolean = false, method: HttpMethod = GET, version: HttpVersion = HTTP_1_1, content: ByteBuffer = ByteBuffer.allocate(0)): HttpRequest =
    apply(URI(if (ssl) "https" else "http", host, if (port == 80 && ssl) 443 else port, path), method, version, content)

}
case class HttpRequest(uri: URI, method: HttpMethod, version: HttpVersion, content: ByteBuffer, headers: Map[String, List[String]]) {

  def host: String = uri.host.getOrElse("localhost")
  def port: Int = uri.port.getOrElse(80)
  def path: String = uri.completePath

  def ssl: Boolean = uri.scheme.getOrElse("http").equalsIgnoreCase("https")

  def apply(name: String): Option[String] = name match {
    case "method" => Option(method.name)
    case "httpVersion" => Option(version.name)
    case "content" =>
      val buf = new Array[Byte](content.remaining())
      content.get(buf)
      Option(new String(buf, "UTF-8"))
    case key: String =>
      val values = headers.getOrElse(key, List.empty[String])
      if (values.isEmpty) None else Option(values.head)
    case _ => None
  }

  def setHeader(key: String, values: String*): HttpRequest = copy(headers = headers + (key -> values.toList))
  def setHeader(t: (String, String)): HttpRequest = setHeader(t._1, t._2)
  def addHeader(key: String, values: String*): HttpRequest = copy(headers = headers + (key -> headers.getOrElse(key, List.empty[String]).++(values.toList)))
  def addHeader(t: (String, String)): HttpRequest = addHeader(t._1, t._2)

  def +(key: String, values: String*): HttpRequest = addHeader(key, values:_*)
  def +(t: (String, String)): HttpRequest = addHeader(t)

  def uri(uri: URI): HttpRequest = copy(uri = uri)
  def method(method: HttpMethod): HttpRequest = copy(method = method)
  def version(version: HttpVersion): HttpRequest = copy(version = version)
  def content(content: ByteBuffer): HttpRequest = copy(content = content)

  def toNetty: http.FullHttpRequest = {
    val req = new DefaultFullHttpRequest(version.httpVersion, method.method, uri.uri)
    for (t <- headers.map(e => e._2.map(v => (e._1, v))).flatten)
      req.headers().add(t._1, t._2)
    if (content.remaining() > 0) req.content().setBytes(0, content)
    req
  }

}
