package de.ioswarm.hannah.http

import java.nio.ByteBuffer

import io.netty.handler.codec.http
import io.netty.handler.codec.http.FullHttpResponse
import HttpStatus._
/**
  * Created by andreas on 16.04.17.
  */
object HttpResponse {
  def apply(status: HttpStatus
           , version: HttpVersion
           , content: ByteBuffer
           , headers: (String, String)*): HttpResponse = new HttpResponse(status, version, content, headers.toList.groupBy(_._1).map{case (k,v) => (k,v.map(_._2))})
  def apply(status: HttpStatus, content: ByteBuffer, headers: (String, String)*): HttpResponse = apply(status, HTTP_1_1, content, headers:_*)
  def apply(content: ByteBuffer, headers: (String, String)*): HttpResponse = apply(HttpStatus.OK, HTTP_1_1, content, headers:_*)
  def apply(content: ByteBuffer): HttpResponse = apply(HttpStatus.OK, HTTP_1_1, content)
  def apply(headers: (String, String)*): HttpResponse = apply(HttpStatus.OK, HTTP_1_1, ByteBuffer.allocate(0), headers:_*)
  def apply(): HttpResponse = new HttpResponse(HttpStatus.OK, HTTP_1_1, ByteBuffer.allocate(0), Map.empty[String, List[String]])

  def apply(resp: http.HttpResponse): HttpResponse = {
    import scala.collection.JavaConverters._

    new HttpResponse(status = HttpStatus(resp.status().code())
      , if (resp.protocolVersion().minorVersion() == 0) HTTP_1_0 else HTTP_1_1
      , resp match {
        case full: FullHttpResponse => full.content().nioBuffer()
        case _ => ByteBuffer.allocate(0)
      }
      , resp.headers().asScala.groupBy(e => e.getKey).map { case (k,v) => (k,v.map(e => e.getValue).toList)})
  }
}
case class HttpResponse(status: HttpStatus, version: HttpVersion, content: ByteBuffer, headers: Map[String, List[String]]) {

  def apply(name: String): Option[String] = name match {
    case "statusCode" => Option(status.id.toString)
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

  def setHeader(key: String, values: String*): HttpResponse = copy(headers = headers + (key -> values.toList))
  def setHeader(t: (String, String)): HttpResponse = setHeader(t._1, t._2)
  def addHeader(key: String, values: String*): HttpResponse = copy(headers = headers + (key -> headers.getOrElse(key, List.empty[String]).++(values.toList)))
  def addHeader(t: (String, String)): HttpResponse = addHeader(t._1, t._2)

  def +(key: String, values: String*): HttpResponse = addHeader(key, values:_*)
  def +(t: (String, String)): HttpResponse = addHeader(t)

  def status(status: HttpStatus): HttpResponse = copy(status = status)
  def version(version: HttpVersion): HttpResponse = copy(version = version)
  def content(content: ByteBuffer): HttpResponse = copy(content = content)

}
