package ioswarm.hannah.http

import io.netty.handler.codec.http.{HttpVersion => NettyHttpVersion}

/**
  * Created by andreas on 15.04.17.
  */
sealed abstract class HttpVersion (val name: String, val httpVersion: NettyHttpVersion)

case object HTTP_1_0 extends HttpVersion("HTTP/1.0", NettyHttpVersion.HTTP_1_0)
case object HTTP_1_1 extends HttpVersion("HTTP/1.1", NettyHttpVersion.HTTP_1_1)
