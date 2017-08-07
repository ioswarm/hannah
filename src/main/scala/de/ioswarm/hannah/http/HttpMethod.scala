package de.ioswarm.hannah.http

import io.netty.handler.codec.http.{HttpMethod => NettyHttpMethod}

/**
  * Created by andreas on 15.04.17.
  */
sealed abstract class HttpMethod(val name: String, val method: NettyHttpMethod)

case object GET extends HttpMethod("GET", NettyHttpMethod.GET)
case object POST extends HttpMethod("POST", NettyHttpMethod.POST)
case object PUT extends HttpMethod("PUT", NettyHttpMethod.PUT)
case object DELETE extends HttpMethod("DELETE", NettyHttpMethod.DELETE)
