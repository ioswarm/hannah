package ioswarm.hannah.http

import java.nio.ByteBuffer

import io.netty.bootstrap.Bootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInitializer, SimpleChannelInboundHandler}
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http._
import ioswarm.hannah.net.URI

import scala.concurrent.{Future, Promise}

/**
  * Created by andreas on 17.04.17.
  */
object HttpClient {
  def apply(uri: URI) = new HttpClient(uri)
  def apply(host: String = "localhost", port: Int = 80, path: String = "/", ssl: Boolean = false): HttpClient = new HttpClient(URI(if (ssl) "https" else "http", host, port, path))

  def call(request: HttpRequest): Future[HttpResponse] = {
    val cli = new HttpClient(request.uri)
    try {
      cli.call(request)
    } finally {
      cli.close()
    }
  }

  def get(request: HttpRequest): Future[HttpResponse] = call(request method GET)

  def post(request: HttpRequest): Future[HttpResponse] = call(request method POST)

  def put(request: HttpRequest): Future[HttpResponse] = call(request method PUT)

  def delete(request: HttpRequest): Future[HttpResponse] = call(request method DELETE)
}
class HttpClient(val uri: URI) {

  val group = new NioEventLoopGroup()

  def host: String = uri.host.getOrElse("localhost")
  def port: Int = uri.port.getOrElse(80)
  def ssl: Boolean = uri.scheme.getOrElse("http").equalsIgnoreCase("https")

  protected def createChannel(p: Promise[HttpResponse]): Channel = new Bootstrap()
    .group(group)
    .channel(classOf[NioSocketChannel])
    .handler(new CallbackInitializer(ssl, p))
    .connect(host, port)
      .sync()
        .channel()

  def createRequest(method: HttpMethod = GET, content: ByteBuffer = ByteBuffer.allocate(0)): HttpRequest = HttpRequest(uri, method, content)

  def call(request: HttpRequest): Future[HttpResponse] = {
    val p = Promise[HttpResponse]()
    val ch = createChannel(p)
    try {
      val req: FullHttpRequest = request.toNetty
      req.headers()
        .set(HttpHeaderNames.HOST, request.host)
        .set(HttpHeaderNames.CONNECTION, HttpHeaderValues.CLOSE)
        .set(HttpHeaderNames.ACCEPT_ENCODING, HttpHeaderValues.GZIP)

      ch.writeAndFlush(req)
      p.future
    } finally {
      ch.closeFuture().sync()
    }
  }

  def call(method: HttpMethod, path: String, content: ByteBuffer = ByteBuffer.allocate(0)): Future[HttpResponse] = call(createRequest(method, content).uri(uri))

  def get(request: HttpRequest): Future[HttpResponse] = call(request method GET)
  def get(path: String = uri.completePath, content: ByteBuffer = ByteBuffer.allocate(0)): Future[HttpResponse] = call(GET, path, content)

  def post(request: HttpRequest): Future[HttpResponse] = call(request method POST)
  def post(path: String = uri.completePath, content: ByteBuffer = ByteBuffer.allocate(0)): Future[HttpResponse] = call(POST, path, content)

  def put(request: HttpRequest): Future[HttpResponse] = call(request method PUT)
  def put(path: String = uri.completePath, content: ByteBuffer = ByteBuffer.allocate(0)): Future[HttpResponse] = call(PUT, path, content)

  def delete(request: HttpRequest): Future[HttpResponse] = call(request method DELETE)
  def delete(path: String = uri.completePath, content: ByteBuffer = ByteBuffer.allocate(0)): Future[HttpResponse] = call(DELETE, path, content)

  def close(): Unit = group.shutdownGracefully().sync()
}

private[http] class CallbackInitializer(ssl: Boolean, p: Promise[HttpResponse]) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(new HttpClientCodec())
      .addLast(new HttpContentDecompressor())
      .addLast(new HttpObjectAggregator(1048576))
      .addLast(new SimpleChannelInboundHandler[HttpObject]() {
        override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = {
          msg match {
            case resp: FullHttpResponse =>
              p success HttpResponse(resp)
              ctx.close()
          }
        }

        override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
          p failure cause
          ctx.close()
        }
      })
  }
}
