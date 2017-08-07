package de.ioswarm.hannah.net

/**
  * Created by andreas on 08.04.17.
  */
object Scheme {
  def apply(scheme: String): URI = new URI(Option(scheme))
  def apply(scheme: String, host: String): URI = new URI(scheme = Option(scheme), host = Option(host))
  def apply(scheme: String, host: String, port: Int): URI = new URI(scheme = Option(scheme), host = Option(host), port = Option(port))
}
object Host {
  def apply(host: String): URI = new URI(host = Option(host))
  def apply(host: String, port: Int): URI = new URI(host = Option(host), port = Option(port))
}
object URI {
  def apply(uri: String): URI = uri match {
    case URI_REGEX(
      _ // schemeArea
      , scheme
      , _ // autorityArea
      , _ // autority
      , _ // credentialsArea
      , _ // credentials
      , user
      , _ // passwordArea
      , password
      , _ // hostArea
      , host
      , _ // portArea
      , port
      , path
      , _ // queryArea
      , query
      , _ // fragmentArea
      , fragment
    ) => new URI(
      Option(scheme)
      , Option(user)
      , Option(password)
      , Option(host)
      , try {
        Some(port.toInt)
      } catch {
        case e: Exception => None
      }
      , Option(path)
      , Option(query)
      , Option(fragment)
    )
  }

  def apply(scheme: String, host: String): URI = new URI(scheme = Option(scheme), host = Option(host))
  def apply(scheme: String, host: String, path: String): URI = new URI(scheme = Option(scheme), host = Option(host), path = Option(path))
  def apply(scheme: String, host: String, path: String, query: String): URI = new URI(scheme = Option(scheme), host = Option(host), path = Option(path), query = Option(query))
  def apply(scheme: String, host: String, port: Int): URI = new URI(scheme = Option(scheme), host = Option(host), port = Option(port))
  def apply(scheme: String, host: String, port: Int, path: String): URI = new URI(scheme = Option(scheme), host = Option(host), port = Option(port), path = Option(path))
  def apply(scheme: String, host: String, port: Int, path: String, query: String): URI = new URI(scheme = Option(scheme), host = Option(host), port = Option(port), path = Option(path), query = Option(query))

  def apply(): URI = new URI()
}
case class URI(scheme: Option[String] = None, user: Option[String] = None, password: Option[String] = None, host: Option[String] = None, port: Option[Int] = None, path: Option[String] = None, query: Option[String] = None, fragment: Option[String] = None) {

  private def portArea = port match {
    case Some(v) => ":"+v
    case None => ""
  }

  private def hostArea = host match {
    case Some(v) => v+portArea
    case None => ""
  }

  private def passwordArea = password match {
    case Some(v) => ":"+v
    case None => ""
  }

  def credentials: Option[String] = user match {
    case Some(v) => Option(v+passwordArea)
    case None => None
  }

  private def credentialsArea = credentials match {
    case Some(v) => v+"@"
    case None => ""
  }

  def authority: Option[String] = {
    val auth = credentialsArea+hostArea
    if (auth.isEmpty) None else Option(auth)
  }

  private def authorityArea = authority match {
    case Some(v) => "//"+v
    case None => ""
  }

  private def pathArea = path match {
    case Some(v) => if (v.startsWith("/")) v else "/"+v
    case None => if (!authorityArea.isEmpty) "/" else ""
  }

  private def queryArea = query match {
    case Some(v) => "?"+v
    case None => ""
  }

  private def fragmentArea = fragment match {
    case Some(v) => "#"+v
    case None => ""
  }

  private def schemeArea = scheme match {
    case Some(v) => v+":"
    case None => ""
  }

  def completePath: String = pathArea+queryArea+fragmentArea
  def uri: String = schemeArea+authorityArea+completePath

  override def toString: String = uri

  def scheme(scheme: String): URI = copy(scheme = Option(scheme))
  def user(user: String): URI = copy(user = Option(user))
  def password(password: String): URI = copy(password = Option(password))
  def host(host: String): URI = copy(host = Option(host))

  def port(port: Int): URI = copy(port = Option(port))
  def ::(port: Int): URI = this.port(port)
  def path(path: String): URI = copy(path = Option(path))
  def appendPath(segment: String): URI = {
    this.path((path match {
      case Some(v) => v
      case None => ""
    }) + (if (!segment.startsWith("/")) "/" else "")+segment)
  }
  def +/(segment: String): URI = appendPath(segment)
  //def /(segment: String): URI = appendPath(segment)
  def query(query: String): URI = copy(query = Option(query))
  def appendQuery(segment: String): URI = {
    require(segment.length > 0)
    val seg = if (segment.startsWith("&") || segment.startsWith("?")) segment.substring(1) else segment
    query(query match {
      case Some(v) => v + "&" + seg
      case None => seg
    })
  }
  def appendQuery[K,V](tuple: (K, V)): URI = appendQuery(tuple._1+"="+tuple._2)
  def +?(segment: String): URI = appendQuery(segment)
  def +?[K,V](tuple: (K, V)): URI = appendQuery(tuple)
  def fragment(fragment: String): URI = copy(fragment = Option(fragment))
  def +#(fragment: String): URI = this.fragment(fragment)

}
