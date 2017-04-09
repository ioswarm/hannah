package ioswarm.hannah

import ioswarm.hannah.net._

/**
  * Created by andreas on 07.04.17.
  */
object URITest extends App {

//  val uriregex = """^(([^:/?#]+):)?(//((([^/?#]*)@)?([^/?#]*)?))?([^?#]*)(\?([^#]*))?(#(.*))?""".r
  val uriregex = """^(([^:/?#]+):)?(//(((([^:/?#]+)(:([^/?#]+))?)@)?(([^:/?#]*)(:([0-9]+))?)))?([^?#]*)(\?([^#]*))?(#(.*))?""".r
//  val uriregex = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

  val url = "http://apape:4711@manrental.eu:8080/api/schmitz/vehicle/35005?pretty#marker1"
  url match {
    case uriregex(schemeArea, scheme, autorityArea, autority, credentialsArea, credentials, user, passwordArea, password, hostArea, host, portArea, port, path, queryArea, query, fragmentArea, fragment) => {
      println(s"schemeArea: $schemeArea")
      println(s"scheme: $scheme")
      println(s"autorityArea: $autorityArea")
      println(s"autority: $autority")
      println(s"credentialsArea: $credentialsArea")
      println(s"credentials: $credentials")
      println(s"user: $user")
      println(s"passwordArea: $passwordArea")
      println(s"password: $password")
      println(s"hostArea: $hostArea")
      println(s"host: $host")
      println(s"portArea: $portArea")
      println(s"port: $port")
      println(s"path: $path")
      println(s"queryArea: $queryArea")
      println(s"query: $query")
      println(s"fragmentArea: $fragmentArea")
      println(s"fragment: $fragment")
    }
  }

  println

  val t = "test" -> false
  val uri = URI(url)
  println(uri scheme "https" appendPath "commit" appendQuery t)

  val scheme = Scheme("http")
  println(scheme)
  val host = Host("10.102.9.54")
  println(host scheme "ftp")
}
