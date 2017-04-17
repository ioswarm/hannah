package ioswarm.hannah

import scala.util.matching.Regex

/**
  * Created by andreas on 08.04.17.
  */
package object net {

  final val URI_REGEX: Regex = """^(([^:/?#]+):)?(//(((([^:/?#]+)(:([^/?#]+))?)@)?(([^:/?#]*)(:([0-9]+))?)))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

  implicit def stringUriSupport(s: String): StringUriSupport = new StringUriSupport(s)

}

package net {

  private[net] class StringUriSupport(str: String) {
    def scheme: URI = Scheme(str)
    def s: URI = scheme
    def uri: URI = URI(str)
    def u: URI = uri
  }

}
