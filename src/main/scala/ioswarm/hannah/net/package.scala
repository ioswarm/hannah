package ioswarm.hannah

import scala.util.matching.Regex

/**
  * Created by andreas on 08.04.17.
  */
package object net {

  final val URI_REGEX: Regex = """^(([^:/?#]+):)?(//(((([^:/?#]+)(:([^/?#]+))?)@)?(([^:/?#]*)(:([0-9]+))?)))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

}
