package de.ioswarm.hannah.http

/**
  * Created by andreas on 16.04.17.
  */
object HttpStatus extends Enumeration{
  type HttpStatus = Value

  val CONTINUE = Value(100, "Continue")
  val SWITCHING_PROTOCOLS = Value(101, "Switching Protocols")
  val PROCESSING = Value(102, "Processing")
  val OK = Value(200, "OK")
  val CREATED = Value(201, "Created")
  val ACCEPTED = Value(202, "Accepted")
  val NON_AUTHORITATIVE_INFORMATION = Value(203, "Non-authoritative Information")
  val NO_CONTENT = Value(204, "No Content")
  val RESET_CONTENT = Value(205, "Reset Content")
  val PARTIAL_CONTENT = Value(206, "Partial Content")
  val MULTI_STATUS = Value(207, "Multi-Status")
  val ALREADY_REPORTED = Value(208, "Already Reported")
  val IM_USED = Value(226, "IM Used")
  val MULTIPLE_CHOICES = Value(300, "Multiple Choices")
  val MOVED_PERMANENTLY = Value(301, "Moved Permanently")
  val FOUND = Value(302, "Found")
  val SEE_OTHER = Value(303, "See Other")
  val NOT_MODIFIED = Value(304, "Not Modified")
  val USE_PROXY = Value(305, "Use Proxy")
  val TEMPORARY_REDIRECT = Value(307, "Temporary Redirect")
  val PERMANENT_REDIRECT = Value(308, "Permanent Redirect")
  val BAD_REQUEST = Value(400, "Bad Request")
  val UNAUTHORIZED = Value(401, "Unauthorized")
  val PAYMENT_REQUIRED = Value(402, "Payment Required")
  val FORBIDDEN = Value(403, "Forbidden")
  val NOT_FOUND = Value(404, "Not Found")
  val METHOD_NOT_ALLOWED = Value(405, "Method Not Allowed")
  val NOT_ACCEPTABLE = Value(406, "Not Acceptable")
  val PROXY_AUTHENTICATION_REQUIRED = Value(407, "Proxy Authentication Required")
  val REQUEST_TIMEOUT = Value(408, "Request Timeout")
  val CONFLICT = Value(409, "Conflict")
  val GONE = Value(410, "Gone")
  val LENGTH_REQUIRED = Value(411, "Length Required")
  val PRECONDITION_FAILED = Value(412, "Precondition Failed")
  val PAYLOAD_TOO_LARGE = Value(413, "Payload Too Large")
  val REQUEST_URI_TOO_LONG = Value(414, "Request-URI Too Long")
  val UNSUPPORTED_MEDIA_TYPE = Value(415, "Unsupported Media Type")
  val REQUESTED_RANGE_NOT_STATISFIABLE = Value(416, "Requested Range Not Satisfiable")
  val EXPECTATION_FAILED = Value(417, "Expectation Failed")
  val IM_A_TEAPOT = Value(418, "I'm a teapot")
  val MISDIRECTED_REQUEST = Value(421, "Misdirected Request")
  val UNPROCESSABLE_ENTITY = Value(422, "Unprocessable Entity")
  val LOCKED = Value(423, "Locked")
  val FAILED_DEPENDENCY = Value(424, "Failed Dependency")
  val UPGRADE_REQUIRED = Value(426, "Upgrade Required")
  val PRECONDITION_REQUIRED = Value(428, "Precondition Required")
  val TOO_MANY_REQUESTS = Value(429, "Too Many Requests")
  val REQUEST_HEADER_FIELDS_TOO_LARGE = Value(431, "Request Header Fields Too Large")
  val CONNECTION_CLOSED_WITHOUT_RESPONSE = Value(444, "Connection Closed Without Response")
  val UNAVAILABLE_FOR_LEGAL_REASONS = Value(451, "Unavailable For Legal Reasons")
  val CLIENT_CLOSED_REQUEST = Value(499, "Client Closed Request")
  val INTERNAL_SERVER_ERROR = Value(500, "Internal Server Error")
  val NOT_IMPLEMENTED = Value(501, "Not Implemented")
  val BAD_GATEWAY = Value(502, "Bad Gateway")
  val SERVICE_UNAVAILABLE = Value(503, "Service Unavailable")
  val GATEWAY_TIMEOUT = Value(504, "Gateway Timeout")
  val HTTP_VERSION_NOT_SUPPORTED = Value(505, "HTTP Version Not Supported")
  val VARIANT_ALSO_NEGOTIATES = Value(506, "Variant Also Negotiates")
  val INSUFFICIENT_STORAGE = Value(507, "Insufficient Storage")
  val LOOP_DETECTED = Value(508, "Loop Detected")
  val NOT_EXTENDED = Value(510, "Not Extended")
  val NETWORK_AUTHENTICATION_REQUIRED = Value(511, "Network Authentication Required")
  val NETWORK_CONNECT_TIMEOUT_ERROR = Value(599, "Network Connect Timeout Error")
}

