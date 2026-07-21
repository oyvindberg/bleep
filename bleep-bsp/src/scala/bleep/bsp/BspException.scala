package bleep.bsp

/** A BSP request failed in a way the client should be told about. `code` is a JSON-RPC error code (see `JsonRpcErrorCodes`) and goes straight into the error
  * response.
  */
class BspException(val code: Int, message: String) extends RuntimeException(message)
