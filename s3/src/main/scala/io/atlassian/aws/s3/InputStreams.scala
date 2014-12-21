package io.atlassian.aws.s3

import java.io.InputStream

import scala.annotation.tailrec

/**
 * Useful input stream utils.
 */
object InputStreams {
  sealed trait ReadBytes {
    def read: Int
  }
  object ReadBytes {
    case class End(read: Int) extends ReadBytes
    case class NotEnd(read: Int) extends ReadBytes
  }

  /**
   * Fill the given buffer with data from the given inputstream. Blocks until the input stream has enough data to fill
   * the buffer or is closed. Might throw an IOException depending on InputStream, so make sure you call this within
   * some safe or try/catch
   * @param is The input stream
   * @param buffer the buffer to fill
   * @return state of input stream and the bytes read
   */
  def readFully(is: InputStream, buffer: Array[Byte]): ReadBytes = {
    import ReadBytes._
    @tailrec
    def go(start: Int, length: Int, totalLengthRead: Int): ReadBytes = {
      val read = is.read(buffer, start, length)
      if (read == -1)
        End(totalLengthRead)
      else if (read < length)
        go(start + read, length - read, totalLengthRead + read)
      else
        NotEnd(totalLengthRead + read)
    }
    go(0, buffer.size, 0)
  }

}
