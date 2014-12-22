package io.atlassian.aws.s3

import java.io.InputStream

import scala.annotation.tailrec
import scalaz.concurrent.Task

/**
 * Useful input stream utils.
 */
object InputStreams {
  sealed trait ReadBytes
  object ReadBytes {
    case object End extends ReadBytes
    case class Chunk(read: Int) extends ReadBytes
  }

  /**
   * Fill the given buffer with data from the given inputstream. Blocks until the input stream has enough data to fill
   * the buffer or is closed.
   *
   * @param is The input stream
   * @param buffer the buffer to fill
   * @return state of input stream and the bytes read
   */
  def readFully(is: InputStream, buffer: Array[Byte]): Task[ReadBytes] =
    Task.delay {
      @tailrec
      def go(start: Int, length: Int, total: Int): ReadBytes =
        is.read(buffer, start, length) match {
          case -1 if total == 0      => ReadBytes.End
          case -1                    => ReadBytes.Chunk(total)
          case read if read < length => go(start + read, length - read, total + read)
          case read                  => ReadBytes.Chunk(total + read)
        }

      go(0, buffer.size, 0)
    }

}
