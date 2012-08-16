package com.jamesrthompson.Data

object BitBuffer {

  def getBits(byteBuffer:Array[Byte], bitsToR:Int) : Int = {
    lazy val bmask : Array[Int] = Array[Int](0x0000, 0x0001, 0x0003, 0x0007, 0x000F, 0x001F, 0x003F, 0x007F)
    lazy val fMask : Array[Int] = Array[Int](0x0000, 0x0080, 0x00C0, 0x00E0, 0x00F0, 0x00F8, 0x00FC, 0x00FE)
    var eof = false
    var currByte : Int = 0
    var currBit : Int = 0
    var bitsToRead = bitsToR
    val eofByte = byteBuffer.length
    var store = 0
    while (bitsToRead != 0 && eof == false) {
      if (bitsToRead >= 8 - currBit) {
        if (currBit == 0) { // special
          store = store << 8
          val cb = byteBuffer(currByte).toInt
          cb < 0 match {
            case true => store += 256 + cb
            case false => store += cb
          }
          bitsToRead -= 8
          currByte += 1
        } else {
          store = store << (8 - currBit)
          store += byteBuffer(currByte).toInt & bmask(8 - currBit)
          bitsToRead -= (8 - currBit)
          currBit = 0
          currByte += 1
        }
      } else {
        store = store << bitsToRead
        var cb = 0
        byteBuffer(currByte) < 0 match {
          case true => cb = 256 + cb
          case false => cb
        }
        store += ((cb) & (0x00FF - fMask(currBit)) >> (8 - (currBit + bitsToRead)))
        currBit += bitsToRead
        bitsToRead = 0
      }
        if (currByte == eofByte) {
          eof = true
          store
        }
      }
    store
  }

}