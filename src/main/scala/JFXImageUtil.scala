package com.jamesrthompson.Data

import javafx.scene.image.Image
import javax.imageio.ImageIO
import java.awt.image._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.util.logging.{Level, Logger}

/**
 * Scala / JavaFX Image handling utilities
 * @author James R. Thompson, D.Phil
 * @since Aug 14, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

object JFXImageUtil {

	def getJavaFXImage(rawPixels:Array[Byte], width:Int, height:Int) = {
		val out = new ByteArrayOutputStream
		try {
         ImageIO.write(createBufferedImage(rawPixels, width, height).asInstanceOf[RenderedImage], "png", out)
         out.flush
      } catch {
			case ex : Exception => Logger.getLogger("JavaFX Image Handling Exception!").log(Level.SEVERE, null, ex)
		}
       new Image(new ByteArrayInputStream(out.toByteArray))
	}

 	private def createBufferedImage(pixels:Array[Byte], width:Int, height:Int) = {
 		def getDefaultColorModel = {
			val r  = new Array[Byte](256)
			val g  = new Array[Byte](256)
			val b  = new Array[Byte](256)
			for(i <- 0 until 256) {
				r(i) = i.toByte
				g(i) = i.toByte
				b(i) = i.toByte
	 		}
	 		new IndexColorModel(8, 256, r, g, b) 
 		}
	 	def getIndexSampleModel(width:Int, height:Int) = {
	 		val icm = getDefaultColorModel
	 		val wr = icm.createCompatibleWritableRaster(1, 1)
	 		wr.getSampleModel.createCompatibleSampleModel(width, height)
	 	}
 		val sm = getIndexSampleModel(width, height)
 		val db = new DataBufferByte(pixels, width * height, 0)
 		val raster = Raster.createWritableRaster(sm, db, null)
 		new BufferedImage(getDefaultColorModel, raster, false, null)
 	}
}