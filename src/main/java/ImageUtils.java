package com.jamesrthompson.Data;

import javafx.scene.image.Image;

import javax.imageio.ImageIO;
import java.awt.image.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Image handling utilities
 * @author James R. Thompson, D.Phil
 * @since Jun 5, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

public class ImageUtils {

    public int min, max;
    
    public ImageUtils() {}
    
    public byte[] convertIntArrayToByteArray(int[] input) {
        byte[] output = new byte[input.length];
        for(int i = 0; i < input.length; i++) {
            output[i] = (byte)input[i];
        }
        return output;
    }

    public Image getJavaFXImage(byte[] rawPixels, int width, int height) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            ImageIO.write((RenderedImage) createBufferedImage(rawPixels, width, height), "png", out);
            out.flush();
        } catch (IOException ex) {
            Logger.getLogger(ImageUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        return new Image(in);
    }

    public Image getJavaFXImage(short[] rawPixels, int width, int height) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        byte[] thePixels = create8BitImage(rawPixels, width, height);
        try {
            ImageIO.write((RenderedImage) createBufferedImage(thePixels, width, height), "png", out);
            out.flush();
        } catch (IOException ex) {
            Logger.getLogger(ImageUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        return new Image(in);
    }

    // create 8-bit image by linearly scaling from 16-bits to 8-bits
    byte[] create8BitImage(short[] pixels, int width, int height) {
        int size = width*height;
        byte[] pixels8 = new byte[size];
        int value;
        findMinAndMax(pixels, width, height);
        int min2=min, max2=max;
        double scale = 256.0/(max2-min2+1);
        for (int i=0; i<size; i++) {
            value = (pixels[i]&0xffff)-min2;
            if (value<0) value = 0;
            value = (int)(value*scale+0.5);
            if (value>255) value = 255;
            pixels8[i] = (byte)value;
        }
        return pixels8;
    }

    public void findMinAndMax(short[] pixels, int width, int height) {
        int size = width*height;
        int value;
        min = 65535;
        max = 0;
        for (int i=0; i<size; i++) {
            value = pixels[i]&0xffff;
            if (value<min)
                min = value;
            if (value>max)
                max = value;
        }
    }

    private BufferedImage createBufferedImage(byte[] pixels, int width, int height) {
        SampleModel sm = getIndexSampleModel(width, height);
        DataBuffer db = new DataBufferByte(pixels, width*height, 0);
        WritableRaster raster = Raster.createWritableRaster(sm, db, null);
        IndexColorModel cm = getDefaultColorModel();
        BufferedImage image = new BufferedImage(cm, raster, false, null);
        return image;
    }

    private SampleModel getIndexSampleModel(int width, int height) {
        IndexColorModel icm = getDefaultColorModel();
        WritableRaster wr = icm.createCompatibleWritableRaster(1, 1);
        SampleModel sampleModel = wr.getSampleModel();
        sampleModel = sampleModel.createCompatibleSampleModel(width, height);
        return sampleModel;
     }

     private IndexColorModel getDefaultColorModel() {
        byte[] r = new byte[256];
        byte[] g = new byte[256];
        byte[] b = new byte[256];
        for(int i=0; i<256; i++) {
                r[i]=(byte)i;
                g[i]=(byte)i;
                b[i]=(byte)i;
        }
        IndexColorModel defaultColorModel = new IndexColorModel(8, 256, r, g, b);
        return defaultColorModel;
     }
    
}
