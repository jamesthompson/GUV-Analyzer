package com.jamesrthompson.Data;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Canny-Deriche Implementation - Adapted from original ImageJ code
 * Original implementation for ImageJ by - Thomas Boudier - thomas.boudier@snv.jussieu.fr
 * @author James R. Thompson, D.Phil
 * @since Jun 4, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

public class CannyDeriche {
    
    public byte[] rawPixels; 
    private int[] pixels = null;
    private double[] norm_deriche = null;
    private double[] angle_deriche = null;
    private int width;
    private int height;
    private int radius;
    private double alpha;
    private double upper;
    private double lower;
    
    /**
     * Constructor for Canny-Deriche Filter
     * @param input - File object for the 8-bit grayscale image
     * @param radius - radius of median filter
     * @param alpha - alpha value for Deriche algorithm
     * @param upper - upper bounds for pixel values for 'trinarization'
     * @param lower - lower bounds for pixel values for 'trinarization'
     * @throws java.io.IOException - loads file so must throw IO error
     */
    public CannyDeriche(File input, int radius, double alpha, double upper, double lower) throws IOException {
        this.radius = radius;
        this.alpha = alpha;
        this.upper = upper;
        this.lower = lower;
        BufferedImage image = ImageIO.read(input);
        width = image.getWidth();
        height = image.getHeight();
        rawPixels = ((DataBufferByte)image.getRaster().getDataBuffer()).getData();
        int[] loadedPixels = new int[rawPixels.length];
        for(int i = 0; i < rawPixels.length; i++) {
            loadedPixels[i] = rawPixels[i]&0xff;
        }
        pixels = medianFilter(loadedPixels, radius);
    }
    
    public CannyDeriche(byte[] rawPixels, int width, int height, int radius, double alpha, double upper, double lower) {
        this.radius = radius;
        this.alpha = alpha;
        this.upper = upper;
        this.lower = lower;
        this.width = width;
        this.height = height;
        this.rawPixels = rawPixels;
        int[] loadedPixels = new int[rawPixels.length];
        for(int i = 0; i < rawPixels.length; i++) {
            loadedPixels[i] = rawPixels[i]&0xff;
        }
        pixels = medianFilter(loadedPixels, radius);
    }
    
    public CannyDeriche(int[] pixels, int width, int height, int radius, double alpha, double upper, double lower) {
        this.radius = radius;
        this.alpha = alpha;
        this.upper = upper;
        this.lower = lower;
        this.width = width;
        this.height = height;
        this.pixels = medianFilter(pixels, radius);
    }
    
    public int[] retryFilter(int radius, double alpha, double upper, double lower) {
        this.radius = radius;
        this.alpha = alpha;
        this.upper = upper;
        this.lower = lower;
        int[] loadedPixels = new int[rawPixels.length];
        for(int i = 0; i < rawPixels.length; i++) {
            loadedPixels[i] = rawPixels[i]&0xff;
        }
        pixels = medianFilter(loadedPixels, radius);
        return getFilteredImage();
    }
    
    public int[] getFilteredImage() {
        double[] deriched = getDeriche();
	int[] binarized = makeBinary(deriched);
	int[] hysteresisarized = hysteresis(binarized);
        return binarized;
    }
        
    private double[] getDeriche() {
        medianFilter(getPixels(), radius);
        dericheCalc();
        double[] suppressed = nonMaximalSuppression(); // do suppression
        return suppressed;
    }
        
    private int[] makeBinary(double[] input) {
        int[] res = new int[input.length];
        double pix;
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                pix = getPixel(input, x, y);
                if (pix >= upper) {
                    putPixel(res, 255, x, y);
                }
                if (pix <= lower) {
                    putPixel(res, 0, x, y);
                }
            }
        }
        return res;
    }

    private int[] hysteresis(int[] input) {
        boolean change = true;
        while (change) {
            change = false;
            for (int x = 1; x < width - 1; x++) {
                for (int y = 1; y < height - 1; y++) {
                    if (getPixel(input, x, y) == 255) {
                        if (getPixel(input, x + 1, y) == 128) {
                            change = true;
                            putPixel(input, 255, x + 1, y);
                        }
                        if (getPixel(input, x - 1, y) == 128) {
                            change = true;
                            putPixel(input, 255, x - 1, y);
                        }
                        if (getPixel(input, x, y + 1) == 128) {
                            change = true;
                            putPixel(input, 255, x, y + 1);
                        }
                        if (getPixel(input, x, y - 1) == 128) {
                            change = true;
                            putPixel(input, 255, x, y - 1);
                        }
                        if (getPixel(input, x + 1, y + 1) == 128) {
                            change = true;
                            putPixel(input, 255, x + 1, y + 1);
                        }
                        if (getPixel(input, x - 1, y - 1) == 128) {
                            change = true;
                            putPixel(input, 255, x - 1, y - 1);
                        }
                        if (getPixel(input, x - 1, y + 1) == 128) {
                            change = true;
                            putPixel(input, 255, x - 1, y + 1);
                        }
                        if (getPixel(input, x + 1, y - 1) == 128) {
                            change = true;
                            putPixel(input, 255, x + 1, y - 1);
                        }
                    }
                }
            }
            if (change) {
                for (int x = width - 2; x > 0; x--) {
                    for (int y = height - 2; y > 0; y--) {
                        if (getPixel(input, x, y) == 255) {
                            if (getPixel(input, x + 1, y) == 128) {
                                change = true;
                                putPixel(input, 255, x + 1, y);
                            }
                            if (getPixel(input, x - 1, y) == 128) {
                                change = true;
                                putPixel(input, 255, x - 1, y);
                            }
                            if (getPixel(input, x, y + 1) == 128) {
                                change = true;
                                putPixel(input, 255, x, y + 1);
                            }
                            if (getPixel(input, x, y - 1) == 128) {
                                change = true;
                                putPixel(input, 255, x, y - 1);
                            }
                            if (getPixel(input, x + 1, y + 1) == 128) {
                                change = true;
                                putPixel(input, 255, x + 1, y + 1);
                            }
                            if (getPixel(input, x - 1, y - 1) == 128) {
                                change = true;
                                putPixel(input, 255, x - 1, y - 1);
                            }
                            if (getPixel(input, x - 1, y + 1) == 128) {
                                change = true;
                                putPixel(input, 255, x - 1, y + 1);
                            }
                            if (getPixel(input, x + 1, y - 1) == 128) {
                                change = true;
                                putPixel(input, 255, x + 1, y - 1);
                            }
                        }
                    }
                }
            }
        }
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                if (getPixel(input, x, y) == 128) {
                    putPixel(input, 0, x, y);
                }
            }
        }
        return input;
    }

    /**
     * Deriche Algorithm
     */
    public void dericheCalc() {
        
        int rows = height;
        int columns = width;
        int imageSize = height * width;
        int icolumns;
        int row1;
        int row2;
        int row3;
        int col1;
        int col2;
        int col3;
        int icol1;
        int icol2;
        int i;
        int j;
        row1 = rows - 1;
        row2 = rows - 2;
        row3 = rows - 3;
        col1 = columns - 1;
        col2 = columns - 2;
        col3 = columns - 3;
        norm_deriche = new double[imageSize];
        angle_deriche = new double[imageSize];
        double[] nf_grx = new double[imageSize];
        double[] nf_gry = new double[imageSize];
        int[] a1 = new int[imageSize];
        double[] a2 = new double[imageSize];
        double[] a3 = new double[imageSize];
        double[] a4 = new double[imageSize];
        double ad1 = -Math.exp(-alpha);
        double ad2 = 0;
        double an1 = 1;
        double an2 = 0;
        double an3 = Math.exp(-alpha);
        double an4 = 0;
        double an11 = 1;
        /*
         *  FIRST STEP:  Y GRADIENT
         *  x-smoothing
         */
        for (i = 0; i < rows; i++) {
            for (j = 0; j < columns; j++) {
                a1[i * columns + j] = getPixel(getPixels(), j, i);
            }
        }
        for (i = 0; i < rows; ++i) {
            icolumns = i * columns;
            icol1 = icolumns - 1;
            icol2 = icolumns - 2;
            a2[icolumns] = an1 * a1[icolumns];
            a2[icolumns + 1] = an1 * a1[icolumns + 1] +
            an2 * a1[icolumns] - ad1 * a2[icolumns];
            for (j = 2; j < columns; ++j) {
                a2[icolumns + j] = an1 * a1[icolumns + j] + an2 * a1[icol1 + j] -
                ad1 * a2[icol1 + j] - ad2 * a2[icol2 + j];
            }
        }
        for (i = 0; i < rows; ++i) {
            icolumns = i * columns;
            icol1 = icolumns + 1;
            icol2 = icolumns + 2;
            a3[icolumns + col1] = 0;
            a3[icolumns + col2] = an3 * a1[icolumns + col1];
            for (j = col3; j >= 0; --j) {
                a3[icolumns + j] = an3 * a1[icol1 + j] + an4 * a1[icol2 + j] -
                ad1 * a3[icol1 + j] - ad2 * a3[icol2 + j];
            }
        }
        icol1 = rows * columns;
        for (i = 0; i < icol1; ++i) {
            a2[i] += a3[i];
        }
        /*
         *  FIRST STEP Y-GRADIENT : y-derivative
         *  Columns top down
         */
        for (j = 0; j < columns; ++j) {
            a3[j] = 0;
            a3[columns + j] = an11 * a2[j] - ad1 * a3[j];
            for (i = 2; i < rows; ++i) {
                a3[i * columns + j] = an11 * a2[(i - 1) * columns + j] -
                ad1 * a3[(i - 1) * columns + j] - ad2 * a3[(i - 2) * columns + j];
            }
        }
        /*
         *  Columns top down
         */
        for (j = 0; j < columns; ++j) {
            a4[row1 * columns + j] = 0;
            a4[(row2 * columns) + j] = -an11 * a2[row1 * columns + j] -
            ad1 * a4[row1 * columns + j];
            for (i = row3; i >= 0; --i) {
                a4[i * columns + j] = -an11 * a2[(i + 1) * columns + j] -
                ad1 * a4[(i + 1) * columns + j] - ad2 * a4[(i + 2) * columns + j];
            }
        }
        icol1 = columns * rows;
        for (i = 0; i < icol1; ++i) {
            a3[i] += a4[i];
        }

        for (i = 0; i < rows; ++i) {
            for (j = 0; j < columns; ++j) {
                nf_gry[i * columns + j] = a3[i * columns + j];
            }
        }
        /*
         *  SECOND STEP X-GRADIENT
         */
        for (i = 0; i < rows; ++i) {
            for (j = 0; j < columns; ++j) {
                a1[i * columns + j] = getPixel(getPixels(), j, i);
            }
        }
        for (i = 0; i < rows; ++i) {
            icolumns = i * columns;
            icol1 = icolumns - 1;
            icol2 = icolumns - 2;
            a2[icolumns] = 0;
            a2[icolumns + 1] = an11 * a1[icolumns];
            for (j = 2; j < columns; ++j) {
                a2[icolumns + j] = an11 * a1[icol1 + j] - ad1 * a2[icol1 + j] - ad2 * a2[icol2 + j];
            }
        }
        for (i = 0; i < rows; ++i) {
            icolumns = i * columns;
            icol1 = icolumns + 1;
            icol2 = icolumns + 2;
            a3[icolumns + col1] = 0;
            a3[icolumns + col2] = -an11 * a1[icolumns + col1];
            for (j = col3; j >= 0; --j) {
                a3[icolumns + j] = -an11 * a1[icol1 + j] - ad1 * a3[icol1 + j] - ad2 * a3[icol2 + j];
            }
        }
        icol1 = rows * columns;
        for (i = 0; i < icol1; ++i) {
            a2[i] += a3[i];
        }
        /*
         *  On the columns
         *  Columns top down
         */
        for (j = 0; j < columns; ++j) {
            a3[j] = an1 * a2[j];
            a3[columns + j] = an1 * a2[columns + j] + an2 * a2[j] - ad1 * a3[j];
            for (i = 2; i < rows; ++i) {
                a3[i * columns + j] = an1 * a2[i * columns + j] + an2 * a2[(i - 1) * columns + j] -
                ad1 * a3[(i - 1) * columns + j] - ad2 * a3[(i - 2) * columns + j];
            }
        }
        /*
         *  Columns top down
         */
        for (j = 0; j < columns; ++j) {
            a4[row1 * columns + j] = 0;
            a4[row2 * columns + j] = an3 * a2[row1 * columns + j] - ad1 * a4[row1 * columns + j];
            for (i = row3; i >= 0; --i) {
                a4[i * columns + j] = an3 * a2[(i + 1) * columns + j] + an4 * a2[(i + 2) * columns + j] -
                ad1 * a4[(i + 1) * columns + j] - ad2 * a4[(i + 2) * columns + j];
            }
        }
        icol1 = columns * rows;
        for (i = 0; i < icol1; ++i) {
            a3[i] += a4[i];
        }
        for (i = 0; i < rows; i++) {
            for (j = 0; j < columns; j++) {
                nf_grx[i * columns + j] = a3[i * columns + j];
            }
        }
        /*
         *  SECOND STEP X-GRADIENT : the x-gradient is  done
         *  THIRD STEP : NORM
         *  Computation of the magnitude and angle
         */
        for (i = 0; i < rows; i++) {
            for (j = 0; j < columns; j++) {
                a2[i * columns + j] = nf_gry[i * columns + j];
            }
        }
        icol1 = columns * rows;
        for (i = 0; i < icol1; ++i) {
            norm_deriche[i] = modulus(nf_grx[i], nf_gry[i]);
            angle_deriche[i] = angle(nf_grx[i], nf_gry[i]);
        }
    }

    public double[] nonMaximalSuppression() {
        double[] res = new double[width * height];
        int la = width;
        int ha = height;
        double ag;
        double pix1 = 0;
        double pix2 = 0;
        double pix;
        for (int x = 1; x < la - 1; x++) {
            for (int y = 1; y < ha - 1; y++) {
                ag = getPixel(angle_deriche, x, y);
                if ((ag > -22.5) && (ag <= 22.5)) {
                    pix1 = getPixel(norm_deriche, x + 1, y);
                    pix2 = getPixel(norm_deriche, x - 1, y);
                } else if ((ag > 22.5) && (ag <= 67.5)) {
                    pix1 = getPixel(norm_deriche, x + 1, y - 1);
                    pix2 = getPixel(norm_deriche, x - 1, y + 1);
                } else if (((ag > 67.5) && (ag <= 90)) || ((ag < -67.5) && (ag >= -90))) {
                    pix1 = getPixel(norm_deriche, x, y - 1);
                    pix2 = getPixel(norm_deriche, x, y + 1);
                } else if ((ag < -22.5) && (ag >= -67.5)) {
                    pix1 = getPixel(norm_deriche, x + 1, y + 1);
                    pix2 = getPixel(norm_deriche, x - 1, y - 1);
                }
                pix = getPixel(norm_deriche, x, y);
                if ((pix >= pix1) && (pix >= pix2)) {
                    putPixel(res, pix, x, y);
                }
            }
        }
        return res;
    }

    private double modulus(double dx, double dy) {
        return (Math.sqrt(dx * dx + dy * dy));
    }

    private double angle(double dx, double dy) {
        return (-Math.toDegrees(Math.atan(dy/dx)));
    }
        
    private double getPixel(double[] pixels, int xloc,int yloc) {
        if (xloc >= 0 && xloc < width && yloc >= 0 && yloc < height) {
            return pixels[yloc*width+xloc];
        } else {
            return 0;
        }
    }
        
    private int getPixel(int[] pixels, int xloc, int yloc) {
	if (xloc >= 0 && xloc < width && yloc >= 0 && yloc < height) {
            return pixels[yloc*width+xloc];
        } else {
            return 0;
        }
    }
    
    private void putPixel(int[] pixels, int value, int xloc, int yloc) {
        if (xloc>=0 && xloc<width && yloc>=0 && yloc<height) {
            if (value>255) {
                value = 255;
            }
            if (value<0) {
                value = 0;
            }
            pixels[yloc*width + xloc] = value;
        }
    }
    
    private void putPixel(double[] pixels, double value, int xloc, int yloc) {
	if (xloc>=0 && xloc<width && yloc>=0 && yloc<height) {
            pixels[yloc*width + xloc] = value;
	}
    }
    
    /**
     * Median filter
     * @param inputPixels input pixels - integer values not byte
     * @param filterRadius number of pixels for radius - actually a box surrounding each pixel
     * @return - A new array of integers representing a 1D filtered image array
     */
    public final int[] medianFilter(int[] inputPixels, int filterRadius) {
        int[][] arrays = new int[width][height];
        for(int i = 0; i < width; ++i) {
            for(int j = 0; j < height; ++j) {
                arrays[i][j] = inputPixels[i + (j * width)];
            }
        }
        int[][] outputArrays = new int[width][height];
        for(int j = 0; j < height; ++j) {
            for(int i = 0;i < width; ++i) {
                outputArrays[i][j] = medianPixel(arrays, filterRadius, width, height, i, j);
            }
        }
        int[] output = new int[width * height];
        for(int i = 0; i < width; ++i) {
            for(int j = 0; j < height; ++j) {
                output[i + (j * width)] = outputArrays[i][j];
            }
        }
        return output;
    }
    
    private int medianPixel(int[][] input, int filterRadius, int w, int h, int x, int y) {
        int count = 0;
	int [] workingArray = new int [filterRadius*filterRadius];
        for(int j = 0; j < filterRadius; ++j) {
            for(int i = 0; i < filterRadius; ++i) {
                if(((x-1+i)>=0) && ((y-1+j)>=0) && ((x-1+i)<w) && ((y-1+j)<h)) {
                    workingArray[count]=input[x-1+i][y-1+j];
                    ++count;
                }
            }
        }
        Arrays.sort(workingArray);
        if(count == 0) {
            return 0;
        }
	int index = count / 2;
        return (workingArray[index]);
    }

    /**
     * @return The median filtered pixels
     */
    public int[] getPixels() {
        return pixels;
    }
    
    
}
