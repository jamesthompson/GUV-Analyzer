package com.jamesrthompson.Data;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;


/**
 * Reads raw 8-bit, 16-bit or 32-bit (float or RGB) images from a stream or URL.
 */

public class Reader {

    private static final int CLEAR_CODE = 256;
    private static final int EOI_CODE = 257;
    private FileInfo fi;
    private int width, height;
    private long skipCount;
    private int bytesPerPixel, bufferSize, nPixels;
    private long byteCount;
    private int eofErrorCount;
    public double min, max;

    /**
     * Constructs a new Reader using a FileInfo object to describe the file
     * to be read.
     *
     * @param fi
     */
    public Reader(FileInfo fi) {
        this.fi = fi;
        width = fi.width;
        height = fi.height;
        skipCount = fi.getOffset();
    }

    void eofError() {
        eofErrorCount++;
    }

    byte[] read8bitImage(InputStream in) throws IOException {
        if (fi.compression > FileInfo.COMPRESSION_NONE) {
            return readCompressed8bitImage(in);
        }
        byte[] pixels = new byte[nPixels];
        // assume contiguous strips
        int count, actuallyRead;
        int totalRead = 0;
        while (totalRead < byteCount) {
            if (totalRead + bufferSize > byteCount) {
                count = (int) (byteCount - totalRead);
            } else {
                count = bufferSize;
            }
            actuallyRead = in.read(pixels, totalRead, count);
            if (actuallyRead == -1) {
                eofError();
                break;
            }
            totalRead += actuallyRead;
        }
        return pixels;
    }

    byte[] readCompressed8bitImage(InputStream in) throws IOException {
        byte[] pixels = new byte[nPixels];
        int current = 0;
        byte last = 0;
        for (int i = 0; i < fi.stripOffsets.length; i++) {
            if (in instanceof RandomAccessStream) {
                ((RandomAccessStream) in).seek(fi.stripOffsets[i]);
            } else if (i > 0) {
                long skip = (fi.stripOffsets[i] & 0xffffffffL) - (fi.stripOffsets[i - 1] & 0xffffffffL) - fi.stripLengths[i - 1];
                if (skip > 0L) {
                    in.skip(skip);
                }
            }
            byte[] byteArray = new byte[fi.stripLengths[i]];
            int read = 0, left = byteArray.length;
            while (left > 0) {
                int r = in.read(byteArray, read, left);
                if (r == -1) {
                    eofError();
                    break;
                }
                read += r;
                left -= r;
            }
            byteArray = uncompress(byteArray);
            int length = byteArray.length;
            length = length - (length % fi.width);
            if (fi.compression == FileInfo.LZW_WITH_DIFFERENCING) {
                for (int b = 0; b < length; b++) {
                    byteArray[b] += last;
                    last = b % fi.width == fi.width - 1 ? 0 : byteArray[b];
                }
            }
            if (current + length > pixels.length) {
                length = pixels.length - current;
            }
            System.arraycopy(byteArray, 0, pixels, current, length);
            current += length;
        }
        return pixels;
    }

    /**
     * Reads a 16-bit image. Signed pixels are converted to unsigned by adding
     * 32768.
     */
    short[] read16bitImage(InputStream in) throws IOException {
        if (fi.compression > FileInfo.COMPRESSION_NONE || (fi.stripOffsets != null && fi.stripOffsets.length > 1)) {
            return readCompressed16bitImage(in);
        }
        int pixelsRead;
        byte[] buffer = new byte[bufferSize];
        short[] pixels = new short[nPixels];
        long totalRead = 0L;
        int base = 0;
        int count, value;
        int bufferCount;

        while (totalRead < byteCount) {
            if ((totalRead + bufferSize) > byteCount) {
                bufferSize = (int) (byteCount - totalRead);
            }
            bufferCount = 0;
            while (bufferCount < bufferSize) { // fill the buffer
                count = in.read(buffer, bufferCount, bufferSize - bufferCount);
                if (count == -1) {
                    if (bufferCount > 0) {
                        for (int i = bufferCount; i < bufferSize; i++) {
                            buffer[i] = 0;
                        }
                    }
                    totalRead = byteCount;
                    eofError();
                    break;
                }
                bufferCount += count;
            }
            totalRead += bufferSize;
            pixelsRead = bufferSize / bytesPerPixel;
            if (fi.intelByteOrder) {
                if (fi.fileType == FileInfo.GRAY16_SIGNED) {
                    for (int i = base, j = 0; i < (base + pixelsRead); i++, j += 2) {
                        pixels[i] = (short) ((((buffer[j + 1] & 0xff) << 8) | (buffer[j] & 0xff)) + 32768);
                    }
                } else {
                    for (int i = base, j = 0; i < (base + pixelsRead); i++, j += 2) {
                        pixels[i] = (short) (((buffer[j + 1] & 0xff) << 8) | (buffer[j] & 0xff));
                    }
                }
            } else {
                if (fi.fileType == FileInfo.GRAY16_SIGNED) {
                    for (int i = base, j = 0; i < (base + pixelsRead); i++, j += 2) {
                        pixels[i] = (short) ((((buffer[j] & 0xff) << 8) | (buffer[j + 1] & 0xff)) + 32768);
                    }
                } else {
                    for (int i = base, j = 0; i < (base + pixelsRead); i++, j += 2) {
                        pixels[i] = (short) (((buffer[j] & 0xff) << 8) | (buffer[j + 1] & 0xff));
                    }
                }
            }
            base += pixelsRead;
        }
        return pixels;
    }

    short[] readCompressed16bitImage(InputStream in) throws IOException {
        short[] pixels = new short[nPixels];
        int base = 0;
        short last = 0;
        for (int k = 0; k < fi.stripOffsets.length; k++) {
            //IJ.log("seek: "+fi.stripOffsets[k]+" "+fi.stripLengths[k]+"  "+(in instanceof RandomAccessStream));
            if (in instanceof RandomAccessStream) {
                ((RandomAccessStream) in).seek(fi.stripOffsets[k]);
            } else if (k > 0) {
                long skip = (fi.stripOffsets[k] & 0xffffffffL) - (fi.stripOffsets[k - 1] & 0xffffffffL) - fi.stripLengths[k - 1];
                if (skip > 0L) {
                    in.skip(skip);
                }
            }
            byte[] byteArray = new byte[fi.stripLengths[k]];
            int read = 0, left = byteArray.length;
            while (left > 0) {
                int r = in.read(byteArray, read, left);
                if (r == -1) {
                    eofError();
                    break;
                }
                read += r;
                left -= r;
            }
            byteArray = uncompress(byteArray);
            int pixelsRead = byteArray.length / bytesPerPixel;
            pixelsRead = pixelsRead - (pixelsRead % fi.width);
            int pmax = base + pixelsRead;
            if (pmax > nPixels) {
                pmax = nPixels;
            }
            if (fi.intelByteOrder) {
                for (int i = base, j = 0; i < pmax; i++, j += 2) {
                    pixels[i] = (short) (((byteArray[j + 1] & 0xff) << 8) | (byteArray[j] & 0xff));
                }
            } else {
                for (int i = base, j = 0; i < pmax; i++, j += 2) {
                    pixels[i] = (short) (((byteArray[j] & 0xff) << 8) | (byteArray[j + 1] & 0xff));
                }
            }
            if (fi.compression == FileInfo.LZW_WITH_DIFFERENCING) {
                for (int b = base; b < pmax; b++) {
                    pixels[b] += last;
                    last = b % fi.width == fi.width - 1 ? 0 : pixels[b];
                }
            }
            base += pixelsRead;
        }
        if (fi.fileType == FileInfo.GRAY16_SIGNED) {
            // convert to unsigned
            for (int i = 0; i < nPixels; i++) {
                pixels[i] = (short) (pixels[i] + 32768);
            }
        }
        return pixels;
    }

    

    void skip(InputStream in) throws IOException {
        if (skipCount > 0) {
            long bytesRead = 0;
            int skipAttempts = 0;
            long count;
            while (bytesRead < skipCount) {
                count = in.skip(skipCount - bytesRead);
                skipAttempts++;
                if (count == -1 || skipAttempts > 5) {
                    break;
                }
                bytesRead += count;
                //IJ.log("skip: "+skipCount+" "+count+" "+bytesRead+" "+skipAttempts);
            }
        }
        byteCount = ((long) width) * height * bytesPerPixel;
        if (fi.fileType == FileInfo.BITMAP) {
            int scan = width / 8, pad = width % 8;
            if (pad > 0) {
                scan++;
            }
            byteCount = scan * height;
        }
        nPixels = width * height;
        bufferSize = (int) (byteCount / 25L);
        if (bufferSize < 8192) {
            bufferSize = 8192;
        } else {
            bufferSize = (bufferSize / 8192) * 8192;
        }
    }

    /**
     * Reads the image from the InputStream and returns the pixel array (byte,
     * short, int or float). Returns null if there was an IO exception. Does not
     * close the InputStream.
     */
    public Object readPixels(InputStream in) {
        Object pixels;
        try {
            switch (fi.fileType) {
                case FileInfo.GRAY8:
                case FileInfo.COLOR8:
                    bytesPerPixel = 1;
                    skip(in);
                    pixels = (Object) read8bitImage(in);
                    break;
                case FileInfo.GRAY16_SIGNED:
                case FileInfo.GRAY16_UNSIGNED:
                    bytesPerPixel = 2;
                    skip(in);
                    pixels = (Object) read16bitImage(in);
                    break;
                default:
                    pixels = null;
            }
            return pixels;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Skips the specified number of bytes, then reads an image and returns the
     * pixel array (byte, short, int or float). Returns null if there was an IO
     * exception. Does not close the InputStream.
     */
    public Object readPixels(InputStream in, long skipCount) {
        this.skipCount = skipCount;
        Object pixels = readPixels(in);
        if (eofErrorCount > 0) {
            return null;
        } else {
            return pixels;
        }
    }

    /**
     * Reads the image from a URL and returns the pixel array (byte, short, int
     * or float). Returns null if there was an IO exception.
     */
    public Object readPixels(String url) {
        URL theURL;
        InputStream is;
        try {
            theURL = new URL(url);
        } catch (MalformedURLException e) {
            return null;
        }
        try {
            is = theURL.openStream();
        } catch (IOException e) {
            return null;
        }
        return readPixels(is);
    }

    byte[] uncompress(byte[] input) {
        if (fi.compression == FileInfo.PACK_BITS) {
            return packBitsUncompress(input, fi.rowsPerStrip * fi.width * fi.getBytesPerPixel());
        } else if (fi.compression == FileInfo.LZW || fi.compression == FileInfo.LZW_WITH_DIFFERENCING) {
            return lzwUncompress(input);
        } else if (fi.compression == FileInfo.ZIP) {
            return zipUncompress(input);
        } else {
            return input;
        }
    }

    /**
     * TIFF Adobe ZIP support contributed by Jason Newton.
     */
    public byte[] zipUncompress(byte[] input) {
        ByteArrayOutputStream imageBuffer = new ByteArrayOutputStream();
        byte[] buffer = new byte[1024];
        Inflater decompressor = new Inflater();
        decompressor.setInput(input);
        try {
            while (!decompressor.finished()) {
                int rlen = decompressor.inflate(buffer);
                imageBuffer.write(buffer, 0, rlen);
            }
        } catch (DataFormatException e) {
        }
        decompressor.end();
        return imageBuffer.toByteArray();
    }

    /**
     * Utility method for decoding an LZW-compressed image strip. Adapted from
     * the TIFF 6.0 Specification:
     * http://partners.adobe.com/asn/developer/pdfs/tn/TIFF6.pdf (page 61)
     *
     * @author Curtis Rueden (ctrueden at wisc.edu)
     */
    public byte[] lzwUncompress(byte[] input) {
        if (input == null || input.length == 0) {
            return input;
        }
        byte[][] symbolTable = new byte[4096][1];
        int bitsToRead = 9;
        int nextSymbol = 258;
        int code;
        int oldCode = -1;
        ByteVector out = new ByteVector(8192);
        byte[] byteBuffer1 = new byte[16];
        byte[] byteBuffer2 = new byte[16];

        while (out.size() < byteCount) {
            code = BitBuffer.getBits(input, bitsToRead);
            if (code == EOI_CODE || code == -1) {
                break;
            }
            if (code == CLEAR_CODE) {
                // initialize symbol table
                for (int i = 0; i < 256; i++) {
                    symbolTable[i][0] = (byte) i;
                }
                nextSymbol = 258;
                bitsToRead = 9;
                code = BitBuffer.getBits(input, bitsToRead);
                if (code == EOI_CODE || code == -1) {
                    break;
                }
                out.add(symbolTable[code]);
                oldCode = code;
            } else {
                if (code < nextSymbol) {
                    // code is in table
                    out.add(symbolTable[code]);
                    // add string to table
                    ByteVector symbol = new ByteVector(byteBuffer1);
                    symbol.add(symbolTable[oldCode]);
                    symbol.add(symbolTable[code][0]);
                    symbolTable[nextSymbol] = symbol.toByteArray(); //**
                    oldCode = code;
                    nextSymbol++;
                } else {
                    // out of table
                    ByteVector symbol = new ByteVector(byteBuffer2);
                    symbol.add(symbolTable[oldCode]);
                    symbol.add(symbolTable[oldCode][0]);
                    byte[] outString = symbol.toByteArray();
                    out.add(outString);
                    symbolTable[nextSymbol] = outString; //**
                    oldCode = code;
                    nextSymbol++;
                }
                if (nextSymbol == 511) {
                    bitsToRead = 10;
                }
                if (nextSymbol == 1023) {
                    bitsToRead = 11;
                }
                if (nextSymbol == 2047) {
                    bitsToRead = 12;
                }
            }
        }
        return out.toByteArray();
    }

    /**
     * Based on the Bio-Formats PackbitsCodec written by Melissa Linkert.
     */
    public byte[] packBitsUncompress(byte[] input, int expected) {
        if (expected == 0) {
            expected = Integer.MAX_VALUE;
        }
        ByteVector output = new ByteVector(1024);
        int index = 0;
        while (output.size() < expected && index < input.length) {
            byte n = input[index++];
            if (n >= 0) { // 0 <= n <= 127
                byte[] b = new byte[n + 1];
                for (int i = 0; i < n + 1; i++) {
                    b[i] = input[index++];
                }
                output.add(b);
            } else if (n != -128) { // -127 <= n <= -1
                int len = -n + 1;
                byte inp = input[index++];
                for (int i = 0; i < len; i++) {
                    output.add(inp);
                }
            }
        }
        return output.toByteArray();
    }
}

class ByteVector {

    private byte[] data;
    private int size;

    public ByteVector() {
        data = new byte[10];
        size = 0;
    }

    public ByteVector(int initialSize) {
        data = new byte[initialSize];
        size = 0;
    }

    public ByteVector(byte[] byteBuffer) {
        data = byteBuffer;
        size = 0;
    }

    public void add(byte x) {
        if (size >= data.length) {
            doubleCapacity();
            add(x);
        } else {
            data[size++] = x;
        }
    }

    public int size() {
        return size;
    }

    public void add(byte[] array) {
        int length = array.length;
        while (data.length - size < length) {
            doubleCapacity();
        }
        System.arraycopy(array, 0, data, size, length);
        size += length;
    }

    void doubleCapacity() {
        //IJ.log("double: "+data.length*2);
        byte[] tmp = new byte[data.length * 2 + 1];
        System.arraycopy(data, 0, tmp, 0, data.length);
        data = tmp;
    }

    public void clear() {
        size = 0;
    }

    public byte[] toByteArray() {
        byte[] bytes = new byte[size];
        System.arraycopy(data, 0, bytes, 0, size);
        return bytes;
    }
}