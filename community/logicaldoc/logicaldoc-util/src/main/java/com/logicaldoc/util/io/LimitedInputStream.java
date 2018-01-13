package com.logicaldoc.util.io;

import java.io.DataInput;
import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * This is a decorator that limits a given input stream to a maximum size
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class LimitedInputStream extends FilterInputStream {

	private long left;

	private long mark = -1;

	public LimitedInputStream(InputStream in, long limit) {
		super(in);
		assert (in != null);
		assert (limit >= 0);
		left = limit;
	}

	@Override
	public int available() throws IOException {
		return (int) Math.min(in.available(), left);
	}

	// it's okay to mark even if mark isn't supported, as reset won't work
	@Override
	public synchronized void mark(int readLimit) {
		in.mark(readLimit);
		mark = left;
	}

	@Override
	public int read() throws IOException {
		if (left == 0) {
			return -1;
		}

		int result = in.read();
		if (result != -1) {
			--left;
		}
		return result;
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		if (left == 0) {
			return -1;
		}

		len = (int) Math.min(len, left);
		int result = in.read(b, off, len);
		if (result != -1) {
			left -= result;
		}
		return result;
	}

	@Override
	public synchronized void reset() throws IOException {
		if (!in.markSupported()) {
			throw new IOException("Mark not supported");
		}
		if (mark == -1) {
			throw new IOException("Mark not set");
		}

		in.reset();
		left = mark;
	}

	@Override
	public long skip(long n) throws IOException {
		n = Math.min(n, left);
		long skipped = in.skip(n);
		left -= skipped;
		return skipped;
	}

	/**
	 * Attempts to read enough bytes from the stream to fill the given byte
	 * array, with the same behavior as {@link DataInput#readFully(byte[])}.
	 * Does not close the stream.
	 *
	 * @param in the input stream to read from.
	 * @param b the buffer into which the data is read.
	 * @throws EOFException if this stream reaches the end before reading all
	 *         the bytes.
	 * @throws IOException if an I/O error occurs.
	 */
	public static void readFully(InputStream in, byte[] b) throws IOException {
		readFully(in, b, 0, b.length);
	}

	/**
	 * Attempts to read {@code len} bytes from the stream into the given array
	 * starting at {@code off}, with the same behavior as
	 * {@link DataInput#readFully(byte[], int, int)}. Does not close the stream.
	 *
	 * @param in the input stream to read from.
	 * @param b the buffer into which the data is read.
	 * @param off an int specifying the offset into the data.
	 * @param len an int specifying the number of bytes to read.
	 * @throws EOFException if this stream reaches the end before reading all
	 *         the bytes.
	 * @throws IOException if an I/O error occurs.
	 */
	public static void readFully(InputStream in, byte[] b, int off, int len) throws IOException {
		int read = read(in, b, off, len);
		if (read != len) {
			throw new EOFException("reached end of stream after reading " + read + " bytes; " + len + " bytes expected");
		}
	}

	/**
	 * Discards {@code n} bytes of data from the input stream. This method will
	 * block until the full amount has been skipped. Does not close the stream.
	 *
	 * @param in the input stream to read from
	 * @param n the number of bytes to skip
	 * @throws EOFException if this stream reaches the end before skipping all
	 *         the bytes
	 * @throws IOException if an I/O error occurs, or the stream does not
	 *         support skipping
	 */
	public static void skipFully(InputStream in, long n) throws IOException {
		long skipped = skipUpTo(in, n);
		if (skipped < n) {
			throw new EOFException("reached end of stream after skipping " + skipped + " bytes; " + n
					+ " bytes expected");
		}
	}

	/**
	 * Discards up to {@code n} bytes of data from the input stream. This method
	 * will block until either the full amount has been skipped or until the end
	 * of the stream is reached, whichever happens first. Returns the total
	 * number of bytes skipped.
	 */
	static long skipUpTo(InputStream in, final long n) throws IOException {
		long totalSkipped = 0;
		byte[] buf = createBuffer();

		while (totalSkipped < n) {
			long remaining = n - totalSkipped;
			long skipped = skipSafely(in, remaining);

			if (skipped == 0) {
				// Do a buffered read since skipSafely could return 0
				// repeatedly, for example if
				// in.available() always returns 0 (the default).
				int skip = (int) Math.min(remaining, buf.length);
				if ((skipped = in.read(buf, 0, skip)) == -1) {
					// Reached EOF
					break;
				}
			}

			totalSkipped += skipped;
		}

		return totalSkipped;
	}

	/**
	 * Attempts to skip up to {@code n} bytes from the given input stream, but
	 * not more than {@code in.available()} bytes. This prevents
	 * {@code FileInputStream} from skipping more bytes than actually remain in
	 * the file, something that it
	 * {@linkplain java.io.FileInputStream#skip(long) specifies} it can do in
	 * its Javadoc despite the fact that it is violating the contract of
	 * {@code InputStream.skip()}.
	 */
	private static long skipSafely(InputStream in, long n) throws IOException {
		int available = in.available();
		return available == 0 ? 0 : in.skip(Math.min(available, n));
	}

	/**
	 * Reads some bytes from an input stream and stores them into the buffer
	 * array {@code b}. This method blocks until {@code len} bytes of input data
	 * have been read into the array, or end of file is detected. The number of
	 * bytes read is returned, possibly zero. Does not close the stream.
	 *
	 * <p>
	 * A caller can detect EOF if the number of bytes read is less than
	 * {@code len}. All subsequent calls on the same stream will return zero.
	 *
	 * <p>
	 * If {@code b} is null, a {@code NullPointerException} is thrown. If
	 * {@code off} is negative, or {@code len} is negative, or {@code off+len}
	 * is greater than the length of the array {@code b}, then an
	 * {@code IndexOutOfBoundsException} is thrown. If {@code len} is zero, then
	 * no bytes are read. Otherwise, the first byte read is stored into element
	 * {@code b[off]}, the next one into {@code b[off+1]}, and so on. The number
	 * of bytes read is, at most, equal to {@code len}.
	 *
	 * @param in the input stream to read from
	 * @param b the buffer into which the data is read
	 * @param off an int specifying the offset into the data
	 * @param len an int specifying the number of bytes to read
	 * @return the number of bytes read
	 * @throws IOException if an I/O error occurs
	 */
	// Sometimes you don't care how many bytes you actually read, I guess.
	// (You know that it's either going to read len bytes or stop at EOF.)
	public static int read(InputStream in, byte[] b, int off, int len) throws IOException {
		assert (in != null);
		assert (b != null);
		if (len < 0) {
			throw new IndexOutOfBoundsException("len is negative");
		}
		int total = 0;
		while (total < len) {
			int result = in.read(b, off + total, len - total);
			if (result == -1) {
				break;
			}
			total += result;
		}
		return total;
	}

	/**
	 * Creates a new byte array for buffering reads or writes.
	 */
	private static byte[] createBuffer() {
		return new byte[8192];
	}
}