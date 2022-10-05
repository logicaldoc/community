package com.logicaldoc.util.io;

import java.io.IOException;
import java.io.InputStream;

/**
 * Wrapper around a standard AutoDeleteZipInputStream that avoids the CRC checks
 * on stream closure.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.2
 */
public class ZipInputStream extends InputStream {

	protected net.lingala.zip4j.io.inputstream.ZipInputStream wrapped;

	public ZipInputStream(net.lingala.zip4j.io.inputstream.ZipInputStream wrapped) {
		this.wrapped = wrapped;
	}

	public int available() throws IOException {
		return wrapped.available();
	}

	public void close() throws IOException {
		wrapped.close();
	}

	public int hashCode() {
		return wrapped.hashCode();
	}

	public boolean equals(Object obj) {
		return wrapped.equals(obj);
	}

	@Override
	public synchronized void mark(int readlimit) {
		wrapped.mark(readlimit);
	}

	public boolean markSupported() {
		return wrapped.markSupported();
	}

	public int read() throws IOException {
		return wrapped.read();
	}

	public int read(byte[] b, int off, int len) throws IOException {
		return wrapped.read(b, off, len);
	}

	public int read(byte[] b) throws IOException {
		return wrapped.read(b);
	}

	@Override
	public synchronized void reset() throws IOException {
		wrapped.reset();
	}

	public long skip(long n) throws IOException {
		return wrapped.skip(n);
	}

	public String toString() {
		return wrapped.toString();
	}

	@Override
	protected void finalize() throws Throwable {
		close();
	}
}
