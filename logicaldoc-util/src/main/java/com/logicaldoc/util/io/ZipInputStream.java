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

	@Override
	public int available() throws IOException {
		return wrapped.available();
	}

	@Override
	public void close() throws IOException {
		wrapped.close();
	}

	@Override
	public int hashCode() {
		return wrapped.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return wrapped.equals(obj);
	}

	@Override
	public synchronized void mark(int readlimit) {
		wrapped.mark(readlimit);
	}

	@Override
	public boolean markSupported() {
		return wrapped.markSupported();
	}

	@Override
	public int read() throws IOException {
		return wrapped.read();
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		return wrapped.read(b, off, len);
	}

	@Override
	public int read(byte[] b) throws IOException {
		return wrapped.read(b);
	}

	@Override
	public synchronized void reset() throws IOException {
		wrapped.reset();
	}

	@Override
	public long skip(long n) throws IOException {
		return wrapped.skip(n);
	}

	@Override
	public String toString() {
		return wrapped.toString();
	}
}