package com.logicaldoc.util.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

/**
 * Wrapper around a standard InputStream that deletes the file after closing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class AutoDeleteInputStream extends InputStream {
	private InputStream wrappedStream;

	private File file;

	public AutoDeleteInputStream(File file) throws FileNotFoundException {
		super();
		this.file = file;
		this.wrappedStream = new FileInputStream(file);
	}

	public AutoDeleteInputStream(InputStream wrappedStream, File file) {
		super();
		this.wrappedStream = wrappedStream;
		this.file = file;
	}

	public int read() throws IOException {
		return wrappedStream.read();
	}

	public int hashCode() {
		return wrappedStream.hashCode();
	}

	@Override
	public int read(byte[] b) throws IOException {
		return wrappedStream.read(b);
	}

	@Override
	public boolean equals(Object obj) {
		return wrappedStream.equals(obj);
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		return wrappedStream.read(b, off, len);
	}

	@Override
	public long skip(long n) throws IOException {
		return wrappedStream.skip(n);
	}

	@Override
	public String toString() {
		return wrappedStream.toString();
	}

	@Override
	public int available() throws IOException {
		return wrappedStream.available();
	}

	@Override
	public void close() throws IOException {
		wrappedStream.close();
		if (file != null)
			FileUtil.delete(file);
	}

	@Override
	public synchronized void mark(int readlimit) {
		wrappedStream.mark(readlimit);
	}

	@Override
	public synchronized void reset() throws IOException {
		wrappedStream.reset();
	}

	@Override
	public boolean markSupported() {
		return wrappedStream.markSupported();
	}
}