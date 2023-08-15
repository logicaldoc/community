package com.logicaldoc.util.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * An output stram that writes into a string
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class StringOutputStream extends OutputStream {

	private StringBuilder sb = new StringBuilder();

	public StringOutputStream() {
	}

	@Override
	public void write(int b) throws IOException {
		sb.append((char) b);
	}

	public StringOutputStream(StringBuilder sb) {
		this.sb = sb;
	}

	@Override
	public void close() throws IOException {
		sb = new StringBuilder();
	}

	@Override
	public void flush() throws IOException {
		// Nothing to do
	}

	@Override
	public void write(byte[] b) throws IOException {
		sb.append(new String(b));
	}

	public void write(byte b) {
		sb.append((char) b);
	}

	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		for (byte c : b) {
			write(c);
		}
	}

	public String getData() {
		return sb.toString();
	}
}