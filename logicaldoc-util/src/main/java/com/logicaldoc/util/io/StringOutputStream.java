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

	StringBuilder sb = new StringBuilder();

	public StringOutputStream() {
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
	public void write(int i) throws IOException {
		sb.append((char) i);
	}

	public String getData() {
		return sb.toString();
	}
}