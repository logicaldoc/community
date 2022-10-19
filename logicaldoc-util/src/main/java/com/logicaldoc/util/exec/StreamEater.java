package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;

public class StreamEater implements Runnable {

	private InputStream stream;

	private String prefix;

	private Writer output;

	private StringBuffer buffer;

	public StreamEater(String prefix, InputStream stream, StringBuffer buffer) {
		super();
		this.prefix = prefix;
		this.stream = stream;
		this.buffer = buffer;
	}

	public StreamEater(String prefix, InputStream stream, Writer output) {
		super();
		this.prefix = prefix;
		this.stream = stream;
		this.output = output;
	}

	public StreamEater(String prefix, InputStream stream) {
		this(prefix, stream, (Writer) null);
	}

	public void run() {
		try (InputStreamReader isr = new InputStreamReader(stream); BufferedReader br = new BufferedReader(isr);) {
			String line = br.readLine();
			boolean firstLine = true;
			while (line != null) {
				System.out.println((prefix != null ? (prefix + ":") : "") + line);
				if (output != null && line != null) {
					if (!firstLine)
						output.write("\n");
					output.write(line);
					output.flush();

				} else if (buffer != null && line != null) {
					if (!firstLine)
						buffer.append("\n");
					buffer.append(line);
				}
				firstLine = false;
				line = br.readLine();
			}
		} catch (IOException e) {
			// nothing to do
		}
	}
}
