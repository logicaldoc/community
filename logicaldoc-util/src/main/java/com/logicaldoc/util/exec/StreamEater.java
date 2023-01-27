package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;

public class StreamEater implements Runnable {

	private InputStream inputStream;

	private String prefix;

	private Writer output;

	private StringBuilder buffer;

	public StreamEater(String prefix, InputStream inputStream, StringBuilder buffer) {
		super();
		this.prefix = prefix;
		this.inputStream = inputStream;
		this.buffer = buffer;
	}

	public StreamEater(String prefix, InputStream inptutStream, Writer output) {
		super();
		this.prefix = prefix;
		this.inputStream = inptutStream;
		this.output = output;
	}

	public StreamEater(String prefix, InputStream stream) {
		this(prefix, stream, (Writer) null);
	}

	public void run() {
		try (InputStreamReader isr = new InputStreamReader(inputStream); BufferedReader br = new BufferedReader(isr);) {
			String line = br.readLine();
			boolean firstLine = true;
			while (line != null) {
				writeLine(line, firstLine);

				if (buffer != null && line != null) {
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

	private void writeLine(String line, boolean firstLine) throws IOException {
		if (prefix != null)
			System.out.println(prefix + ": " + line);

		if (output != null && line != null) {
			if (!firstLine)
				output.write("\n");
			output.write(line);
			output.flush();
		}
	}
}
