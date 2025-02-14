package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StreamEater implements Runnable {

	private static final Logger log = LoggerFactory.getLogger(StreamEater.class);

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
		try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream));) {
			String line = br.readLine();
			while (line != null) {
				writeLine(line);

				if (buffer != null) {
					if(!buffer.isEmpty())
						buffer.append("\n");
					buffer.append(line);
				}

				line = br.readLine();
			}
		} catch (IOException e) {
			// nothing to do
		}
	}

	private void writeLine(String line) throws IOException {
		if (prefix != null)
			log.debug("{}: {}", prefix, line);

		if (output != null && line != null) {
			output.write(line);
			output.flush();
		}
	}
}
