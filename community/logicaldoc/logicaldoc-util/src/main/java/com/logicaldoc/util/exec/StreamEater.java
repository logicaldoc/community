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

	public StreamEater(String prefix, InputStream stream, Writer output) {
		super();
		this.prefix = prefix;
		this.stream = stream;
		this.output = output;
	}

	public StreamEater(String prefix, InputStream stream) {
		this(prefix, stream, null);
	}

	public void run() {

		try {
			InputStreamReader isr = new InputStreamReader(stream);

			BufferedReader br = new BufferedReader(isr);

			String line = br.readLine();

			while (line != null) {
				System.out.println(prefix + ":" + line);
				if (output != null && line != null) {
					output.write(line+"\n");
					output.flush();
				}
				line = br.readLine();
			}
			
			br.close();
		} catch (IOException e) {

		} finally{
			if (output != null)
				try {
					output.flush();
					output.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		}
	}
}
