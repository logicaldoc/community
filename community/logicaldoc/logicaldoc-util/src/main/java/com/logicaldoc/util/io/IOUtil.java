package com.logicaldoc.util.io;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.file.StandardCopyOption;

public class IOUtil {
	private static final int DEFAULT_BUFFER_SIZE = 10240; // ..bytes = 10KB.

	/**
	 * Close the given resource.
	 * 
	 * @param resource The resource to be closed.
	 */
	public static void close(Closeable resource) {
		if (resource != null) {
			try {
				resource.close();
			} catch (IOException ignore) {
				// Ignore IOException. If you want to handle this anyway, it
				// might be useful to know
				// that this will generally only be thrown when the client
				// aborted the request.
			}
		}
	}

	/**
	 * Creates a new input stream that represents the first part of the original
	 * stream.
	 * 
	 * @param input Original input stream
	 * @param limit Size of the new stream, expressed in bytes
	 * 
	 * @return the limited input stream
	 */
	public static InputStream getLimitedStream(InputStream input, long limit) {
		return new LimitedInputStream(input, limit);
	}

	public static void write(InputStream input, OutputStream output) throws IOException {
		int letter = 0;
		byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
		while ((letter = input.read(buffer)) != -1) {
			output.write(buffer, 0, letter);
		}
	}

	public static void write(InputStream input, File output) throws IOException {
		java.nio.file.Files.copy(input, output.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}

	public static String getStringFromInputStream(InputStream is) throws IOException {
		BufferedReader br = null;
		StringBuilder sb = new StringBuilder();

		String line;
		try {
			br = new BufferedReader(new InputStreamReader(is));
			while ((line = br.readLine()) != null) {
				if (sb.length() > 0)
					sb.append("\n");
				sb.append(line);
			}
		} finally {
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

		return sb.toString();

	}
}