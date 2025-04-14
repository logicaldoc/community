package com.logicaldoc.util.io;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.StandardCopyOption;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IOUtil {

	private static final int DEFAULT_BUFFER_SIZE = 10240; // ..bytes = 10KB.

	private static final Logger log = LoggerFactory.getLogger(IOUtil.class);

	private IOUtil() {
		throw new IllegalStateException("Utility class");
	}

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
		int totalBytes = 0;
		byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
		while ((totalBytes = input.read(buffer)) != -1) {
			output.write(buffer, 0, totalBytes);
		}
	}

	public static void write(InputStream input, File output) throws IOException {
		java.nio.file.Files.copy(input, output.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}

	public static String readStream(InputStream is) throws IOException {
		return IOUtils.toString(is, StandardCharsets.UTF_8);
	}

	public static byte[] getBytesOfStream(InputStream stream) throws IOException {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream();
				BufferedOutputStream bos = new BufferedOutputStream(baos);
				BufferedInputStream bis = new BufferedInputStream(stream);) {
			int aByte;
			while ((aByte = bis.read()) != -1) {
				bos.write(aByte);
			}
			bos.flush();
			return baos.toByteArray();
		}
	}

	/**
	 * Serializes an object into XML string
	 * 
	 * @param object The object to serialize
	 * @param charset The charset to use for the output, if not provided UTF-8
	 *        is used instead
	 * @return The serialized string
	 */
	public static String serialize(Serializable object, String charset) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try (XMLEncoder encoder = new XMLEncoder(baos, StringUtils.defaultString(charset, "UTF-8"), false, 0);) {
			encoder.setExceptionListener(e -> log.warn(e.getMessage(), e));
			encoder.writeObject(object);
		}
		return baos.toString().trim().replace("\n", "");
	}

	/**
	 * Deserializes an XML serialization to obtain the object
	 * 
	 * @param xml The XML content
	 * @return The recovered object
	 */
	public static Object deserialize(String xml) {
		try (XMLDecoder decoder = new XMLDecoder(new ByteArrayInputStream(xml.getBytes()))) {
			return decoder.readObject();
		}
	}

	/**
	 * Downloads an URL into a local file
	 * 
	 * @param url The URL to download
	 * @param dest The destination local file
	 * @param timeout A connection timeout in seconds
	 * @param bufferSize The buffer size in bytes
	 * 
	 * @throws IOException I/O error
	 * @throws URISyntaxException The URL is invalid
	 */
	public static void download(final String url, File dest, int timeout, int bufferSize)
			throws IOException, URISyntaxException {
		if (url == null || url.isEmpty()) {
			throw new IOException("Url argument is not specified"); // URL
																	// isn't
																	// specified
		}

		URL uri = new URI(url).toURL();
		java.net.HttpURLConnection connection = (java.net.HttpURLConnection) uri.openConnection();
		if (timeout > 0)
			connection.setConnectTimeout(timeout);
		try (InputStream stream = connection.getInputStream();) {
			int statusCode = connection.getResponseCode();

			if (statusCode != HttpServletResponse.SC_OK) { // checking status
															// code
				connection.disconnect();
				throw new IOException("Document editing service returned status: " + statusCode);
			}

			if (stream == null)
				throw new IOException("Input stream is null");

			FileUtil.writeFile(getAllBytes(stream, bufferSize), dest.getAbsolutePath());
		} finally {
			connection.disconnect();
		}
	}

	// get byte array from stream
	private static byte[] getAllBytes(InputStream is, int bufferSize) throws IOException {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		byte[] buffer = new byte[bufferSize];
		for (int len = is.read(buffer); len != -1; len = is.read(buffer)) {
			os.write(buffer, 0, len);
		}
		return os.toByteArray();
	}
}