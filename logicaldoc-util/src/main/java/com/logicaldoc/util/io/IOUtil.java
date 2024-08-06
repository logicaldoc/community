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
import java.nio.charset.StandardCharsets;
import java.nio.file.StandardCopyOption;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;

public class IOUtil {
	private static final int DEFAULT_BUFFER_SIZE = 10240; // ..bytes = 10KB.

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
	 * @throws IOException In case of I/O issue
	 */
	public static String serialize(Serializable object, String charset) throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try (XMLEncoder encoder = new XMLEncoder(baos, StringUtils.defaultString(charset, "UTF-8"), false, 0);) {
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
}