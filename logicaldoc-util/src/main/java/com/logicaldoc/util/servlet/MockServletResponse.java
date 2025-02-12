package com.logicaldoc.util.servlet;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

/**
 * A servlet created for tests
 * 
 * @author Francesco Bignardi
 * @since 8.9
 */
public class MockServletResponse implements HttpServletResponse {

	protected static Logger log = LoggerFactory.getLogger(MockServletResponse.class);

	private File output;

	private PrintWriter outputWriter;

	private int contentLength = 0;

	private String contentType;

	private String characterEncoding;

	private Map<String, String> headers = new HashMap<>();
	
	private int status = HttpServletResponse.SC_OK;

	public MockServletResponse(File output) {
		super();
		this.output = output;
	}

	@Override
	public void flushBuffer() throws IOException {
		if (outputWriter != null)
			outputWriter.flush();
	}

	@Override
	public int getBufferSize() {
		return 0;
	}

	@Override
	public String getCharacterEncoding() {
		return characterEncoding;
	}

	@Override
	public String getContentType() {
		return contentType;
	}

	@Override
	public Locale getLocale() {
		return null;
	}

	@Override
	public ServletOutputStream getOutputStream() throws IOException {
		log.debug("getOutputStream");
		return new ServletOutputStream() {
			private FileOutputStream fileOutputStream = new FileOutputStream(output, true);

			@Override
			public void write(int b) throws IOException {
				fileOutputStream.write(b);
			}

			@Override
			public void write(byte[] b) throws IOException {
				fileOutputStream.write(b);
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				fileOutputStream.write(b, off, len);
				if (log.isDebugEnabled())
					log.debug("Wrote {}", new String(b));
			}

			@Override
			public void flush() throws IOException {
				fileOutputStream.flush();
			}

			@Override
			public void close() throws IOException {
				fileOutputStream.close();
			}

			@Override
			public boolean isReady() {
				return true;
			}

			@Override
			public void setWriteListener(WriteListener arg0) {
				// Ignore
			}
		};
	}

	@Override
	public PrintWriter getWriter() throws IOException {
		if (outputWriter == null)
			outputWriter = new PrintWriter(new FileWriter(output));
		return outputWriter;
	}

	@Override
	public boolean isCommitted() {
		return false;
	}

	@Override
	public void reset() {
		// Nothing to do
	}

	@Override
	public void resetBuffer() {
		// Nothing to do
	}

	@Override
	public void setBufferSize(int arg0) {
		// Nothing to do
	}

	@Override
	public void setCharacterEncoding(String characterEncoding) {
		this.characterEncoding = characterEncoding;
	}

	@Override
	public void setContentLength(int contentLength) {
		this.contentLength = contentLength;
	}

	@Override
	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	@Override
	public void setLocale(Locale arg0) {
		// Nothing to do
	}

	@Override
	public void addCookie(Cookie arg0) {
		// Nothing to do
	}

	@Override
	public void addDateHeader(String arg0, long arg1) {
		// Nothing to do
	}

	@Override
	public void addHeader(String name, String value) {
		setHeader(name, value);
	}

	public String getHeader(String name) {
		return headers.get(name);
	}

	@Override
	public void addIntHeader(String arg0, int arg1) {
		// Nothing to do
	}

	@Override
	public boolean containsHeader(String arg0) {
		return false;
	}

	@Override
	public String encodeRedirectURL(String arg0) {
		return null;
	}

	@Override
	public String encodeRedirectUrl(String arg0) {
		return null;
	}

	@Override
	public String encodeURL(String arg0) {
		return null;
	}

	@Override
	public String encodeUrl(String arg0) {
		return null;
	}

	@Override
	public void sendError(int arg0) throws IOException {
		// Nothing to do
	}

	@Override
	public void sendError(int arg0, String arg1) throws IOException {
		// Nothing to do
	}

	@Override
	public void sendRedirect(String arg0) throws IOException {
		// Nothing to do
	}

	@Override
	public void setDateHeader(String arg0, long arg1) {
		// Nothing to do
	}

	@Override
	public void setHeader(String name, String value) {
		headers.put(name, value);
	}

	@Override
	public void setIntHeader(String arg0, int arg1) {
		// Nothing to do
	}

	@Override
	public void setStatus(int status) {
		this.status = status;
	}

	@Override
	public void setStatus(int status, String statusMessage) {
		this.status = status;
	}

	public int getContentLength() {
		return contentLength;
	}

	public String getOutputString() throws IOException {
		if (output != null)
			return FileUtil.readFile(output);
		else
			return "";
	}

	public Map<String, String> getHeaders() {
		return headers;
	}

	@Override
	public void setContentLengthLong(long arg0) {
		// Ignore
	}

	@Override
	public Collection<String> getHeaderNames() {
		return new ArrayList<>();
	}

	@Override
	public Collection<String> getHeaders(String arg0) {
		return new ArrayList<>();
	}

	@Override
	public int getStatus() {
		return this.status;
	}
	
	public void clear() {
		FileUtil.delete(output);
		
	}
}