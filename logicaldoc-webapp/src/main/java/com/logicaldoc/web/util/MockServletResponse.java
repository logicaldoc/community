package com.logicaldoc.web.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.util.io.FileUtil;

/**
 * A servlet created for tests
 * 
 * @author Francesco Bignardi
 * @since 8.9
 */
public class MockServletResponse implements HttpServletResponse {

	private File output;

	private PrintWriter outputWriter;

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
		return null;
	}

	@Override
	public String getContentType() {
		return null;
	}

	@Override
	public Locale getLocale() {
		return null;
	}

	@Override
	public ServletOutputStream getOutputStream() throws IOException {
		return new ServletOutputStream() {
			private FileOutputStream fileOutputStream = new FileOutputStream(output);

			@Override
			public void write(int b) throws IOException {
				fileOutputStream.write(b);
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

	}

	@Override
	public void resetBuffer() {

	}

	@Override
	public void setBufferSize(int arg0) {

	}

	@Override
	public void setCharacterEncoding(String arg0) {

	}

	@Override
	public void setContentLength(int arg0) {

	}

	@Override
	public void setContentType(String arg0) {

	}

	@Override
	public void setLocale(Locale arg0) {

	}

	@Override
	public void addCookie(Cookie arg0) {

	}

	@Override
	public void addDateHeader(String arg0, long arg1) {

	}

	@Override
	public void addHeader(String arg0, String arg1) {

	}

	@Override
	public void addIntHeader(String arg0, int arg1) {

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

	}

	@Override
	public void sendError(int arg0, String arg1) throws IOException {

	}

	@Override
	public void sendRedirect(String arg0) throws IOException {

	}

	@Override
	public void setDateHeader(String arg0, long arg1) {

	}

	@Override
	public void setHeader(String arg0, String arg1) {

	}

	@Override
	public void setIntHeader(String arg0, int arg1) {

	}

	@Override
	public void setStatus(int arg0) {

	}

	@Override
	public void setStatus(int arg0, String arg1) {

	}

	public String getOutputString() throws IOException {
		if (output != null)
			return FileUtil.readFile(output);
		else
			return "";
	}
}
