package com.logicaldoc.webservice;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.MimeType;

/**
 * Some methods useful in webservice servlets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebserviceServletUtil {
	private static final String UTF_8 = "UTF-8";
	private static final int DEFAULT_BUFFER_SIZE = 10240; // ..bytes = 10KB.

	/**
	 * Sets the correct Content-Disposition header into the response
	 * 
	 * @param request the HTTP request
	 * @param response the server's response
	 * @param filename name of the file
	 * 
	 * @throws UnsupportedEncodingException error trying to encode the response
	 */
	public static void setContentDisposition(HttpServletRequest request, HttpServletResponse response, String filename)
			throws UnsupportedEncodingException {
		// Encode the filename
		String userAgent = request.getHeader("User-Agent").toLowerCase();

		String encodedFileName = null;
		if (userAgent.contains("msie") || userAgent.contains("opera")
				|| (userAgent.contains("trident") && userAgent.contains("windows"))
				|| (userAgent.contains("edge") && userAgent.contains("windows"))) {
			encodedFileName = URLEncoder.encode(filename, UTF_8);
			encodedFileName = encodedFileName.replace("+", "%20");
		} else if (userAgent.contains("safari") && !userAgent.contains("chrome")) {
			// Safari User-Agent contains "chrome"
			encodedFileName = filename;
		} else if (userAgent.contains("safari") && userAgent.contains("chrome") && userAgent.contains("android")) {
			// Used by some LG phones
			encodedFileName = filename;
		} else {
			encodedFileName = "=?UTF-8?B?" + new String(Base64.encodeBase64(filename.getBytes(UTF_8)), UTF_8)
					+ "?=";
		}

		boolean asAttachment = true;
		if (request.getParameter("open") != null)
			asAttachment = !"true".equals(request.getParameter("open"));
		else if (request.getAttribute("open") != null)
			asAttachment = !"true".equals(request.getAttribute("open"));

		response.setHeader("Content-Disposition",
				(asAttachment ? "attachment" : "inline") + "; filename=\"" + encodedFileName + "\"");

		// Headers required by Internet Explorer
		response.setHeader("Pragma", "public");
		response.setHeader("Cache-Control", "must-revalidate, post-check=0,pre-check=0");
		response.setHeader("Expires", "0");
	}

	/**
	 * Sends the specified file to the response object; the client will receive
	 * it as a download
	 * 
	 * Sends the specified file to the response object; the client will receive
	 * it as a download
	 * 
	 * @param request the current request
	 * @param response the file is written to this object
	 * @param file file to serve
	 * @param fileName client file name
	 * 
	 * @throws FileNotFoundException cannot find the file to download
	 * @throws IOException generic I/O error
	 * @throws ServletException error in the servlet container
	 */
	public static void downloadFile(HttpServletRequest request, HttpServletResponse response, File file,
			String fileName) throws FileNotFoundException, IOException, ServletException {

		String filename = fileName;
		if (filename == null)
			filename = file.getName();

		// get the mimetype
		String mimetype = MimeType.getByFilename(filename);
		// it seems everything is fine, so we can now start writing to the
		// response object
		response.setContentType(mimetype);
		setContentDisposition(request, response, filename);

		// Add this header for compatibility with internal .NET browsers
		response.setHeader("Content-Length", Long.toString(file.length()));

		try (InputStream is = new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE);
				OutputStream os = response.getOutputStream();) {
			IOUtils.copy(is, os);
		}
	}

	public static Session validateSession(HttpServletRequest request) throws ServletException {
		String sid = SessionManager.get().getSessionId(request);
		return validateSession(sid);
	}

	/**
	 * Throws a runtime exception id the given session is invalid
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return the session
	 * 
	 * @throws ServletException the session does not exist or is expired
	 */
	public static Session validateSession(String sid) throws ServletException {
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new ServletException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new ServletException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}
}