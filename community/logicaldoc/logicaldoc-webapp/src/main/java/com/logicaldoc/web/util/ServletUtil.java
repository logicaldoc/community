package com.logicaldoc.web.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.hsqldb.lib.StringUtil;

import com.ibm.icu.util.Calendar;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.HistoryDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Some helper utilities to download/upload a document and its resources. The
 * downloaded document is also added to the recent files of the user.
 * 
 * @author Sebastian Stein
 */
public class ServletUtil {
	private static final int DEFAULT_BUFFER_SIZE = 10240; // ..bytes = 10KB.

	private static final String MULTIPART_BOUNDARY = "MULTIPART_BYTERANGES";

	private static Set<String> localAddresses = null;

	public static Session validateSession(HttpServletRequest request) throws ServletException {
		try {
			String sid = SessionManager.get().getSessionId(request);
			return ServiceUtil.validateSession(sid);
		} catch (Throwable t) {
			throw new ServletException(t.getMessage());
		}
	}

	public static Session checkMenu(HttpServletRequest request, long menuId) throws ServletException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!dao.isReadEnable(menuId, session.getUserId())) {
			String message = "User " + session.getUsername() + " cannot access the menu " + menuId;
			throw new ServletException(message);
		}
		return session;
	}

	/**
	 * Checks if a specific menu is accessible by the user in the current
	 * session
	 */
	public static Session checkEvenOneMenu(HttpServletRequest request, long... menuIds) throws ServletException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		for (long menuId : menuIds) {
			if (dao.isReadEnable(menuId, session.getUserId()))
				return session;
		}

		String message = "User " + session.getUsername() + " cannot access the menues " + Arrays.asList(menuIds);
		throw new ServletException(message);
	}

	/**
	 * Downloads a plugin resource
	 * 
	 * @param request
	 * @param response
	 * @param pluginName name of the plug-in
	 * @param resourcePath Relative path ot the plug-in's resource
	 * @param fileName Optional file name
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws ServletException
	 * @throws InvalidSessionException
	 */
	public static void downloadPluginResource(HttpServletRequest request, HttpServletResponse response, String sid,
			String pluginName, String resourcePath, String fileName) throws FileNotFoundException, IOException,
			ServletException, InvalidSessionException {

		if (sid != null)
			try {
				ServiceUtil.validateSession(sid);
			} catch (ServerException e) {
				throw new ServletException(e.getMessage(), e);
			}
		else
			ServiceUtil.validateSession(request);

		String filename = fileName;
		if (filename == null)
			filename = FilenameUtils.getName(resourcePath);

		File file = PluginRegistry.getPluginResource(pluginName, resourcePath);

		// get the mimetype
		String mimetype = MimeType.getByFilename(filename);
		// it seems everything is fine, so we can now start writing to the
		// response object
		response.setContentType(mimetype);
		setContentDisposition(request, response, filename);

		// Add this header for compatibility with internal .NET browsers
		response.setHeader("Content-Length", Long.toString(file.length()));

		InputStream is = null;
		OutputStream os = null;

		try {
			is = new FileInputStream(file);
			os = response.getOutputStream();

			int letter = 0;

			byte[] buffer = new byte[128 * 1024];
			while ((letter = is.read(buffer)) != -1) {
				os.write(buffer, 0, letter);
			}
		} finally {
			os.flush();
			os.close();
			is.close();
		}

	}

	/**
	 * Sends the specified document to the response object; the client will
	 * receive it as a download
	 * 
	 * @param request the current request
	 * @param response the document is written to this object
	 * @param sid Session identifier, if not provided the request parameter is
	 *        inspected
	 * @param docId Id of the document
	 * @param fileVersion name of the file version; if null the latest version
	 *        will be returned
	 * @param suffix of the linked document's resource
	 * @throws ServletException
	 */
	public static void downloadDocument(HttpServletRequest request, HttpServletResponse response, String sid,
			long docId, String fileVersion, String fileName, String suffix, User user) throws FileNotFoundException,
			IOException, ServletException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		UserDAO udao = (UserDAO) Context.get().getBean(UserDAO.class);
		ContextProperties config = Context.get().getProperties();

		Session session = null;
		if (sid != null)
			try {
				session = ServiceUtil.validateSession(sid);
			} catch (ServerException e) {
				throw new ServletException(e.getMessage(), e);
			}
		else
			try {
				session = ServiceUtil.validateSession(request);
			} catch (Throwable t) {

			}

		if (user != null)
			try {
				udao.initialize(user);
			} catch (Throwable t) {

			}

		Document doc = dao.findById(docId);

		if (doc != null && user != null && !user.isMemberOf("admin") && !user.isMemberOf("publisher")
				&& !doc.isPublishing())
			throw new FileNotFoundException("Document not published");

		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(doc, fileVersion, null);

		String filename = fileName;
		if (filename == null)
			filename = doc.getFileName();

		if (StringUtils.isNotEmpty(suffix) && !suffix.endsWith(".p7m") && !suffix.endsWith(".m7m"))
			filename = FilenameUtils.getBaseName(filename);

		if (!storer.exists(doc.getId(), resource)) {
			throw new FileNotFoundException(resource);
		}

		if (StringUtils.isNotEmpty(suffix)) {
			resource = storer.getResourceName(doc, fileVersion, suffix);
			filename = filename + "." + FilenameUtils.getExtension(suffix);
		}

		long length = storer.size(doc.getId(), resource);
		String contentType = MimeType.getByFilename(filename);
		long lastModified = doc.getDate().getTime();
		String eTag = doc.getId() + "_" + doc.getVersion() + "_" + lastModified;
		boolean acceptsGzip = false;

		String acceptEncoding = request.getHeader("Accept-Encoding");
		acceptsGzip = acceptEncoding != null && accepts(acceptEncoding, "gzip");
		acceptsGzip = acceptsGzip && "true".equals(config.getProperty("download.gzip"));

		// Don't compress if we have to serve a thumbnail
		acceptsGzip = acceptsGzip
				&& (StringUtil.isEmpty(suffix) || (!"thumb.jpg".endsWith(suffix) && !"tile.jpg".endsWith(suffix)));

		response.setContentType(contentType);
		response.setHeader("Content-Length", Long.toString(length));
		setContentDisposition(request, response, filename);

		// Prepare some variables. The full Range represents the complete file.
		Range full = new Range(0, length - 1, length);
		List<Range> ranges = new ArrayList<Range>();

		// Validate and process Range and If-Range headers.
		String range = request.getHeader("Range");
		if (range != null) {

			// Range header should match format "bytes=n-n,n-n,n-n...". If not,
			// then return 416.
			if (!range.matches("^bytes=\\d*-\\d*(,\\d*-\\d*)*$")) {
				response.setHeader("Content-Range", "bytes */" + length); // Required
																			// in
																			// 416.
				response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
				return;
			}

			// If-Range header should either match ETag or be greater then
			// LastModified. If not,
			// then return full file.
			String ifRange = request.getHeader("If-Range");
			if (ifRange != null && !ifRange.equals(eTag)) {
				try {
					long ifRangeTime = request.getDateHeader("If-Range"); // Throws
																			// IAE
																			// if
																			// invalid.
					if (ifRangeTime != -1 && ifRangeTime + 1000 < lastModified) {
						ranges.add(full);
					}
				} catch (IllegalArgumentException ignore) {
					ranges.add(full);
				}
			}

			// If any valid If-Range header, then process each part of byte
			// range.
			if (ranges.isEmpty()) {
				for (String part : range.substring(6).split(",")) {
					// Assuming a file with length of 100, the following
					// examples returns bytes at:
					// 50-80 (50 to 80), 40- (40 to length=100), -20
					// (length-20=80 to length=100).
					long start = sublong(part, 0, part.indexOf("-"));
					long end = sublong(part, part.indexOf("-") + 1, part.length());

					if (start == -1) {
						start = length - end;
						end = length - 1;
					} else if (end == -1 || end > length - 1) {
						end = length - 1;
					}

					// Check if Range is syntactically valid. If not, then
					// return 416.
					if (start > end) {
						response.setHeader("Content-Range", "bytes */" + length); // Required
																					// in
																					// 416.
						response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
						return;
					}

					// Add range.
					ranges.add(new Range(start, end, length));
				}
			}
		} else {
			ranges.add(full);
		}

		response.setBufferSize(DEFAULT_BUFFER_SIZE);
		response.setHeader("Accept-Ranges", "bytes");
		response.setHeader("ETag", eTag);
		response.setDateHeader("Last-Modified", lastModified);
		response.setHeader("Cache-Control", "no-cache, no-store, must-revalidate"); // HTTP
																					// 1.1.
		response.setHeader("Pragma", "no-cache"); // HTTP 1.0.
		response.setDateHeader("Expires", 0); // Proxies.

		// Send requested file (part(s)) to client
		// ------------------------------------------------

		// Prepare streams.
		OutputStream output = null;

		try {
			// Open streams.
			output = response.getOutputStream();

			if (ranges.isEmpty() || ranges.get(0) == full || ranges.get(0).length == length) {
				// Return full file.
				Range r = full;
				response.setHeader("Content-Range", "bytes " + r.start + "-" + r.end + "/" + r.total);

				if (acceptsGzip) {
					// The browser accepts GZIP, so GZIP the content.
					response.setHeader("Content-Encoding", "gzip");
					output = new GZIPOutputStream(output, DEFAULT_BUFFER_SIZE);
				} else {
					// Content length is not directly predictable in case of
					// GZIP.
					// So only add it if there is no means of GZIP, else browser
					// will hang.
					response.setHeader("Content-Length", String.valueOf(r.length));
				}

				// Copy full range
				InputStream is = storer.getStream(docId, resource);
				try {
					if (is != null && output != null) {
						IOUtils.copy(is, output, DEFAULT_BUFFER_SIZE);
					}
				} finally {
					if (is != null)
						IOUtils.closeQuietly(is);
				}
			} else if (ranges.size() == 1) {
				// Return single part of file.
				Range r = ranges.get(0);
				response.setHeader("Content-Range", "bytes " + r.start + "-" + r.end + "/" + r.total);
				response.setHeader("Content-Length", String.valueOf(r.length));
				response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT); // 206.

				// Copy single part range.
				storer.writeToStream(docId, resource, output, r.start, r.length);
			} else {
				// Return multiple parts of file.
				response.setContentType("multipart/byteranges; boundary=" + MULTIPART_BOUNDARY);
				response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT); // 206.

				// Cast back to ServletOutputStream to get the easy println
				// methods.
				ServletOutputStream sos = (ServletOutputStream) output;

				// Copy multi part range.
				for (Range r : ranges) {
					// Add multipart boundary and header fields for every
					// range.
					sos.println();
					sos.println("--" + MULTIPART_BOUNDARY);
					sos.println("Content-Type: " + contentType);
					sos.println("Content-Range: bytes " + r.start + "-" + r.end + "/" + r.total);

					// Copy single part range of multi part range.
					storer.writeToStream(docId, resource, sos, r.start, r.length);

					// End with multipart boundary.
					sos.println();
					sos.println("--" + MULTIPART_BOUNDARY + "--");
				}
			}
		} finally {
			// Gently close streams.
			IOUtils.closeQuietly(output);
		}

		/*
		 * Save an history only if it is requested the first fragment
		 */
		boolean saveHistory = StringUtils.isEmpty(suffix)
				|| ("conversion.pdf".equals(suffix) && "preview".equals(request.getParameter("control")));
		if (!ranges.isEmpty() && saveHistory) {
			saveHistory = false;
			for (Range rng : ranges)
				if (rng.start == 0) {
					saveHistory = true;
					break;
				}
		}
		saveHistory = saveHistory && (user != null);

		if (saveHistory) {
			// Add an history entry to track the download of the document
			History history = new History();
			history.setDocId(doc.getId());
			history.setVersion(doc.getVersion());
			history.setFilename(doc.getFileName());
			history.setFolderId(doc.getFolder().getId());
			history.setUser(user);
			if (session != null){
				history.setSession(session);
			}else {
				history.setSessionId(sid);
			}

			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			history.setPath(fdao.computePathExtended(doc.getFolder().getId()));
			if ("preview".equals(request.getParameter("control")))
				history.setEvent(DocumentEvent.VIEWED.toString());
			else
				history.setEvent(DocumentEvent.DOWNLOADED.toString());

			/*
			 * Avoid to save frequent views of this document in the same
			 * session. So we will not save if there is another view in the same
			 * session asked since 30 seconds.
			 */
			HistoryDAO hdao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
			List<History> oldHistories = hdao.findByUserIdAndEvent(user.getId(), history.getEvent(), session.getSid());
			Calendar cal = Calendar.getInstance();
			cal.setTime(history.getDate());
			cal.add(Calendar.SECOND, -30);
			Date oldestDate = cal.getTime();

			History latestHistory = null;
			Date latestDate = null;
			if (!oldHistories.isEmpty()) {
				latestHistory = oldHistories.get(oldHistories.size() - 1);
				cal.setTime(latestHistory.getDate());
				latestDate = cal.getTime();
			}

			if (latestHistory == null || oldestDate.getTime() > latestDate.getTime()
					|| !latestHistory.getDocId().equals(history.getDocId())) {
				hdao.store(history);
			}
		}
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
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws ServletException
	 */
	public static void downloadFile(HttpServletRequest request, HttpServletResponse response, File file, String fileName)
			throws FileNotFoundException, IOException, ServletException {

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

		InputStream is = null;
		OutputStream os = null;

		try {
			is = new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE);
			os = response.getOutputStream();

			IOUtils.copy(is, os);
		} finally {
			// Gently close streams.
			IOUtils.closeQuietly(is);
			IOUtils.closeQuietly(os);
		}
	}

	/**
	 * Sets the correct Content-Disposition header into the response
	 */
	public static void setContentDisposition(HttpServletRequest request, HttpServletResponse response, String filename)
			throws UnsupportedEncodingException {
		// Encode the filename
		String userAgent = request.getHeader("User-Agent").toLowerCase();

		String encodedFileName = null;
		if (userAgent.contains("msie") || userAgent.contains("opera")
				|| (userAgent.contains("trident") && userAgent.contains("windows"))
				|| (userAgent.contains("edge") && userAgent.contains("windows"))) {
			encodedFileName = URLEncoder.encode(filename, "UTF-8");
			encodedFileName = encodedFileName.replace("+", "%20");
		} else if (userAgent.contains("safari") && !userAgent.contains("chrome")) {
			// Safari User-Agent contains "chrome"
			encodedFileName = filename;
		} else if (userAgent.contains("safari") && userAgent.contains("chrome") && userAgent.contains("android")) {
			// Used by some LG phones
			encodedFileName = filename;
		} else {
			encodedFileName = "=?UTF-8?B?" + new String(Base64.encodeBase64(filename.getBytes("UTF-8")), "UTF-8")
					+ "?=";
		}

		boolean asAttachment = request.getParameter("open") == null;
		response.setHeader("Content-Disposition", (asAttachment ? "attachment" : "inline") + "; filename=\""
				+ encodedFileName + "\"");

		// Headers required by Internet Explorer
		response.setHeader("Pragma", "public");
		response.setHeader("Cache-Control", "must-revalidate, post-check=0,pre-check=0");
		response.setHeader("Expires", "0");
	}

	/**
	 * Sends the specified document's indexed text to the response object; the
	 * client will receive it as a download
	 * 
	 * @param request the current request
	 * @param response the document is written to this object
	 * @param docId Id of the document
	 * @param version name of the version; if null the latest version will
	 *        returned
	 */
	public static void downloadDocumentText(HttpServletRequest request, HttpServletResponse response, long docId,
			User user) throws FileNotFoundException, IOException {

		response.setCharacterEncoding("UTF-8");

		// get document
		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = ddao.findById(docId);

		if (doc == null) {
			throw new FileNotFoundException();
		}

		if (doc.getDocRef() != null) {
			doc = ddao.findById(doc.getDocRef());
		}

		String mimetype = "text/plain";

		// it seems everything is fine, so we can now start writing to the
		// response object
		response.setContentType(mimetype);

		setContentDisposition(request, response, doc.getFileName() + ".txt");

		UserDAO udao = (UserDAO) Context.get().getBean(UserDAO.class);
		udao.initialize(user);
		if (doc != null && !user.isMemberOf("admin") && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new FileNotFoundException("Document not published");

		SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);

		String content = indexer.getHit(docId).getContent();
		if (content == null)
			content = "";

		try {
			response.getOutputStream().write(content.getBytes(Charset.forName("UTF-8")));
		} finally {
			response.getOutputStream().flush();
			response.getOutputStream().close();
		}
	}

	/**
	 * Sends the specified document to the response object; the client will
	 * receive it as a download
	 * 
	 * @param request the current request
	 * @param response the document is written to this object
	 * @param docId Id of the document
	 * @param fileVersion name of the file version; if null the latest version
	 *        will be returned
	 * @throws ServletException
	 * @throws NumberFormatException
	 */
	public static void downloadDocument(HttpServletRequest request, HttpServletResponse response, String sid,
			String docId, String fileVersion, String fileName, User user) throws FileNotFoundException, IOException,
			NumberFormatException, ServletException {
		downloadDocument(request, response, sid, Integer.parseInt(docId), fileVersion, fileName, null, user);
	}

	/**
	 * Uploads a document's related resource.
	 * 
	 * The resource will be stored in the folder where the document's files
	 * reside using the following pattern: <b>fileVersion</b>-<b>suffix</b>
	 * 
	 * If no version is specified, the current one is used instead
	 * 
	 * @param request the current request
	 * @param docId Id of the document
	 * @param suffix Suffix of the document
	 * @param fileVersion id of the file version; if null the latest version
	 *        will returned
	 * @param docVersion id of the doc version; if null the latest version will
	 *        returned
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static void uploadDocumentResource(HttpServletRequest request, String docId, String suffix,
			String fileVersion, String docVersion) throws Exception {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(Long.parseLong(docId));

		String ver = docVersion;
		if (StringUtils.isEmpty(ver))
			ver = fileVersion;
		if (StringUtils.isEmpty(ver))
			ver = doc.getFileVersion();

		Storer storer = (Storer) Context.get().getBean(Storer.class);

		DiskFileItemFactory factory = new DiskFileItemFactory();
		// Configure the factory here, if desired.
		ServletFileUpload upload = new ServletFileUpload(factory);
		// Configure the uploader here, if desired.
		List<FileItem> fileItems = upload.parseRequest(request);
		for (FileItem item : fileItems) {
			if (!item.isFormField()) {
				File savedFile = File.createTempFile("", "");
				item.write(savedFile);

				InputStream is = null;
				try {
					is = item.getInputStream();
					storer.store(item.getInputStream(), Long.parseLong(docId), storer.getResourceName(doc, ver, suffix));
				} finally {
					if (is != null)
						is.close();
					FileUtils.forceDelete(savedFile);
				}
			}
		}
	}

	/**
	 * Returns true if the given accept header accepts the given value.
	 * 
	 * @param acceptHeader The accept header.
	 * @param toAccept The value to be accepted.
	 * @return True if the given accept header accepts the given value.
	 */
	private static boolean accepts(String acceptHeader, String toAccept) {
		String[] acceptValues = acceptHeader.split("\\s*(,|;)\\s*");
		Arrays.sort(acceptValues);
		return Arrays.binarySearch(acceptValues, toAccept) > -1
				|| Arrays.binarySearch(acceptValues, toAccept.replaceAll("/.*$", "/*")) > -1
				|| Arrays.binarySearch(acceptValues, "*/*") > -1;
	}

	/**
	 * Returns a substring of the given string value from the given begin index
	 * to the given end index as a long. If the substring is empty, then -1 will
	 * be returned
	 * 
	 * @param value The string value to return a substring as long for.
	 * @param beginIndex The begin index of the substring to be returned as
	 *        long.
	 * @param endIndex The end index of the substring to be returned as long.
	 * @return A substring of the given string value as long or -1 if substring
	 *         is empty.
	 */
	private static long sublong(String value, int beginIndex, int endIndex) {
		String substring = value.substring(beginIndex, endIndex);
		return (substring.length() > 0) ? Long.parseLong(substring) : -1;
	}

	/**
	 * This class represents a byte range.
	 */
	protected static class Range {
		long start;

		long end;

		long length;

		long total;

		/**
		 * Construct a byte range.
		 * 
		 * @param start Start of the byte range.
		 * @param end End of the byte range.
		 * @param total Total length of the byte source.
		 */
		public Range(long start, long end, long total) {
			this.start = start;
			this.end = end;
			this.length = end - start + 1;
			this.total = total;
		}
	}
}