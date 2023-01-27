package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionListener;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.spring.LDSecurityContextRepository;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for document resource upload. It receives the
 * document resource and uploads it in LogicalDOC, inside the document's folder.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5
 */
public class UploadServlet extends HttpServlet implements SessionListener {

	public static final String receivedFiles = "receivedFiles";

	private static final long serialVersionUID =  1L;

	protected static Logger log = LoggerFactory.getLogger(UploadServlet.class);

	/**
	 * Sets the size threshold (in bytes) beyond which files are written
	 * directly to disk.
	 */
	private static int maxMemSize = 1 * 1024 * 1024;

	/**
	 * Constructor of the object.
	 */
	public UploadServlet() {
		super();
		SessionManager.get().addListener(this);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		try {
			response.setContentType("text/html");
			PrintWriter out = response.getWriter();
			out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
			out.println("<HTML>");
			out.println("  <HEAD><TITLE>Upload Document Resource Servlet</TITLE></HEAD>");
			out.println("  <BODY>");
			out.print(" This servlet doesn't support GET method. Use POST instead. ");
			out.println("  </BODY>");
			out.println("</HTML>");
			out.flush();
			out.close();
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	/**
	 * Retrieves the root of the upload folder
	 * 
	 * @param sid The current session ID
	 * 
	 * @return the upload folder
	 */
	public static File getUploadDir(String sid) {
		File dir = new File(System.getProperty("java.io.tmpdir") + "/upload/" + sid);
		dir.mkdirs();
		dir.mkdir();
		return dir;
	}

	/**
	 * Retrieves the root of the upload folder
	 * 
	 * @param httpSession The current session
	 * 
	 * @return the upload folder
	 */
	public static File getUploadDir(HttpSession httpSession) {
		File dir = new File(System.getProperty("java.io.tmpdir") + "/upload/" + httpSession.getId());
		dir.mkdirs();
		dir.mkdir();
		return dir;
	}

	/**
	 * Retrieves the map containing the files uploaded in the current session
	 * 
	 * @param sid The current session ID
	 * 
	 * @return the files map where the key is the original file name and the
	 *         value is the real file
	 */
	public static Map<String, File> getReceivedFiles(String sid) {
		HttpSession session = SessionManager.get().getServletSession(sid);
		if (session == null)
			return null;
		return getReceivedFiles(session);
	}

	/**
	 * Retrieves the map containing the files uploaded in the current session
	 * 
	 * @param httpSession The current session
	 * 
	 * @return the files map where the key is the original file name and the
	 *         value is the real file
	 */
	public static Map<String, File> getReceivedFiles(HttpSession httpSession) {
		@SuppressWarnings("unchecked")
		Map<String, File> uploadedFiles = (Map<String, File>) httpSession.getAttribute(receivedFiles);
		if (uploadedFiles == null) {
			uploadedFiles = new HashMap<String, File>();
			httpSession.setAttribute(receivedFiles, uploadedFiles);
		}
		return uploadedFiles;
	}

	/**
	 * Removes all the uploaded files from session and file system.
	 * 
	 * @param sid The current session ID
	 */
	public static void cleanReceivedFiles(String sid) {
		FileUtil.strongDelete(getUploadDir(sid));
		Map<String, File> uploadedFiles = getReceivedFiles(sid);
		if (uploadedFiles != null)
			uploadedFiles.clear();
	}

	/**
	 * Removes all the uploaded files from session and file system.
	 * 
	 * @param httpSession The current session
	 */
	public static void cleanReceivedFiles(HttpSession httpSession) {
		FileUtil.strongDelete(getUploadDir(httpSession));
		if (getReceivedFiles(httpSession) != null)
			getReceivedFiles(httpSession).clear();
	}

	protected String getSid(HttpServletRequest request) {
		String sid = SessionManager.get().getSessionId(request);
		HttpSession session = SessionManager.get().getServletSession(sid);

		if (session == null) {
			// No SID already associated to the current session, so do it
			session = request.getSession();
			if (sid != null)
				LDSecurityContextRepository.bindServletSession(sid, request);
		}

		return sid;
	}

	protected void checkIfAllowedForUpload(String tenant, String fileName) throws ServletException {
		String extension = FileUtil.getExtension(fileName).toLowerCase();
		String disallow = Context.get().getProperties().getString(tenant + ".upload.disallow");
		if (disallow != null && !disallow.isEmpty()) {
			disallow = disallow.toLowerCase().replace(" ", "");
			List<String> disallowedExtensions = Arrays.asList(disallow.toLowerCase().split(","));
			if (disallowedExtensions.contains(extension))
				throw new ServletException("Disallowed extension: " + extension);
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) {

		try {
			String tenant = "default";
			String sid = getSid(request);
			
			if (StringUtils.isEmpty(sid)) {
				sid = "tmp";
			} else {
				tenant = SessionManager.get().get(sid).getTenantName();
			}
			File uploadDir = getUploadDir(sid);

			// Check that we have a file upload request
			boolean isMultipart = ServletFileUpload.isMultipartContent(request);
			response.setContentType("text/html");
			java.io.PrintWriter out = response.getWriter();

			if (!isMultipart) {
				out.println("<html>");
				out.println("<head>");
				out.println("<title>Upload</title>");
				out.println("</head>");
				out.println("<body>");
				out.println("<p>No file uploaded</p>");
				out.println("</body>");
				out.println("</html>");
				return;
			}

			DiskFileItemFactory factory = new DiskFileItemFactory();

			// maximum size that will be stored in memory (in bytes)
			factory.setSizeThreshold(maxMemSize);

			// Location to save data that is larger than maxMemSize.
			factory.setRepository(uploadDir);

			// Create a new file upload handler
			ServletFileUpload upload = new ServletFileUpload(factory);

			// maximum file size to be uploaded (in bytes)
			upload.setFileSizeMax(Context.get().getProperties().getLong(tenant + ".upload.maxsize", 10L) * 1024 * 1024);

			Map<String, File> uploadedFiles = "tmp".equals(sid) ? getReceivedFiles(request.getSession(true)) : getReceivedFiles(sid);

			// Parse the request to get uploaded file items.
			List<FileItem> fileItems = upload.parseRequest(request);

			out.println("<html>");
			out.println("<head>");
			out.println("<title>Upload</title>");
			out.println("</head>");
			out.println("<body>");

			for (FileItem fi : fileItems) {
				if (!fi.isFormField()) {
					String fileName = fi.getName();

					checkIfAllowedForUpload(tenant, fileName);

					File file = new File(uploadDir.getAbsolutePath() + "/" + UUID.randomUUID().toString() + "."
							+ FileUtil.getExtension(fileName));
					fi.write(file);
					if (uploadedFiles != null)
						uploadedFiles.put(fileName, file);
					out.println("Uploaded Filename: " + fileName + "<br>");
				}
			}
			out.println("</body>");
			out.println("</html>");
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			try {
				response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
			} catch (IOException ioe) {
				// Nothing to do
			}
		}

	}

	@Override
	public void onSessionCreated(Session session) {
		// Nothing to do
	}

	@Override
	public void onSessionClosed(Object sid) {
		try {
			FileUtils.forceDelete(getUploadDir(sid.toString()));
		} catch (Throwable e) {
			log.warn("Unable to clean the upload folder for session {}", sid);
		}
	}
}