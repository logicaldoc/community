package com.logicaldoc.web;

import gwtupload.server.UploadAction;
import gwtupload.server.exceptions.UploadActionException;
import gwtupload.shared.UConsts;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for document uploads operations.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class UploadServlet extends UploadAction {

	private static final long serialVersionUID = 1L;

	public static String RECEIVEDFILES = "receivedFiles";

	public static String RECEIVEDCONTENTTYPES = "receivedContentTypes";

	public static String RECEIVEDFILENAMES = "receivedFileNames";

	protected static Logger log = LoggerFactory.getLogger(UploadServlet.class);

	/**
	 * Override executeAction to save the received files in a custom place and
	 * delete this items from session.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public String executeAction(HttpServletRequest request, List<FileItem> sessionFiles) throws UploadActionException {
		try {
			setUploadMax();

			HttpSession session = SessionManager.get().getServletSession(SessionManager.get().getSessionId(request));

			if (session == null)
				session = request.getSession();

			/**
			 * Maintain a list with received files and their content types
			 */
			Map<String, File> receivedFiles = (Map<String, File>) session.getAttribute(RECEIVEDFILES);
			if (receivedFiles == null) {
				receivedFiles = new Hashtable<String, File>();
				session.setAttribute(RECEIVEDFILES, receivedFiles);
			}

			Map<String, String> receivedContentTypes = (Map<String, String>) session.getAttribute(RECEIVEDCONTENTTYPES);
			if (receivedContentTypes == null) {
				receivedContentTypes = new Hashtable<String, String>();
				session.setAttribute(RECEIVEDCONTENTTYPES, receivedContentTypes);
			}

			Map<String, String> receivedFileNames = (Map<String, String>) session.getAttribute(RECEIVEDFILENAMES);
			if (receivedFileNames == null) {
				receivedFileNames = new Hashtable<String, String>();
				session.setAttribute(RECEIVEDFILENAMES, receivedFileNames);
			}

			String path = getServletContext().getRealPath("/upload/" + session.getId());
			File uploadFolder = new File(path);

			// Google App Engine doesn't support disk writing
			uploadFolder.mkdirs();
			uploadFolder.mkdir();

			String tenant = Tenant.DEFAULT_NAME;

			try {
				Session sess = ServiceUtil.validateSession(request);
				tenant = sess.getTenantName();
			} catch (Throwable t) {
				// ok no session, but was the security checks passed?
				if (request.getAttribute("__spring_security_session_mgmt_filter_applied")==null || !"true".equals(request.getAttribute("__spring_security_session_mgmt_filter_applied").toString()))
					throw t;
			}

			for (FileItem item : sessionFiles) {
				if (false == item.isFormField()) {
					if (!isAllowedForUpload(item.getName(), tenant))
						throw new UploadActionException("Invalid file name " + item.getName());

					OutputStream os = null;
					try {
						File file = new File(uploadFolder, item.getFieldName());

						os = new FileOutputStream(file);
						copyFromInputStreamToOutputStream(item.getInputStream(), os);

						receivedFiles.put(item.getFieldName(), file);
						receivedContentTypes.put(item.getFieldName(), item.getContentType());
						try {
							receivedFileNames.put(item.getFieldName(),
									URLDecoder.decode(FilenameUtils.getName(item.getName()), "UTF-8"));
						} catch (Throwable uee) {
							log.debug(uee.getMessage());
							receivedFileNames.put(item.getFieldName(), FilenameUtils.getName(item.getName()));
						}
					} catch (Throwable e) {
						log.warn(e.getMessage(), e);
						throw new UploadActionException(e.getMessage());
					} finally {
						if (os != null) {
							try {
								os.flush();
								os.close();
							} catch (IOException e) {
								log(e.getMessage());
							}
						}
					}
				}
			}
			removeSessionFileItems(request);
		} catch (UploadActionException e) {
			log.error(e.getMessage(), e);
			removeSessionFileItems(request);
			throw e;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
		return null;
	}

	/**
	 * The post method is used to receive the file and save it in the user
	 * session. It returns a very XML page that the client receives in an
	 * iframe.
	 * 
	 * The content of this xml document has a tag error in the case of error in
	 * the upload process or the string OK in the case of success.
	 * 
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException,
			ServletException {
		setUploadMax();
		super.doPost(request, response);
	}

	@Override
	protected String parsePostRequest(HttpServletRequest request, HttpServletResponse response) {
		setUploadMax();
		return super.parsePostRequest(request, response);
	}

	@Override
	public void checkRequest(HttpServletRequest request) {
		setUploadMax();

		if (super.maxFileSize > 0)
			super.checkRequest(request);
	}

	/**
	 * Remove a file when the user sends a delete request
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void removeItem(HttpServletRequest request, String fieldName) throws UploadActionException {
		HttpSession session = SessionManager.get().getServletSession(SessionManager.get().getSessionId(request));
		if (session == null)
			session = request.getSession();

		Map<String, File> receivedFiles = (Map<String, File>) session.getAttribute(RECEIVEDFILES);
		if (receivedFiles == null || !receivedFiles.containsKey(fieldName))
			return;

		File file = receivedFiles.get(fieldName);
		receivedFiles.remove(fieldName);
		if (file != null && file.exists())
			file.delete();

		Map<String, String> receivedContentTypes = (Map<String, String>) session.getAttribute(RECEIVEDCONTENTTYPES);
		if (receivedContentTypes == null || !receivedContentTypes.containsKey(fieldName))
			return;
		receivedContentTypes.remove(fieldName);

		Map<String, String> receivedFileNames = (Map<String, String>) session.getAttribute(RECEIVEDFILENAMES);
		if (receivedFileNames == null || !receivedFileNames.containsKey(fieldName))
			return;
		receivedFileNames.remove(fieldName);
	}

	/**
	 * Get the content of an uploaded file
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void getUploadedFile(HttpServletRequest request, HttpServletResponse response) throws IOException {
		setUploadMax();

		String fieldName = request.getParameter(UConsts.PARAM_SHOW);

		HttpSession session = SessionManager.get().getServletSession(SessionManager.get().getSessionId(request));

		if (session == null)
			session = request.getSession();

		for (FileItem item : getSessionFileItems(request)) {
			if (false == item.isFormField()) {
				Map<String, File> receivedFiles = (Map<String, File>) session.getAttribute(RECEIVEDFILES);

				if (receivedFiles == null || !receivedFiles.containsKey(fieldName))
					return;

				File f = receivedFiles.get(fieldName);
				if (f != null) {
					Map<String, String> receivedContentTypes = (Map<String, String>) session
							.getAttribute(RECEIVEDCONTENTTYPES);

					if (receivedContentTypes != null && receivedContentTypes.containsKey(fieldName))
						response.setContentType(receivedContentTypes.get(fieldName));
					FileInputStream is = new FileInputStream(f);
					copyFromInputStreamToOutputStream(is, response.getOutputStream());
				} else {
					renderXmlResponse(request, response, XML_ERROR_ITEM_NOT_FOUND);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	public static Map<String, File> getReceivedFiles(HttpServletRequest request, String sid) {
		HttpSession session = SessionManager.get().getServletSession(SessionManager.get().getSessionId(request));

		if (session == null)
			session = request.getSession();

		return (Map<String, File>) session.getAttribute(RECEIVEDFILES);
	}

	@SuppressWarnings("unchecked")
	public static Map<String, String> getReceivedFileNames(HttpServletRequest request, String sid) {
		HttpSession session = SessionManager.get().getServletSession(SessionManager.get().getSessionId(request));
		if (session == null)
			session = request.getSession();

		return (Map<String, String>) session.getAttribute(RECEIVEDFILENAMES);
	}

	protected void setUploadMax() {
		ContextProperties config = Context.get().getProperties();
		int maxUploadMB = 100;
		if (config.getProperty("upload.maxsize") != null)
			maxUploadMB = Integer.parseInt(config.getProperty("upload.maxsize"));

		if (maxUploadMB > 0)
			super.maxFileSize = maxUploadMB * 1024 * 1024;
		else
			super.maxFileSize = -1;
		super.maxSize = super.maxFileSize;
	}

	/**
	 * Checks if the passed filename can be uploaded or not on the basis of what
	 * configured in 'upload.disallow'.
	 */
	public static boolean isAllowedForUpload(String filename, String tenant) {
		ContextProperties config = Context.get().getProperties();
		String disallow = config.getProperty(tenant + ".upload.disallow");

		if (disallow == null || disallow.trim().isEmpty())
			return true;

		// Extract and normalize the extensions
		String[] disallowedExtensions = disallow.split(",");
		for (int i = 0; i < disallowedExtensions.length; i++) {
			disallowedExtensions[i] = disallowedExtensions[i].toLowerCase().trim();
			if (!disallowedExtensions[i].startsWith("."))
				disallowedExtensions[i] = "." + disallowedExtensions[i];
		}

		for (int i = 0; i < disallowedExtensions.length; i++)
			if (filename.toLowerCase().endsWith(disallowedExtensions[i]))
				return false;

		return true;
	}

	public static void cleanReceivedFiles(String sid) {
		HttpSession session = SessionManager.get().getServletSession(sid);
		cleanReceivedFiles(session);
	}

	public static void cleanReceivedFiles(HttpSession session) {
		if (session == null)
			return;
		try {
			session.setAttribute(RECEIVEDFILES, new Hashtable<String, File>());
			session.setAttribute(RECEIVEDCONTENTTYPES, new Hashtable<String, String>());
			session.setAttribute(RECEIVEDFILENAMES, new HashMap<String, String>());
			String path = session.getServletContext().getRealPath("/upload/" + session.getId());
			FileUtils.forceDelete(new File(path));
		} catch (IOException e) {
		}
	}
}