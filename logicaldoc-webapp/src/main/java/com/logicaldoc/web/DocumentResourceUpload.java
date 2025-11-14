package com.logicaldoc.web;

import java.io.IOException;
import java.io.PrintWriter;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for document resource upload. It receives the
 * document resource and uploads it in LogicalDOC, inside the document's folder.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5
 */
public class DocumentResourceUpload extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(DocumentResourceUpload.class);

	public static final String DOC_ID = "docId";

	public static final String SUFFIX = "suffix";

	// The file version
	public static final String VERSION_ID = "versionId";

	// The document version
	public static final String VERSION_DOC = "versionDoc";

	/**
	 * Constructor of the object.
	 */
	public DocumentResourceUpload() {
		super();
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
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		try {
			Session session = ServletUtil.validateSession(request);

			UserDAO udao = UserDAO.get();

			// Load the user associated to the session
			User user = udao.findByUsername(session.getUsername());
			if (user == null)
				return;

			long docId = Long.parseLong(request.getParameter(DOC_ID));

			String suffix = request.getParameter(SUFFIX);

			String fileVersion = request.getParameter(VERSION_ID);

			String docVersion = request.getParameter(VERSION_DOC);

			log.debug("Start Upload resource for document {}", docId);

			FolderDAO fdao = FolderDAO.get();

			DocumentDAO docDao = DocumentDAO.get();

			Document doc = docDao.findById(docId);
			Folder folder = doc.getFolder();
			if (fdao.isPermissionAllowed(Permission.SIGN, folder.getId(), user.getId())) {
				ServletUtil.uploadDocumentResource(request, docId, suffix, fileVersion, docVersion);
				if (suffix.startsWith("sign")) {
					docDao.initialize(doc);
					doc.setSigned(true);
					docDao.store(doc);
					VersionDAO vdao = VersionDAO.get();
					Version version = null;
					if (StringUtils.isNotEmpty(docVersion))
						version = vdao.findByVersion(doc.getId(), docVersion);
					else
						version = vdao.findByVersion(doc.getId(), doc.getVersion());
					vdao.initialize(version);
					version.setSigned(true);
					vdao.store(version);
				}
			}
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		}
	}
}