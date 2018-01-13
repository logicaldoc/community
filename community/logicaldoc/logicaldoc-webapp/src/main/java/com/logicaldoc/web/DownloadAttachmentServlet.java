package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.util.MailUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible for the download of email attachments
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.4
 */
public class DownloadAttachmentServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(DownloadAttachmentServlet.class);

	/**
	 * The doGet method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to get.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		Session session = ServletUtil.validateSession(request);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		long docId = Long.parseLong(request.getParameter("docId"));
		String fileVersion = request.getParameter("fileVersion");
		String filename = request.getParameter("attachmentFileName");

		Version version = null;
		Document doc = docDao.findById(docId);
		if (session.getUser() != null
				&& !folderDao.isPermissionEnabled(Permission.DOWNLOAD, doc.getFolder().getId(), session.getUserId()))
			throw new IOException("You don't have the DOWNLOAD permission");

		/*
		 * In case of alias we have to work on the real document
		 */
		if (doc.getDocRef() != null)
			doc = docDao.findById(doc.getDocRef());

		if (version == null && doc != null && StringUtils.isNotEmpty(fileVersion))
			version = versDao.findByFileVersion(doc.getId(), fileVersion);

		if (doc.isPasswordProtected() && !session.getUnprotectedDocs().containsKey(doc.getId()))
			throw new IOException("The document is protected by a password");

		InputStream is = null;
		File tmp = File.createTempFile("attdown", null);
		try {
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(docId, fileVersion, null);
			is = storer.getStream(docId, resource);

			EMail email = null;

			if (doc.getFileName().toLowerCase().endsWith(".eml"))
				email = MailUtil.messageToMail(is, true);
			else
				email = MailUtil.msgToMail(is, true);

			EMailAttachment attachment = null;
			if (email.getAttachments().size() > 0)
				for (EMailAttachment att : email.getAttachments().values()) {
					if (filename.equals(att.getFileName())) {
						attachment = att;
						break;
					}
				}
			if (attachment == null || attachment.getData() == null)
				throw new IOException("Attachment not found");

			FileUtils.writeByteArrayToFile(tmp, attachment.getData());
			ServletUtil.downloadFile(request, response, tmp, filename);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new IOException(t.getMessage());
		} finally {
			IOUtils.closeQuietly(is);
			FileUtil.strongDelete(tmp);
		}
	}
}
