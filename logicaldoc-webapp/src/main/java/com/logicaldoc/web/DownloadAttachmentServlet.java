package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.bouncycastle.cms.CMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.store.Store;
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
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		try {
			Session session = ServletUtil.validateSession(request);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			long docId = Long.parseLong(request.getParameter("docId"));
			String fileVersion = request.getParameter("fileVersion");
			String filename = request.getParameter("attachmentFileName");

			Document doc = docDao.findById(docId);
			if (session.getUser() != null && !folderDao.isPermissionAllowed(Permission.DOWNLOAD,
					doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the DOWNLOAD permission");

			/*
			 * In case of alias we have to work on the real document
			 */
			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			if (doc != null && doc.isPasswordProtected() && !session.getUnprotectedDocs().containsKey(doc.getId()))
				throw new IOException("The document is protected by a password");

			download(request, response, docId, fileVersion, filename, doc);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	private void download(HttpServletRequest request, HttpServletResponse response, long docId, String fileVersion,
			String filename, Document doc) throws MessagingException, IOException, CMSException {
		Store store = (Store) Context.get().getBean(Store.class);
		String resource = store.getResourceName(docId, fileVersion, null);

		try (InputStream is = store.getStream(docId, resource)) {
			EMail email = null;

			if (doc != null && doc.getFileName().toLowerCase().endsWith(".eml"))
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

			File tmp = null;
			try {
				tmp = FileUtil.createTempFile("attdown", null);
				FileUtils.writeByteArrayToFile(tmp, attachment.getData());
				ServletUtil.downloadFile(request, response, tmp, filename);
			} finally {
				FileUtil.delete(tmp);
			}
		}
	}
}
