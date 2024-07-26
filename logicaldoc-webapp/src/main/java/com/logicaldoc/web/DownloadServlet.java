package com.logicaldoc.web;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.hsqldb.lib.StringUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.authentication.InvalidSessionException;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.html.HTMLSanitizer;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible for document downloads. It searches for the
 * attribute docId in any scope and extracts the proper document's content.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class DownloadServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(DownloadServlet.class);

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

			if (request.getParameter("pluginId") != null)
				ServletUtil.downloadPluginResource(request, response, session.getSid(),
						request.getParameter("pluginId"), request.getParameter("resourcePath"),
						request.getParameter("fileName"));
			else
				downloadDocument(request, response, session);
		} catch (InvalidSessionException | IOException | ServletException | PersistenceException e) {
			if (e.getClass().getName().contains("ClientAbortException")) {
				log.debug(e.getMessage(), e);
			} else {
				log.error(e.getMessage(), e);
				ServletUtil.sendError(response, e.getMessage());
			}
		}
	}

	private void downloadDocument(HttpServletRequest request, HttpServletResponse response, Session session)
			throws IOException, ServletException, PersistenceException {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);

		// Flag indicating to download only indexed text
		String downloadText = request.getParameter("downloadText");

		Long docId = getDocId(request);

		Long versionId = getVersionId(request);

		String ver = request.getParameter("version");
		String fileVersion = request.getParameter("fileVersion");
		String suffix = request.getParameter("suffix");

		Document document = null;

		if (docId != null) {
			document = docDao.findById(docId);

			checkDownloadPermission(session, document);

			/*
			 * In case of alias to PDF, we have to serve the PDF conversion
			 */
			suffix = convertToPdf(session, downloadText, fileVersion, suffix, document);

			/*
			 * In case of alias we have to work on the real document
			 */
			document = getReferencedDocument(docDao, document);
		}

		Version version = null;
		if (versionId != null) {
			version = versDao.findById(versionId);
			if (document == null && version != null) {
				document = docDao.findDocument(version.getDocId());
				checkDownloadPermission(session, document);
			}
		}

		if (version == null && document != null && StringUtils.isNotEmpty(ver))
			version = versDao.findByVersion(document.getId(), ver);

		if (version == null && document != null && StringUtils.isNotEmpty(fileVersion))
			version = versDao.findByFileVersion(document.getId(), fileVersion);

		checkPasswordProtection(session, document);

		/*
		 * In case the client asks for a safe version of the HTML content
		 */
		processSafeHtml(suffix, version, document);

		String filename = getFilename(document, version);

		ServletUtil.setContentDisposition(request, response, filename);

		if (StringUtils.isEmpty(fileVersion)) {
			if (version != null)
				fileVersion = version.getFileVersion();
			else if (document != null)
				fileVersion = document.getFileVersion();
		}

		if (StringUtils.isEmpty(suffix)) {
			suffix = "";
		}

		download(request, response, session, downloadText, docId, fileVersion, filename, suffix, document);
	}

	private Document getReferencedDocument(DocumentDAO docDao, Document document) throws PersistenceException {
		if (document.getDocRef() != null)
			document = docDao.findById(document.getDocRef());
		return document;
	}

	private String getFilename(Document document, Version version) {
		String filename = "";
		if (version != null)
			filename = version.getFileName();
		else if (document != null)
			filename = document.getFileName();
		return filename;
	}

	private Long getVersionId(HttpServletRequest request) {
		Long versionId = null;
		try {
			if (request.getParameter("versionId") != null)
				versionId = Long.parseLong(request.getParameter("versionId"));
		} catch (NumberFormatException e) {
			log.warn("Invalid version ID: {}", versionId);
		}
		return versionId;
	}

	private Long getDocId(HttpServletRequest request) {
		Long docId = null;
		try {
			docId = Long.parseLong(request.getParameter("docId"));
		} catch (NumberFormatException e) {
			log.error("Invalid document ID: {}", docId);
		}
		return docId;
	}

	private void checkPasswordProtection(Session session, Document doc) throws IOException {
		if (doc != null && doc.isPasswordProtected() && !session.getUnprotectedDocs().containsKey(doc.getId()))
			throw new IOException("The document is protected by a password");
	}

	private String convertToPdf(Session session, String downloadText, String fileVersion, String suffix, Document doc) {
		if (doc.getDocRef() != null && StringUtil.isEmpty(downloadText)
				&& (doc.getDocRefType() != null && doc.getDocRefType().contains("pdf"))) {

			// Generate the PDF conversion
			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			try {
				manager.convertToPdf(doc, fileVersion, session.getSid());
			} catch (Exception e) {
				log.error("Cannot convert to PDF the document {}", doc);
			}

			suffix = FormatConverterManager.PDF_CONVERSION_SUFFIX;
		}
		return suffix;
	}

	private void checkDownloadPermission(Session session, Document doc) throws PersistenceException, IOException {
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (!documentDao.isDownloadAllowed(doc.getId(), session.getUserId()))
			throw new IOException("You don't have the DOWNLOAD permission");
	}

	private void download(HttpServletRequest request, HttpServletResponse response, Session session,
			String downloadText, Long docId, String fileVersion, String filename, String suffix, Document doc)
			throws IOException, PersistenceException, ServletException {
		if (doc != null) {
			if ("true".equals(downloadText)) {
				ServletUtil.downloadDocumentText(request, response, doc.getId(), session.getUser());
			} else {
				ServletUtil.downloadDocument(request, response, session.getSid(), doc.getId(), fileVersion, filename,
						suffix, session.getUser());
			}
		} else {
			throw new FileNotFoundException("Cannot find document " + docId);
		}
	}

	static void processSafeHtml(String suffix, Version version, Document doc) throws IOException {
		if ("safe.html".equals(suffix)) {
			Store store = (Store) Context.get().getBean(Store.class);
			if (doc != null) {
				String safeResource = store.getResourceName(doc,
						version == null ? doc.getFileVersion() : version.getFileVersion(), suffix);
				if (!store.exists(doc.getId(), safeResource)) {
					String unsafeResource = store.getResourceName(doc,
							version == null ? doc.getFileVersion() : version.getFileVersion(), null);
					String unsafe = store.getString(doc.getId(), unsafeResource);
					String safe = HTMLSanitizer.sanitize(unsafe);
					store.store(new ByteArrayInputStream(safe.getBytes(StandardCharsets.UTF_8)), doc.getId(),
							safeResource);
				}
			}
		}
	}
}