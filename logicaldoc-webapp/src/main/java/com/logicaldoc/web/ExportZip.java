package com.logicaldoc.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.transfer.ZipExport;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible of zip export
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ExportZip extends HttpServlet {
	private static final String DOC_ID = "docId";

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(ExportZip.class);

	/**
	 * Constructor of the object.
	 */
	public ExportZip() {
		super();
	}

    @Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			Session session = ServletUtil.validateSession(request);

			FolderDAO folderDao = Context.get(FolderDAO.class);

			String folderId = request.getParameter("folderId");
			if (folderId != null) {
				Folder folder = folderDao.findFolder(Long.parseLong(folderId));
				folderId = "" + folder.getId();
			}

			String level = getLevel(request);

			ArrayList<Long> docIds = getDocIds(request, session.getUserId());

			ZipExport exporter = new ZipExport();

			if (level.equals("all")) {
				exporter.setAllLevel(true);
			}

			try (ByteArrayOutputStream bos = getZipFileOutputStream(session, folderId, docIds, exporter)) {
				String exportName = getExportName(folderId);

				response.setContentType("application/zip");
				response.setContentLength(bos.size());
				response.setHeader("Content-Disposition", "attachment; filename=\"" + exportName + ".zip\"");

				// Headers required by MS Internet Explorer
				response.setHeader("Pragma", "public");
				response.setHeader("Cache-Control", "must-revalidate, post-check=0,pre-check=0");
				response.setHeader("Expires", "0");

				try (OutputStream os = response.getOutputStream()) {
					bos.flush();
					os.write(bos.toByteArray());
					os.flush();
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	private ByteArrayOutputStream getZipFileOutputStream(Session session, String folderId, ArrayList<Long> docIds,
			ZipExport exporter) throws PersistenceException {
		ByteArrayOutputStream bos = null;

		if (CollectionUtils.isNotEmpty(docIds)) {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(session.getSid());
			transaction.setEvent(DocumentEvent.DOWNLOADED.toString());
			transaction.setSession(session);

			bos = exporter.process(docIds.toArray(new Long[0]), false, transaction);
		} else {
			FolderHistory transaction = new FolderHistory();
			transaction.setFolderId(Long.parseLong(folderId));
			transaction.setSession(session);

			bos = exporter.process(transaction, false);
		}
		return bos;
	}

	private String getExportName(String folderId) throws PersistenceException {
		FolderDAO folderDao = Context.get(FolderDAO.class);
		String exportName = "export";
		if (folderId != null) {
			Folder folder = folderDao.findById(Long.parseLong(folderId));
			if (folder.getName().matches("[\\p{Punct}\\p{Space}\\p{IsLatin}\\p{Digit}]+"))
				exportName += "-" + folder.getName();
			else
				exportName += "-" + folder.getId();
		}
		return exportName;
	}

	private ArrayList<Long> getDocIds(HttpServletRequest request, Long userId) throws PersistenceException {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		FolderDAO folderDao = Context.get(FolderDAO.class);

		ArrayList<Long> docIds = new ArrayList<>();
		if (request.getParameterValues(DOC_ID) != null && request.getParameterValues(DOC_ID).length > 0) {
			String[] ids = request.getParameterValues(DOC_ID);
			for (int i = 0; i < ids.length; i++) {
				Document doc = docDao.findDocument(Long.parseLong(ids[i]));
				Long docId = Long.parseLong(ids[i]);
				if (doc != null && folderDao.isDownloadllowed(doc.getFolder().getId(), userId)
						&& !docIds.contains(docId))
					docIds.add(docId);
			}
		}
		return docIds;
	}

	private String getLevel(HttpServletRequest request) {
		String level = request.getParameter("level");
		if (level == null) {
			level = "all";
		}
		return level;
	}

	/**
	 * The doPost method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to
	 * post.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			response.setContentType("text/html");

			PrintWriter out = response.getWriter();
			out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
			out.println("<HTML>");
			out.println("  <HEAD><TITLE>Download Document Servlet</TITLE></HEAD>");
			out.println("  <BODY>");
			out.print("    This is ");
			out.print(this.getClass());
			out.println(", using the POST method");
			out.println("  </BODY>");
			out.println("</HTML>");
			out.flush();
			out.close();
		} catch (Exception e) {
			// Nothing to do
		}
	}
}