package com.logicaldoc.web;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.jfree.util.Log;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.web.util.ServletUtil;

public class TicketDownload extends HttpServlet {

	private static final long serialVersionUID = 9088160958327454062L;

	protected static Logger logger = LoggerFactory.getLogger(TicketDownload.class);

	/**
	 * Constructor of the object.
	 */
	public TicketDownload() {
		super();
	}

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
		HttpSession session = request.getSession();
		String ticketId = request.getParameter("ticketId");

		if (StringUtils.isEmpty(ticketId)) {
			ticketId = (String) request.getAttribute("ticketId");
		}

		if (StringUtils.isEmpty(ticketId)) {
			ticketId = (String) session.getAttribute("ticketId");
		}

		logger.debug("Download ticket ticketId={}", ticketId);

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			FormatConverterManager converter = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			Ticket ticket = ticketDao.findByTicketId(ticketId);
			if (ticket == null || ticket.getDocId() == 0)
				throw new IOException("Unexisting ticket");

			if (ticket.isTicketExpired())
				throw new IOException("Expired ticket");

			Document doc = docDao.findById(ticket.getDocId());
			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			if (!doc.isPublishing())
				throw new IOException("Document not published");

			String suffix = ticket.getSuffix();
			if ("pdf".equals(suffix))
				suffix = "conversion.pdf";
			if ("conversion.pdf".equals(suffix)) {
				converter.convertToPdf(doc, null);
				if ("pdf".equals(FilenameUtils.getExtension(doc.getFileName()).toLowerCase()))
					suffix = null;
			}

			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			String tenantName = tenantDao.getTenantName(ticket.getTenantId());
			
			request.setAttribute("open", Boolean.toString("display".equals(
					Context.get().getProperties().getProperty(tenantName + ".downloadticket.behavior", "download"))));
			
			downloadDocument(request, response, doc, null, suffix, ticketId);
			ticket.setCount(ticket.getCount() + 1);
			ticketDao.store(ticket);
		} catch (Throwable e) {
			logger.error(e.getMessage(), e);
			PrintWriter out = response.getWriter();
			out.println("Ticket " + ticketId + " is no more active"
					+ (e.getMessage() != null ? ": " + e.getMessage() : ""));
			out.flush();
			out.close();
		}
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
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		response.setContentType("text/html");

		PrintWriter out = response.getWriter();
		out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		out.println("<HTML>");
		out.println("  <HEAD><TITLE>Download Ticket Action</TITLE></HEAD>");
		out.println("  <BODY>");
		out.print("    This is ");
		out.print(this.getClass());
		out.println(", using the POST method");
		out.println("  </BODY>");
		out.println("</HTML>");
		out.flush();
		out.close();
	}

	private void downloadDocument(HttpServletRequest request, HttpServletResponse response, Document doc,
			String fileVersion, String suffix, String ticket)
			throws FileNotFoundException, IOException, ServletException {

		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(doc, fileVersion, suffix);
		String filename = doc.getFileName();
		if (suffix != null && suffix.contains("pdf"))
			filename = doc.getFileName() + ".pdf";

		InputStream is = null;
		OutputStream os = null;
		try {
			long size = storer.size(doc.getId(), resource);
			is = storer.getStream(doc.getId(), resource);

			// get the mimetype
			String mimetype = MimeType.getByFilename(filename);
			// it seems everything is fine, so we can now start writing to the
			// response object
			response.setContentType(mimetype);
			ServletUtil.setContentDisposition(request, response, filename);

			// Chrome and Safary need these headers to allow to skip backwards
			// or
			// forwards multimedia contents
			response.setHeader("Accept-Ranges", "bytes");
			response.setHeader("Content-Length", Long.toString(size));

			os = response.getOutputStream();

			int letter = 0;
			while ((letter = is.read()) != -1) {
				os.write(letter);
			}
		} finally {
			if (os != null)
				os.flush();
			os.close();
			if (is != null)
				is.close();
		}

		// Add an history entry to track the download of the document
		DocumentHistory history = new DocumentHistory();
		history.setDocId(doc.getId());
		history.setFilename(doc.getFileName());
		history.setVersion(doc.getVersion());

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		history.setPath(fdao.computePathExtended(doc.getFolder().getId()));
		history.setEvent(DocumentEvent.DOWNLOADED.toString());
		history.setFilename(doc.getFileName());
		history.setFolderId(doc.getFolder().getId());
		history.setComment("Ticket " + ticket);

		DocumentHistoryDAO hdao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		try {
			hdao.store(history);
		} catch (PersistenceException e) {
			Log.error(e.getMessage(), e);
		}
	}
}
