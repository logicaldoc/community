package com.logicaldoc.web;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServletUtil;

public class TicketDownload extends HttpServlet {

	private static final String TICKET_ID = "ticketId";

	private static final long serialVersionUID = 9088160958327454062L;

	protected static Logger log = LoggerFactory.getLogger(TicketDownload.class);

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
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		String ticketId = request.getParameter(TICKET_ID);
		try {
			ticketId = getTicketId(request);

			Ticket ticket = getTicket(ticketId);

			if (ticket.isTicketExpired() && !isPreviewDownload(ticketId, request))
				throw new IOException("Expired ticket");

			Document document = getDocument(ticket);

			String suffix = getSuffix(ticket, document, request);

			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			String tenantName = tenantDao.getTenantName(ticket.getTenantId());

			String behavior = request.getParameter("behavior");
			if (behavior == null)
				behavior = Context.get().getProperties().getProperty(tenantName + ".downloadticket.behavior",
						"download");
			request.setAttribute("open", Boolean.toString("display".equals(behavior)));

			downloadDocument(request, response, document, null, suffix, ticketId);

			if (!isPreviewDownload(ticketId, request)) {
				ticket.setCount(ticket.getCount() + 1);
				TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
				ticketDao.store(ticket);
			} else
				request.getSession().removeAttribute(getPreviewAttributeName(ticketId));
		} catch (Exception e) {
			log.error(e.getMessage(), e);

			try (PrintWriter out = response.getWriter();) {
				out.println("Ticket " + ticketId + " is no more active"
						+ (e.getMessage() != null ? ": " + e.getMessage() : ""));
			} catch (Exception t) {
				// Nothing to do
			}
		}
	}

	private boolean isPreviewDownload(String ticketId, HttpServletRequest request) {
		return request.getSession() != null
				&& request.getSession().getAttribute(getPreviewAttributeName(ticketId)) != null;
	}

	protected String getPreviewAttributeName(String ticketId) {
		return "preview-" + ticketId;
	}

	private String getSuffix(Ticket ticket, Document document, HttpServletRequest request) throws IOException {
		String suffix = ticket.getSuffix();
		if (request.getParameter("suffix") != null)
			suffix = request.getParameter("suffix");

		if ("pdf".equals(suffix))
			suffix = "conversion.pdf";
		if ("conversion.pdf".equals(suffix)) {
			FormatConverterManager converter = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			converter.convertToPdf(document, null);
			if ("pdf".equalsIgnoreCase(FileUtil.getExtension(document.getFileName())))
				suffix = null;
		}
		return suffix;
	}

	private Document getDocument(Ticket ticket) throws PersistenceException, IOException {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(ticket.getDocId());
		if (doc.getDocRef() != null)
			doc = docDao.findById(doc.getDocRef());
		if (!doc.isPublishing())
			throw new IOException("Document not published");
		return doc;
	}

	private Ticket getTicket(String ticketId) throws IOException {
		TicketDAO tktDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		Ticket ticket = tktDao.findByTicketId(ticketId);
		if (ticket == null || ticket.getDocId() == 0)
			throw new IOException("Unexisting ticket");
		return ticket;
	}

	private String getTicketId(HttpServletRequest request) {
		String ticketId = request.getParameter(TICKET_ID);
		if (StringUtils.isEmpty(ticketId)) {
			ticketId = (String) request.getAttribute(TICKET_ID);
		}

		if (StringUtils.isEmpty(ticketId)) {
			HttpSession session = request.getSession();
			ticketId = (String) session.getAttribute(TICKET_ID);
		}

		log.debug("Download ticket ticketId={}", ticketId);
		return ticketId;
	}

	/**
	 * The doPost method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to
	 * post.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 */
	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			response.setContentType("text/html");

			try (PrintWriter out = response.getWriter()) {
				out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
				out.println("<HTML>");
				out.println("  <HEAD><TITLE>Download Ticket Action</TITLE></HEAD>");
				out.println("  <BODY>");
				out.print("    This is ");
				out.print(this.getClass());
				out.println(", using the POST method");
				out.println("  </BODY>");
				out.println("</HTML>");
			}
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void downloadDocument(HttpServletRequest request, HttpServletResponse response, Document doc,
			String fileVersion, String suffix, String ticket) throws IOException, PersistenceException {

		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(doc, fileVersion, suffix);
		OutputStream os = null;
		try (InputStream is = storer.getStream(doc.getId(), resource)) {
			String filename = doc.getFileName();
			if (suffix != null && suffix.contains("pdf"))
				filename = doc.getFileName() + ".pdf";

			long size = storer.size(doc.getId(), resource);

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
		} catch (IOException ioe) {
			log.error("Cannot open the stream {} {} {} of for ticket {}", doc, fileVersion, suffix, ticket);
			throw ioe;
		} finally {
			try {
				if (os != null) {
					os.flush();
					os.close();
				}
			} catch (Exception t) {
				// Nothing to do
			}
		}

		// Add an history entry to track the download of the document
		DocumentHistory history = new DocumentHistory();
		history.setDocument(doc);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		history.setPath(fdao.computePathExtended(doc.getFolder().getId()));
		history.setEvent(DocumentEvent.DOWNLOADED.toString());
		history.setFilename(doc.getFileName());
		history.setFolderId(doc.getFolder().getId());
		history.setComment("Ticket " + ticket);

		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			ddao.saveDocumentHistory(doc, history);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}
}
