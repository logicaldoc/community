package com.logicaldoc.web;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

public class TicketDownload extends HttpServlet {

	private static final String TICKET_ID = "ticketId";

	private static final long serialVersionUID = 9088160958327454062L;

	private static final Logger log = LoggerFactory.getLogger(TicketDownload.class);

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
		Ticket ticket = null;
		try {
			ticket = TicketDownload.getTicket(request);

			Document document = getDocument(ticket);

			if (ticket.isTicketExpired() && !isPreviewDownload(ticket.getTicketId(), request, document))
				throw new IOException("Expired ticket");

			String suffix = getSuffix(ticket, document, request);

			TenantDAO tenantDao = Context.get(TenantDAO.class);
			String tenantName = tenantDao.getTenantName(ticket.getTenantId());

			String behavior = request.getParameter("behavior");
			if (behavior == null)
				behavior = Context.get().getProperties().getProperty(tenantName + ".downloadticket.behavior",
						"download");
			request.setAttribute("open", Boolean.toString("display".equals(behavior)));

			if (!TicketDownload.checkPassword(request, response, ticket))
				return;

			downloadDocument(request, response, document, null, suffix, ticket.getTicketId());

			if (isPreviewDownload(ticket.getTicketId(), request, document)) {
				request.getSession().removeAttribute(getPreviewAttributeName(ticket.getTicketId()));

				/**
				 * The user may resize the view panel that will trigger a reload
				 * so we must mark the read in the session and count it just the
				 * first time
				 */
				String viewMarker = "ticketviewed-" + ticket.getTicketId();
				if (request.getSession().getAttribute(viewMarker) == null) {
					ticket.setViews(ticket.getViews() + 1);
					request.getSession().setAttribute(viewMarker, true);
				}
				if (isHtml(document))
					suffix = "safe.html";
			} else {
				increaseDownloadCount(request, ticket, document);
			}

			TicketDAO.get().store(ticket);
		} catch (Exception e) {
			log.error(e.getMessage(), e);

			try (PrintWriter out = response.getWriter();) {
				out.println("Ticket " + ticket.getTicketId() + " not authorized"
						+ (e.getMessage() != null ? ": " + e.getMessage() : ""));
			} catch (Exception t) {
				// Nothing to do
			}
		}
	}

	public static boolean checkPassword(HttpServletRequest request, HttpServletResponse response) throws IOException {
		return checkPassword(request, response, getTicket(request));
	}

	public static boolean checkPassword(HttpServletRequest request, HttpServletResponse response, Ticket ticket)
			throws IOException {
		if (StringUtils.isNotEmpty(ticket.getPassword())) {
			String password = getPasswordInRequest(request);
			if (StringUtils.isNotEmpty(password)) {
				try {
					if (!CryptUtil.encryptSHA256(password).equals(ticket.getPassword()))
						throw new IOException("Wrong password");
				} catch (NoSuchAlgorithmException e) {
					throw new IOException(e);
				}
			} else {
				response.setHeader("WWW-Authenticate", "Basic realm=\"Ticket " + ticket.getTicketId() + "\"");
				response.sendError(401, "Unauthorized");
				return false;
			}
		}
		return true;
	}

	private static String getPasswordInRequest(HttpServletRequest request) {
		String authorization = request.getHeader("Authorization");
		if (authorization != null && authorization.toLowerCase().startsWith("basic")) {
			String base64Credentials = authorization.substring("Basic".length()).trim();
			byte[] credDecoded = Base64.getDecoder().decode(base64Credentials);
			String credentials = new String(credDecoded, StandardCharsets.UTF_8);
			// credentials = username:password
			String[] values = credentials.split(":", 2);
			return values[1];
		} else {
			return request.getParameter("password");
		}
	}

	private boolean isHtml(Document document) {
		return document.getFileName().toLowerCase().endsWith(".html")
				|| document.getFileName().toLowerCase().endsWith(".htm")
				|| document.getFileName().toLowerCase().endsWith(".xhtml");
	}

	private void increaseDownloadCount(HttpServletRequest request, Ticket ticket, Document document) {
		if (!((document.getFileName().toLowerCase().endsWith(".dcm") || isHtml(document))
				&& "preview".equals(request.getParameter("control"))))
			ticket.setCount(ticket.getCount() + 1);
	}

	private boolean isPreviewDownload(String ticketId, HttpServletRequest request, Document document) {
		return (request.getSession() != null
				&& request.getSession().getAttribute(getPreviewAttributeName(ticketId)) != null)
				|| ((isHtml(document) || com.logicaldoc.gui.common.client.util.Util.isMediaFile(document.getFileName()))
						&& "preview".equals(request.getParameter("control")));

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
			FormatConverterManager converter = Context.get(FormatConverterManager.class);
			converter.convertToPdf(document, null);
			if ("pdf".equalsIgnoreCase(FileUtil.getExtension(document.getFileName())))
				suffix = null;
		}
		return suffix;
	}

	private Document getDocument(Ticket ticket) throws PersistenceException, IOException {
		DocumentDAO docDao = DocumentDAO.get();
		Document doc = docDao.findById(ticket.getDocId());
		if (doc.getDocRef() != null)
			doc = docDao.findById(doc.getDocRef());
		if (!doc.isPublishing())
			throw new IOException("Document not published");
		return doc;
	}

	private static Ticket getTicket(HttpServletRequest request) throws IOException {
		TicketDAO tktDao = TicketDAO.get();
		Ticket ticket = tktDao.findByTicketId(getTicketId(request));
		if (ticket == null || ticket.getDocId() == 0)
			throw new IOException("Unexisting ticket");
		return ticket;
	}

	private static String getTicketId(HttpServletRequest request) {
		String ticketId = request.getParameter(TICKET_ID);
		if (StringUtils.isEmpty(ticketId)) {
			ticketId = (String) request.getAttribute(TICKET_ID);
		}

		if (StringUtils.isEmpty(ticketId)) {
			HttpSession session = request.getSession();
			ticketId = (String) session.getAttribute(TICKET_ID);
		}

		log.debug("Ticket ticketId={}", ticketId);
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

	private void downloadDocument(HttpServletRequest request, HttpServletResponse response, Document document,
			String fileVersion, String suffix, String ticket) throws IOException, PersistenceException {

		/*
		 * In case the client asks for a safe version of the HTML content
		 */
		DownloadServlet.processSafeHtml(suffix, null, document);

		Store store = Context.get(Store.class);
		String resource = store.getResourceName(document, fileVersion, suffix);
		OutputStream os = null;
		try (InputStream is = store.getStream(document.getId(), resource)) {
			String filename = document.getFileName();
			if (suffix != null && suffix.contains("pdf"))
				filename = document.getFileName() + ".pdf";

			long size = store.size(document.getId(), resource);

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
			log.error("Cannot open the stream {} {} {} of for ticket {}", document, fileVersion, suffix, ticket);
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
		history.setDocument(document);

		FolderDAO fdao = FolderDAO.get();
		history.setPath(fdao.computePathExtended(document.getFolder().getId()));
		history.setEvent(
				isPreviewDownload(ticket, request, document) ? DocumentEvent.VIEWED : DocumentEvent.DOWNLOADED);
		history.setFilename(document.getFileName());
		history.setFolderId(document.getFolder().getId());
		history.setComment("Ticket " + ticket);

		DocumentDAO ddao = DocumentDAO.get();
		try {
			ddao.saveDocumentHistory(document, history);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}
}
