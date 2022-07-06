package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for documents history data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentHistoryDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DocumentHistoryDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			String locale = request.getParameter("locale");
			Integer max = request.getParameter("max") != null ? Integer.parseInt(request.getParameter("max")) : null;

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			// Used only to cache the already encountered documents when the
			// history
			// is related to a single user (for dashboard visualization)
			Set<Long> docIds = new HashSet<Long>();

			List<Object> parameters = new ArrayList<Object>();
			DocumentHistoryDAO dao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
			StringBuffer query = new StringBuffer(
					"select A.username, A.event, A.version, A.date, A.comment, A.filename, A.isNew, A.folderId, A.docId, A.path, A.sessionId, A.userId, A.reason, A.ip, A.device, A.geolocation, A.color from DocumentHistory A where 1=1 and A.deleted = 0 ");
			if (request.getParameter("docId") != null) {
				Long docId = Long.parseLong(request.getParameter("docId"));
				DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = ddao.findDocument(docId);
				if (doc != null)
					docId = doc.getId();
				query.append(" and A.docId = ?" + (parameters.size() + 1));
				parameters.add(docId);
			}
			if (request.getParameter("userId") != null) {
				query.append(" and A.userId = ?" + (parameters.size() + 1));
				parameters.add(Long.parseLong(request.getParameter("userId")));
			}
			if (request.getParameter("event") != null) {
				query.append(" and A.event = ?" + (parameters.size() + 1));
				parameters.add(request.getParameter("event"));
			}
			query.append(" order by A.date desc ");

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), parameters.toArray(new Object[0]),
					max);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;
				if (request.getParameter("userId") != null) {
					/*
					 * If the request contains the user specification, we report
					 * just the latest event per each document
					 */
					if (docIds.contains(cols[8]))
						continue;
					else
						docIds.add((Long) cols[8]);
				}

				writer.print("<history>");
				writer.print("<user><![CDATA[" + cols[0] + "]]></user>");
				writer.print("<event><![CDATA[" + I18N.message((String) cols[1], locale) + "]]></event>");
				writer.print("<version>" + cols[2] + "</version>");
				writer.print("<date>" + df.format((Date) cols[3]) + "</date>");
				writer.print("<comment><![CDATA[" + (cols[4] == null ? "" : cols[4]) + "]]></comment>");
				writer.print("<filename><![CDATA[" + (cols[5] == null ? "" : cols[5]) + "]]></filename>");
				writer.print("<icon>" + FilenameUtils.getBaseName(
						IconSelector.selectIcon(FilenameUtils.getExtension((String) cols[5]))) + "</icon>");
				writer.print("<new>" + (1 == (Integer) cols[6]) + "</new>");
				writer.print("<folderId>" + cols[7] + "</folderId>");
				writer.print("<docId>" + cols[8] + "</docId>");
				writer.print("<path><![CDATA[" + (cols[9] == null ? "" : cols[9]) + "]]></path>");
				if (showSid)
					writer.print("<sid><![CDATA[" + (cols[10] == null ? "" : cols[10]) + "]]></sid>");
				writer.print("<userId>" + cols[11] + "</userId>");
				writer.print("<reason><![CDATA[" + (cols[12] == null ? "" : cols[12]) + "]]></reason>");
				writer.print("<ip><![CDATA[" + (cols[13] == null ? "" : cols[13]) + "]]></ip>");
				writer.print("<device><![CDATA[" + (cols[14] == null ? "" : cols[14]) + "]]></device>");
				writer.print("<geolocation><![CDATA[" + (cols[15] == null ? "" : cols[15]) + "]]></geolocation>");

				if (cols[16] != null)
					writer.write("<color><![CDATA[" + cols[16] + "]]></color>");
				writer.print("</history>");
			}
			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}