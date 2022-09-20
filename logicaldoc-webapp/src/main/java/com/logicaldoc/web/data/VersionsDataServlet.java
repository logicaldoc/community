package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for document versions data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class VersionsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(VersionsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			ServiceUtil.validateSession(request);

			/*
			 * Load some filters from the current request
			 */
			int max = 100;

			try {
				max = Integer.parseInt(request.getParameter("max"));
			} catch (Throwable t) {
			}

			String locale = request.getParameter("locale");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			VersionDAO dao = (VersionDAO) Context.get().getBean(VersionDAO.class);

			Map<String, Object> params = new HashMap<String, Object>();

			StringBuffer query = new StringBuffer(
					"select A.id, A.username, A.event, A.version, A.fileVersion, A.versionDate, A.comment, A.docId, A.fileName,"
							+ " A.customId, A.fileSize, A.type, A.templateName, A.workflowStatus, A.workflowStatusDisplay, A.userId, A.color ");
			if (request.getParameter("docId") != null) {
				long docId = Long.parseLong(request.getParameter("docId"));
				DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = ddao.findDocument(docId);
				if (doc != null)
					docId = doc.getId();

				query.append(" from Version A where A.deleted = 0 and A.docId = :docId ");
				params.put("docId", docId);
			} else {
				query.append(" from Version A, Archive B where A.deleted = 0 and A in elements(B.entries) ");
				query.append(" and B.id = :archiveId");
				params.put("archiveId", Long.parseLong(request.getParameter("archiveId")));
			}
			query.append(" order by A.versionDate desc ");

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), params, max);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<version>");
				writer.print("<id>" + cols[0] + "</id>");
				writer.print("<user><![CDATA[" + (cols[1] == null ? "" : cols[1]) + "]]></user>");
				writer.print("<event><![CDATA[" + I18N.message((String) cols[2], locale) + "]]></event>");
				writer.print("<version>" + cols[3] + "</version>");
				writer.print("<fileVersion>" + cols[4] + "</fileVersion>");
				writer.print("<date>" + df.format((Date) cols[5]) + "</date>");
				writer.print("<comment><![CDATA[" + (cols[6] == null ? "" : cols[6]) + "]]></comment>");
				writer.print("<docid>" + cols[7] + "</docid>");
				writer.print("<filename><![CDATA[" + (String) cols[8] + "]]></filename>");
				writer.print("<customid><![CDATA[" + (cols[9] == null ? "" : cols[9]) + "]]></customid>");
				writer.print("<size>" + cols[10] + "</size>");
				writer.print(
						"<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon((String) cols[11])) + "</icon>");
				writer.print("<type>" + (String) cols[11] + "</type>");

				if (cols[12] != null)
					writer.print("<template><![CDATA[" + cols[12] + "]]></template>");
				if (cols[13] != null)
					writer.print("<workflowStatus><![CDATA[" + cols[13] + "]]></workflowStatus>");
				if (cols[14] != null)
					writer.print("<workflowStatusDisplay><![CDATA[" + cols[14] + "]]></workflowStatusDisplay>");
				writer.print("<userId>" + cols[15] + "</userId>");
				if (cols[16] != null)
					writer.print("<color><![CDATA[" + cols[16] + "]]></color>");

				writer.print("</version>");
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