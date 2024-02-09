package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for document versions data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class VersionsDataServlet extends AbstractDataServlet {

	private static final String DOC_ID = "docId";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		List<Object> records = executeQuery(request, max);

		/*
		 * Iterate over records composing the response XML document
		 */
		DateFormat df = getDateFormat();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

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
			writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon((String) cols[11])) + "</icon>");
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
	}

	private List<Object> executeQuery(HttpServletRequest request, Integer max) throws PersistenceException {
		VersionDAO dao = (VersionDAO) Context.get().getBean(VersionDAO.class);

		Map<String, Object> params = new HashMap<>();

		StringBuilder query = new StringBuilder(
				"select A.id, A.username, A.event, A.version, A.fileVersion, A.versionDate, A.comment, A.docId, A.fileName,"
						+ " A.customId, A.fileSize, A.type, A.templateName, A.workflowStatus, A.workflowStatusDisplay, A.userId, A.color ");
		if (request.getParameter(DOC_ID) != null) {
			long docId = Long.parseLong(request.getParameter(DOC_ID));
			DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = ddao.findDocument(docId);
			if (doc != null)
				docId = doc.getId();

			query.append(" from Version A where A.deleted = 0 and A.docId = :docId ");
			params.put(DOC_ID, docId);
		} else {
			query.append(" from Version A, Archive B where A.deleted = 0 and A in elements(B.entries) ");
			query.append(" and B.id = :archiveId");
			params.put("archiveId", Long.parseLong(request.getParameter("archiveId")));
		}
		query.append(" order by A.versionDate desc ");

		return dao.findByQuery(query.toString(), params, max != null ? max : 100);
	}
}