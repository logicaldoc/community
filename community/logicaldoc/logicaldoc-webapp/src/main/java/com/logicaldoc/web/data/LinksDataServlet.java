package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

public class LinksDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(LinksDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			Long docId = null;
			if (StringUtils.isNotEmpty(request.getParameter("docId")))
				docId = new Long(request.getParameter("docId"));

			String parent = request.getParameter("parent");
			if (StringUtils.isEmpty(parent))
				parent = "/";

			Long parentDocId = docId;
			if (parent.contains("-"))
				parentDocId = Long.parseLong(parent.substring(parent.lastIndexOf('-') + 1));
			else if (!"/".equals(parent))
				parentDocId = Long.parseLong(parent);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			StringBuffer query = new StringBuffer(
					"select A.id, B.folder.id, A.type, A.document1.id, A.document1.fileName, A.document1.type, A.document2.id, A.document2.fileName, A.document2.type, ");
			query.append(" A.document1.folder.id, A.document2.folder.id from DocumentLink A, Document B where A.deleted = 0 and B.deleted = 0 ");
			query.append(" and ((A.document1.id = B.id and A.document1.id = " + parentDocId + ")");
			query.append(" or  (A.document2.id = B.id and A.document2.id = " + parentDocId + ")");
			query.append(")");
			if (!docId.equals(parentDocId)) {
				query.append(" and not A.document1.id = " + docId);
				query.append(" and not A.document2.id = " + docId);
			}

			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), null, null);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<link>");
				writer.print("<linkId>" + cols[0] + "</linkId>");
				writer.print("<folderId>" + cols[1] + "</folderId>");
				writer.print("<type>" + cols[2] + "</type>");
				writer.print("<parent>" + parent + "</parent>");
				writer.print("<folderId1>" + cols[9] + "</folderId1>");
				writer.print("<folderId2>" + cols[10] + "</folderId2>");
				if (parentDocId.longValue() == (Long) cols[3]) {
					writer.print("<documentId>" + parent + "-" + cols[6] + "</documentId>");
					writer.print("<filename><![CDATA[" + (String) cols[7] + "]]></filename>");
					writer.print("<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon((String) cols[8]))
							+ "</icon>");
					writer.print("<direction>out</direction>");
				} else {
					writer.print("<documentId>" + parent + "-" + cols[3] + "</documentId>");
					writer.print("<filename><![CDATA[" + (String) cols[4] + "]]></filename>");
					writer.print("<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon((String) cols[5]))
							+ "</icon>");
					writer.print("<direction>in</direction>");
				}
				writer.print("</link>");
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