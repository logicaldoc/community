package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Produces the XML used to display the links of a document in the GUI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class LinksDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long docId = getDocId(request);

		String parent = getParent(request);

		Long parentDocId = getParentDocId(docId, parent);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		StringBuilder query = new StringBuilder(
				"select A.id, B.folder.id, A.type, A.document1.id, A.document1.fileName, A.document1.type, A.document2.id, A.document2.fileName, A.document2.type, ");
		query.append(
				" A.document1.folder.id, A.document2.folder.id, A.document1.color, A.document2.color from DocumentLink A, Document B where A.deleted = 0 and B.deleted = 0 ");
		query.append(" and ((A.document1.id = B.id and A.document1.id = " + parentDocId + ")");
		query.append(" or  (A.document2.id = B.id and A.document2.id = " + parentDocId + ")");
		query.append(") ");
		if (!docId.equals(parentDocId)) {
			query.append(" and not A.document1.id = " + docId);
			query.append(" and not A.document2.id = " + docId);
		}

		List<Object> records = dao.findByQuery(query.toString(), (Map<String, Object>) null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;
			printLink(writer, cols, parent, parentDocId, null);
		}

		/*
		 * Now retrieve the documents linked as extended attributes
		 */
		printAttributesOfTypeDocument(docId, parent, parentDocId, session.getTenantName(), writer);

		writer.write("</list>");
	}

	private void printAttributesOfTypeDocument(Long docId, String parent, Long parentDocId, String tenant,
			PrintWriter writer) throws PersistenceException {
		if (!Context.get().getProperties().getBoolean(tenant + ".gui.showdocattrsaslinks", false))
			return;

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		StringBuilder query;
		query = new StringBuilder(
				"select DOC1.ld_id, DOC1.ld_folderid, DOC1.ld_filename, DOC1.ld_color, E.ld_name, DOC2.ld_id, DOC2.ld_folderid, DOC2.ld_filename, DOC2.ld_color, E.ld_label from ld_document_ext E, ld_document DOC1, ld_document DOC2 where E.ld_type="
						+ Attribute.TYPE_DOCUMENT
						+ " and DOC1.ld_deleted=0 and DOC2.ld_deleted=0 and E.ld_intvalue is not null ");
		query.append(" and DOC1.ld_id = E.ld_docid ");
		query.append(" and DOC2.ld_id = E.ld_intvalue ");
		query.append(" and (E.ld_intvalue = ");
		query.append(parentDocId);
		query.append(" or E.ld_docid = ");
		query.append(parentDocId);
		query.append(") ");
		if (!docId.equals(parentDocId)) {
			query.append(" and not DOC1.ld_id = " + docId);
			query.append(" and not DOC2.ld_id = " + docId);
		}

		SqlRowSet rs = dao.queryForRowSet(query.toString(), null);
		while (rs.next()) {
			List<Object> cols = new ArrayList<>(Collections.nCopies(13, null));
			final long docId1 = rs.getLong(1);
			final long docId2 = rs.getLong(6);
			final String attributeName = rs.getString(5);

			cols.set(0, -docId1);
			cols.set(1, rs.getLong(2));
			cols.set(2, StringUtils.defaultString(rs.getString(10), attributeName));
			cols.set(3, docId1);
			cols.set(9, rs.getLong(2));
			cols.set(10, rs.getLong(7));

			if (parentDocId.longValue() == docId1) {
				cols.set(6, docId2);
				cols.set(7, rs.getString(8));
				cols.set(8, rs.getString(8));
				cols.set(12, rs.getString(9));
			} else {
				cols.set(3, docId1);
				cols.set(4, rs.getString(3));
				cols.set(5, rs.getString(3));
				cols.set(11, rs.getString(4));
			}

			printLink(writer, cols.toArray(new Object[0]), parent, parentDocId, attributeName);
		}
	}

	private void printLink(PrintWriter writer, Object[] cols, String parent, Long parentDocId, String attribute) {
		writer.print("<link>");
		writer.print("<linkId>" + cols[0] + "</linkId>");
		writer.print("<folderId>" + cols[1] + "</folderId>");
		writer.print("<type>" + cols[2] + "</type>");
		writer.print("<parent>" + parent + "</parent>");
		writer.print("<folderId1>" + cols[9] + "</folderId1>");
		writer.print("<folderId2>" + cols[10] + "</folderId2>");
		if (StringUtils.isNotEmpty(attribute))
			writer.print("<attribute>" + attribute + "</attribute>");
		if (parentDocId.longValue() == (Long) cols[3]) {
			writer.print("<documentId>" + parent + "-" + cols[6] + "</documentId>");
			writer.print("<filename><![CDATA[" + (String) cols[7] + "]]></filename>");
			writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon((String) cols[8])) + "</icon>");
			writer.print("<direction>out</direction>");
			if (cols[12] != null)
				writer.print("<color><![CDATA[" + cols[12] + "]]></color>");
		} else {
			writer.print("<documentId>" + parent + "-" + cols[3] + "</documentId>");
			writer.print("<filename><![CDATA[" + (String) cols[4] + "]]></filename>");
			writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon((String) cols[5])) + "</icon>");
			if (cols[11] != null)
				writer.print("<color><![CDATA[" + cols[11] + "]]></color>");
			writer.print("<direction>in</direction>");
		}

		writer.print("</link>");
	}

	private Long getParentDocId(Long docId, String parent) {
		Long parentDocId = docId;
		if (parent.contains("-"))
			parentDocId = Long.parseLong(parent.substring(parent.lastIndexOf('-') + 1));
		else if (!"/".equals(parent))
			parentDocId = Long.parseLong(parent);
		return parentDocId;
	}

	private String getParent(HttpServletRequest request) {
		String parent = request.getParameter("parent");
		if (StringUtils.isEmpty(parent))
			parent = "/";
		return parent;
	}

	private Long getDocId(HttpServletRequest request) throws PersistenceException, IOException {
		Long docId = null;
		if (StringUtils.isNotEmpty(request.getParameter("docId"))) {
			docId = Long.parseLong(request.getParameter("docId"));
			DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = ddao.findDocument(docId);
			if (doc != null)
				docId = doc.getId();
		}

		if (docId == null)
			throw new IOException("No document ID");
		return docId;
	}
}