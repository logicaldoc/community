package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for document bookmarks data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class BookmarksDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max, Locale locale)
			throws PersistenceException, IOException {

		/*
		 * Iterate over the collection of bookmarks
		 */
		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		List<Object> records = new ArrayList<Object>();
		BookmarkDAO dao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);

		/*
		 * Search for documents first.
		 */
		StringBuilder query = new StringBuilder(
				"select A.id, A.fileType, A.title, A.description, A.position, A.userId, A.targetId, A.type, B.folder.id, B.color "
						+ " from Bookmark A, Document B where A.type=" + Bookmark.TYPE_DOCUMENT
						+ " and A.deleted = 0 and B.deleted = 0 and A.targetId = B.id and A.userId = "
						+ session.getUserId());
		records.addAll(dao.findByQuery(query.toString(), (Map<String, Object>) null, null));

		/*
		 * Than for folders
		 */
		query = new StringBuilder(
				"select A.id, A.fileType, A.title, A.description, A.position, A.userId, A.targetId, A.type, A.targetId, B.color "
						+ " from Bookmark A, Folder B where A.targetId = B.id and A.type=" + Bookmark.TYPE_FOLDER
						+ " and A.deleted = 0 and A.userId = " + session.getUserId());
		records.addAll(dao.findByQuery(query.toString(), (Map<String, Object>) null, null));

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object record : records) {
			Object[] cols = (Object[]) record;

			writer.print("<bookmark>");
			writer.print("<id>" + cols[0] + "</id>");
			if (cols[7].toString().equals("0"))
				writer.print(
						"<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon((String) cols[1])) + "</icon>");
			else
				writer.print("<icon>folder</icon>");
			writer.print("<name><![CDATA[" + (cols[2] == null ? "" : cols[2]) + "]]></name>");
			writer.print("<description><![CDATA[" + (cols[3] == null ? "" : cols[3]) + "]]></description>");
			writer.print("<position>" + (cols[4] == null ? "" : cols[4]) + "</position>");
			writer.print("<userId>" + (cols[5] == null ? "" : cols[5]) + "</userId>");
			writer.print("<targetId>" + (cols[6] == null ? "" : cols[6]) + "</targetId>");
			writer.print("<type>" + (cols[7] == null ? "" : cols[7]) + "</type>");
			writer.print("<folderId>" + (cols[8] == null ? "" : cols[8]) + "</folderId>");
			if (cols[9] != null)
				writer.print("<color><![CDATA[" + (cols[9] == null ? "" : cols[9]) + "]]></color>");
			writer.print("</bookmark>");
		}
		writer.write("</list>");
	}
}
