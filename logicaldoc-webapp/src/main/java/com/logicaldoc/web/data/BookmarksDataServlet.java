package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for document bookmarks data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class BookmarksDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws IOException, PersistenceException {

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
		for (Object bookmarkRecord : records) {
			printBookmark(bookmarkRecord, writer);
		}
		writer.write("</list>");
	}

	private void printBookmark(Object bookmarkRecord, PrintWriter writer) {
		Object[] cols = (Object[]) bookmarkRecord;

		writer.print("<bookmark>");
		writer.print("<id>" + cols[0] + "</id>");
		if (cols[7].toString().equals("0"))
			writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon((String) cols[1])) + "</icon>");
		else
			writer.print("<icon>folder</icon>");
		writer.print("<name><![CDATA[" + printValue(cols[2]) + "]]></name>");
		writer.print("<description><![CDATA[" + printValue(cols[3]) + "]]></description>");
		writer.print("<position>" + printValue(cols[4]) + "</position>");
		writer.print("<userId>" + printValue(cols[5]) + "</userId>");
		writer.print("<targetId>" + printValue(cols[6]) + "</targetId>");
		writer.print("<type>" + printValue(cols[7]) + "</type>");
		writer.print("<folderId>" + printValue(cols[8]) + "</folderId>");
		if (cols[9] != null)
			writer.print("<color><![CDATA[" + printValue(cols[9]) + "]]></color>");
		writer.print("</bookmark>");
	}

	private Object printValue(Object value) {
		return value == null ? "" : value.toString();
	}
}
