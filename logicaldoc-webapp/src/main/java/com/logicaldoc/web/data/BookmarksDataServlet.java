package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		List<Object> records = new ArrayList<>();
		BookmarkDAO dao = BookmarkDAO.get();

		/*
		 * Search for documents first.
		 */
		String query = """ 
                       select A.id, A.fileType, A.title, A.description, A.position, A.userId, A.targetId, A.type, B.folder.id, B.color 
                         from Bookmark A, Document B 
                        where A.type = %d and A.deleted = 0 and B.deleted = 0 
                          and A.targetId = B.id and A.userId = %d
                       """.formatted(Bookmark.TYPE_DOCUMENT, session.getUserId());
		records.addAll(dao.findByQuery(query, (Map<String, Object>) null, null));

		/*
		 * Than for folders
		 */
		query = """
                select A.id, A.fileType, A.title, A.description, A.position, A.userId, A.targetId, A.type, A.targetId, B.color
                  from Bookmark A, Folder B where A.targetId = B.id and A.type = %d and A.deleted = 0 
                   and A.userId = %d
                """.formatted(Bookmark.TYPE_FOLDER, session.getUserId());
		records.addAll(dao.findByQuery(query, (Map<String, Object>) null, null));

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object bookmarkRecord : records)
			printBookmark(bookmarkRecord, writer);

		writer.write("</list>");
	}

	private void printBookmark(Object bookmarkRecord, PrintWriter writer) {
		Object[] cols = (Object[]) bookmarkRecord;

		writer.print("<bookmark>");
		writer.print(String.format("<id>%d</id>", (Long) cols[0]));
		if (cols[7].toString().equals("0"))
			writer.print(
					String.format("<icon>%s</icon>", FileUtil.getBaseName(IconSelector.selectIcon((String) cols[1]))));
		else
			writer.print("<icon>folder</icon>");
		writer.print(String.format("<name><![CDATA[%s]]></name>", printValue(cols[2])));
		writer.print(String.format("<description><![CDATA[%s]]></description>", printValue(cols[3])));
		writer.print(String.format("<position>%s</position>", printValue(cols[4])));
		writer.print(String.format("<userId>%s</userId>", printValue(cols[5])));
		writer.print(String.format("<targetId>%s</targetId>", printValue(cols[6])));
		writer.print(String.format("<type>%s</type>", printValue(cols[7])));
		writer.print(String.format("<folderId>%s</folderId>", printValue(cols[8])));
		if (cols[9] != null)
			writer.print(String.format("<color><![CDATA[%s]]></color>", printValue(cols[9])));
		writer.print("</bookmark>");
	}

	private String printValue(Object value) {
		return value == null ? "" : value.toString();
	}
}
