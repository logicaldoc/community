package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for garbage data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GarbageDataServlet extends AbstractDataServlet {
	private static final long serialVersionUID = 1L;

	private static final String ICON = "<icon>%s</icon>";

	private static final String COLOR = "<color><![CDATA[%s]]></color>";

	private static final String LAST_MODIFIED = "<lastModified>%s</lastModified>";

	private static final String FILENAME = "<filename><![CDATA[%s]]></filename>";

	private static final String ID = "<id>%d</id>";

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		DateFormat df = getDateFormat();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		for (Document doc : DocumentDAO.get().findDeleted(session.getUserId(), 100)) {
			writer.print("<entry>");
			writer.print(String.format(ID, doc.getId()));
			writer.print(String.format(ICON, FileUtil.getBaseName(IconSelector.selectIcon(doc.getFileExtension()))));
			writer.print(String.format(FILENAME, doc.getFileName()));
			writer.print(String.format("<customId><![CDATA[%s]]></customId>", doc.getCustomId()));
			writer.print(String.format(LAST_MODIFIED, df.format(doc.getLastModified())));
			writer.print(String.format("<folderId>%d</folderId>", doc.getFolder().getId()));
			writer.print("<type>document</type>");
			if (doc.getColor() != null)
				writer.print(String.format(COLOR, doc.getColor()));
			writer.print("</entry>");
		}

		for (Folder fld : FolderDAO.get().findDeleted(session.getUserId(), 100)) {
			writer.print("<entry>");
			writer.print(String.format(ID, fld.getId()));
			writer.print(String.format(FILENAME, fld.getName()));
			writer.print(String.format(LAST_MODIFIED, df.format(fld.getLastModified())));
			writer.print(String.format("<folderId>%s</folderId>", fld.getParentId()));
			writer.print("<type>folder</type>");
			if (fld.getColor() != null)
				writer.print(String.format(COLOR, fld.getColor()));
			writer.print(String.format("<folderType>%d</folderType>", fld.getType()));
			writer.print(String.format(ICON, fld.getType() == Folder.TYPE_WORKSPACE ? "workspace" : "folder"));
			writer.print("</entry>");
		}
		writer.write("</list>");
	}
}