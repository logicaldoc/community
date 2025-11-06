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

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max, Locale locale)
			throws PersistenceException, IOException {

		DocumentDAO documentDAO = DocumentDAO.get();
		FolderDAO folderDAO = FolderDAO.get();

		DateFormat df = getDateFormat();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		for (Document doc : documentDAO.findDeleted(session.getUserId(), 100)) {
			writer.print("<entry>");
			writer.print("<id>" + doc.getId() + "</id>");
			writer.print(
					"<icon>" + FileUtil.getBaseName(IconSelector.selectIcon(doc.getFileExtension())) + "</icon>");
			writer.print("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");
			writer.print("<customId><![CDATA[" + doc.getCustomId() + "]]></customId>");
			writer.print("<lastModified>" + df.format(doc.getLastModified()) + "</lastModified>");
			writer.print("<folderId>" + doc.getFolder().getId() + "</folderId>");
			writer.print("<type>document</type>");
			if (doc.getColor() != null)
				writer.print("<color><![CDATA[" + doc.getColor() + "]]></color>");
			writer.print("</entry>");
		}

		for (Folder fld : folderDAO.findDeleted(session.getUserId(), 100)) {
			writer.print("<entry>");
			writer.print("<id>" + fld.getId() + "</id>");
			writer.print("<filename><![CDATA[" + fld.getName() + "]]></filename>");
			writer.print("<lastModified>" + df.format(fld.getLastModified()) + "</lastModified>");
			writer.print("<folderId>" + fld.getParentId() + "</folderId>");
			writer.print("<type>folder</type>");
			if (fld.getColor() != null)
				writer.print("<color><![CDATA[" + fld.getColor() + "]]></color>");
			writer.print("<folderType>" + fld.getType() + "</folderType>");
			if (fld.getType() == Folder.TYPE_WORKSPACE)
				writer.print("<icon>workspace</icon>");
			else
				writer.print("<icon>folder</icon>");
			writer.print("</entry>");
		}
		writer.write("</list>");
	}
}