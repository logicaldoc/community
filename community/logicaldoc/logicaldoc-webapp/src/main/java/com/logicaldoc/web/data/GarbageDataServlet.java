package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
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
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for garbage data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GarbageDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(GarbageDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			DocumentDAO documentDAO = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			FolderDAO folderDAO = (FolderDAO) Context.get().getBean(FolderDAO.class);

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");
			for (Document doc : documentDAO.findDeleted(session.getUserId(), 100)) {
				writer.print("<entry>");
				writer.print("<id>" + doc.getId() + "</id>");
				writer.print("<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon(doc.getFileExtension()))
						+ "</icon>");
				writer.print("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");
				writer.print("<customId><![CDATA[" + doc.getCustomId() + "]]></customId>");
				writer.print("<lastModified>" + df.format(doc.getLastModified()) + "</lastModified>");
				writer.print("<folderId>" + doc.getFolder().getId() + "</folderId>");
				writer.print("<type>document</type>");
				writer.print("</entry>");
			}
			
			for (Folder fld : folderDAO.findDeleted(session.getUserId(), 100)) {
				writer.print("<entry>");
				writer.print("<id>" + fld.getId() + "</id>");
				writer.print("<icon>folder</icon>");
				writer.print("<filename><![CDATA[" + fld.getName() + "]]></filename>");
				writer.print("<lastModified>" + df.format(fld.getLastModified()) + "</lastModified>");
				writer.print("<folderId>" + fld.getParentId() + "</folderId>");
				writer.print("<type>folder</type>");
				writer.print("</entry>");
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