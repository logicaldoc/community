package com.logicaldoc.dropbox;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.Metadata;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.util.IconSelector;

/**
 * This servlet is responsible for retrieving Dropbox entries.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DropboxDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = SessionUtil.validateSession(request);
			User user = session.getUser();

			Dropbox dbox = new Dropbox();
			boolean connected = dbox.login(DropboxServiceImpl.loadAccessToken(user));
			if (!connected)
				throw new IOException("Unable to connect to Dropbox");

			boolean folders = "true".equals(request.getParameter("folders"));

			String parent = request.getParameter("parent");
			if (parent == null)
				parent = "#parent#";

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			if ("#parent#".equals(parent)) {
				writer.print("<entry>");
				writer.print("<path>/</path>");
				writer.print("<parent><![CDATA[" + parent + "]]></parent>");
				writer.print("<name>/</name>");
				writer.print("<type>folder</type>");
				writer.print("<iicon>folder</iicon>");
				writer.print("</entry>");
			} else {
				Metadata ent = dbox.get(parent);
				if ((ent == null && "/".equals(parent)) || (ent != null && ent instanceof FolderMetadata)) {
					List<Metadata> entries = dbox.list(parent);
					for (Metadata entry : entries) {
						if (folders && entry instanceof FileMetadata)
							continue;
						writer.print("<entry>");
						writer.print("<path><![CDATA[" + entry.getPathDisplay() + "]]></path>");
						writer.print("<parent><![CDATA[" + parent + "]]></parent>");
						writer.print("<name><![CDATA[" + entry.getName() + "]]></name>");
						writer.print("<type>" + ((entry instanceof FileMetadata) ? "file" : "folder") + "</type>");
						if (entry instanceof FileMetadata)
							writer.print("<iicon>"
									+ FilenameUtils.getBaseName(IconSelector.selectIcon(FilenameUtils
											.getExtension(entry.getName()).toLowerCase().trim())) + "</iicon>");
						else
							writer.print("<iicon>folder</iicon>");
						writer.print("</entry>");
					}
				}
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