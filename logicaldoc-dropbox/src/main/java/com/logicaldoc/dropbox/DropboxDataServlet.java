package com.logicaldoc.dropbox;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.dropbox.core.DbxException;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.Metadata;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.security.StringEncrypter.EncryptionException;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for retrieving Dropbox entries.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(DropboxDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		try {
			Session session = DropboxServiceImpl.validateSession(request);
			User user = session.getUser();

			Dropbox dbox = connectDropBox(user);

			boolean folders = "true".equals(request.getParameter("folders"));

			String parent = getParent(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Cache-Control", "no-cache,no-store,must-revalidate");
			response.setHeader("Expires", "0");
			response.setHeader("Pragma", "no-cache");

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
				printEntries(dbox, folders, parent, writer);
			}
			writer.write("</list>");
		} catch (Exception e) {
			handleError(response, e);
		}
	}

	private void printEntries(Dropbox dbox, boolean folders, String parent, PrintWriter writer) throws DbxException {
		Metadata ent = dbox.get(parent);
		if ((ent == null && "/".equals(parent)) || ent instanceof FolderMetadata) {
			List<Metadata> entries = dbox.list(parent);
			for (Metadata entry : entries) {
				if (folders && entry instanceof FileMetadata)
					continue;
				writer.print("<entry>");
				writer.print("<path><![CDATA[" + entry.getPathDisplay() + "]]></path>");
				writer.print("<parent><![CDATA[" + parent + "]]></parent>");
				writer.print("<name><![CDATA[" + entry.getName() + "]]></name>");
				writer.print("<type>" + ((entry instanceof FileMetadata) ? "file" : "folder") + "</type>");
				printIcon(writer, entry);
				writer.print("</entry>");
			}
		}
	}

	private void printIcon(PrintWriter writer, Metadata entry) {
		if (entry instanceof FileMetadata)
			writer.print("<iicon>"
					+ FileUtil.getBaseName(
							IconSelector.selectIcon(FileUtil.getExtension(entry.getName()).toLowerCase().trim()))
					+ "</iicon>");
		else
			writer.print("<iicon>folder</iicon>");
	}

	private String getParent(HttpServletRequest request) {
		String parent = request.getParameter("parent");
		if (parent == null)
			parent = "#parent#";
		return parent;
	}

	private void handleError(HttpServletResponse response, Throwable e) {
		String message = e.getMessage();
		log.error(message, e);
		try {
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private Dropbox connectDropBox(User user) throws IOException, PersistenceException, EncryptionException {
		Dropbox dbox = new Dropbox(user.getId());
		boolean connected = dbox.login();
		if (!connected)
			throw new IOException("Unable to connect to Dropbox");
		return dbox;
	}
}