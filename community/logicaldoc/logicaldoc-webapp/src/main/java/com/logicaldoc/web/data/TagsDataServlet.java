package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for document tags data.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class TagsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TagsDataServlet.class);

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

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			ContextProperties config = Context.get().getProperties();
			String mode = config.getProperty(session.getTenantName() + ".tag.mode");

			String firstLetter = request.getParameter("firstLetter");
			String editing = request.getParameter("editing");

			// Check if the data must be collected for a specific document
			Document doc = null;
			if (request.getParameter("docId") != null) {
				doc = docDao.findDocument(Long.parseLong(request.getParameter("docId")));
				docDao.initialize(doc);
			}

			Folder folder = null;
			if (request.getParameter("folderId") != null) {
				folder = fDao.findFolder(Long.parseLong(request.getParameter("folderId")));
				fDao.initialize(folder);
			}

			HashMap<String, Long> tgs = new HashMap<String, Long>();

			if (("preset".equals(firstLetter) || "preset".equals(mode)) && "true".equals(editing)) {
				// We have to return the preset only, since the user is editing
				// a document
				GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
				List<Generic> buf = gDao.findByTypeAndSubtype("tag", null, null, session.getTenantId());
				for (Generic generic : buf)
					tgs.put(generic.getSubtype(), 0L);
			} else if (org.apache.commons.lang.StringUtils.isNotEmpty(firstLetter)) {
				tgs = (HashMap<String, Long>) docDao.findTags(firstLetter, session.getTenantId());
			} else {
				tgs = (HashMap<String, Long>) docDao.findTags(null, session.getTenantId());
			}

			if (doc != null) {

				/*
				 * In case a document was specified we have to enrich the list
				 * with the document's tags
				 */
				for (Tag tag : doc.getTags())
					if (!tgs.containsKey(tag.getTag()))
						tgs.put(tag.getTag(), 1L);
			}

			if (folder != null) {

				/*
				 * In case a folder was specified we have to enrich the list
				 * with the folder's tags
				 */
				for (Tag tag : folder.getTags())
					if (!tgs.containsKey(tag.getTag()))
						tgs.put(tag.getTag(), 1L);
			}

			PrintWriter writer = response.getWriter();
			writer.write("<list>");
			int i = 0;

			List<String> words = new ArrayList<String>(tgs.keySet());
			Collections.sort(words);

			for (String tag : words) {
				writer.print("<tag>");
				writer.print("<index>" + i++ + "</index>");
				writer.print("<word><![CDATA[" + tag + "]]></word>");
				writer.print("<count>" + tgs.get(tag) + "</count>");
				writer.print("</tag>");
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