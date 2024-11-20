package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This servlet is responsible for document tags data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TagsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		ContextProperties config = Context.get().getProperties();
		String mode = config.getProperty(session.getTenantName() + ".tag.mode");

		String firstLetter = request.getParameter("firstLetter");
		String editing = request.getParameter("editing");

		// Check if the data must be collected for a specific document
		Long docId = getDocId(request);

		Long folderId = getFolderId(request);

		long mx = config.getLong(session.getTenantName() + ".tag.select.maxtags", max != null ? max : 100);

		HashMap<String, Long> tagsMap = buildTagsMap(session, mode, firstLetter, editing);

		List<String> words = new ArrayList<>(tagsMap.keySet());
		Collections.sort(words);

		// Limit the collection
		if (words.size() > mx && mx > 0)
			words = words.stream().limit(mx).collect(Collectors.toList());

		enrichMapWithDocumentTags(docId, tagsMap, words);

		enrichMapWithFolderTags(folderId, tagsMap, words);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		int i = 0;

		for (String tag : words) {
			writer.print("<tag>");
			writer.print("<index>" + i++ + "</index>");
			writer.print("<word><![CDATA[" + tag + "]]></word>");
			writer.print("<count>" + tagsMap.get(tag) + "</count>");
			writer.print("</tag>");
		}
		writer.write("</list>");
	}

	private void enrichMapWithFolderTags(Long folderId, HashMap<String, Long> tagsMap, List<String> words) {
		FolderDAO fDao = Context.get().getBean(FolderDAO.class);
		if (folderId != null) {
			List<String> tags = fDao.findTags(folderId);

			/*
			 * In case a folder was specified we have to enrich the list with
			 * the folder's tags
			 */
			for (String tag : tags) {
				tagsMap.computeIfAbsent(tag, k -> 1L);
				if (!words.contains(tag))
					words.add(tag);
			}
		}
	}

	private void enrichMapWithDocumentTags(Long docId, HashMap<String, Long> tagsMap, List<String> words)
			throws PersistenceException {
		if (docId != null) {
			DocumentDAO docDao = Context.get().getBean(DocumentDAO.class);
			List<String> tags = docDao.findTags(docId);

			/*
			 * In case a document was specified we have to enrich the list with
			 * the document's tags
			 */
			for (String tag : tags) {
				tagsMap.computeIfAbsent(tag, k -> 1L);
				if (!words.contains(tag))
					words.add(tag);
			}
		}
	}

	private HashMap<String, Long> buildTagsMap(Session session, String mode, String firstLetter, String editing)
			throws PersistenceException {

		DocumentDAO docDao = Context.get().getBean(DocumentDAO.class);
		HashMap<String, Long> tagsMap = new HashMap<>();

		if (("preset".equals(firstLetter) || "preset".equals(mode)) && "true".equals(editing)) {
			// We have to return the preset only, because the user is
			// editing a document
			GenericDAO gDao = Context.get().getBean(GenericDAO.class);
			List<Generic> buf = gDao.findByTypeAndSubtype("tag", null, null, session.getTenantId());
			for (Generic generic : buf)
				tagsMap.put(generic.getSubtype(), 0L);
		} else if (org.apache.commons.lang.StringUtils.isNotEmpty(firstLetter)) {
			tagsMap = (HashMap<String, Long>) docDao.findTags(firstLetter, session.getTenantId());
		} else {
			tagsMap = (HashMap<String, Long>) docDao.findTags(null, session.getTenantId());
		}
		return tagsMap;
	}

	private Long getFolderId(HttpServletRequest request) {
		Long folderId = null;
		if (request.getParameter("folderId") != null)
			folderId = Long.parseLong(request.getParameter("folderId"));
		return folderId;
	}

	private Long getDocId(HttpServletRequest request) {
		Long docId = null;
		if (request.getParameter("docId") != null)
			docId = Long.parseLong(request.getParameter("docId"));
		return docId;
	}
}