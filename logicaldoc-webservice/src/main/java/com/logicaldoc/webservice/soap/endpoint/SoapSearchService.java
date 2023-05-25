package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.SearchService;

/**
 * Search Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapSearchService extends AbstractService implements SearchService {

	protected static Logger log = LoggerFactory.getLogger(SoapSearchService.class);

	@Override
	public WSSearchResult find(String sid, WSSearchOptions opt) throws PersistenceException, AuthenticationException, WebserviceException, SearchException {
		User user = validateSession(sid);

		SearchOptions options = opt.toSearchOptions();
		options.setUserId(user.getId());

		WSSearchResult searchResult = new WSSearchResult();

		Search lastSearch = Search.get(options);
		lastSearch.search();
		List<Hit> hitsList = lastSearch.getHits();

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<WSDocument> docs = new ArrayList<>();
		for (Hit hit : hitsList) {
			Document d = docDao.findById(hit.getId());
			docDao.initialize(d);
			WSDocument doc = WSUtil.toWSDocument(d);
			doc.setScore(hit.getScore());
			doc.setSummary(hit.getSummary());
			docs.add(doc);
		}

		searchResult.setTotalHits(docs.size());
		searchResult.setHits(docs.toArray(new WSDocument[0]));
		searchResult.setEstimatedHitsNumber(lastSearch.getEstimatedHitsNumber());
		searchResult.setTime(lastSearch.getExecTime());
		searchResult.setMoreHits(lastSearch.isMoreHitsPresent() ? 1 : 0);

		log.info("User: {}  Query: {}", user.getUsername(), options.getExpression());
		log.info("Results number: {}", docs.size());

		return searchResult;
	}

	@Override
	public WSDocument[] findByFilename(String sid, String filename) throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByFileNameAndParentFolderId(null, filename, null, user.getTenantId(), null);
		
		WSDocument[] wsDocs = new WSDocument[docs.size()];
		for (int i = 0; i < docs.size(); i++) {
			try {
				checkReadEnable(user, docs.get(i).getFolder().getId());
				checkPublished(user, docs.get(i));
				checkNotArchived(docs.get(i));
			} catch (Exception e) {
				continue;
			}
			docDao.initialize(docs.get(i));
			wsDocs[i] = WSUtil.toWSDocument(docs.get(i));
		}

		return wsDocs;
	}

	@Override
	public WSFolder[] findFolders(String sid, String name) throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		List<Folder> folders = folderDao.find(name, user.getTenantId());
		WSFolder[] wsFolders = new WSFolder[folders.size()];
		for (int i = 0; i < folders.size(); i++) {
			try {
				checkReadEnable(user, folders.get(i).getId());
			} catch (Exception e) {
				continue;
			}
			folderDao.initialize(folders.get(i));
			wsFolders[i] = WSFolder.fromFolder(folders.get(i));
		}

		return wsFolders;
	}
}