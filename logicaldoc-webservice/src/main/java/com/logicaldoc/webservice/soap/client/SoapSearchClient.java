package com.logicaldoc.webservice.soap.client;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.soap.SearchService;

/**
 * Search Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class SoapSearchClient extends SoapClient<SearchService> implements SearchService {

	public SoapSearchClient(String endpoint, int gzipThreshold, boolean log, int timeout) {
		super(endpoint, SearchService.class, gzipThreshold, log, timeout);
	}

	public SoapSearchClient(String endpoint) {
		super(endpoint, SearchService.class, -1, true, -1);
	}

	@Override
	public WSSearchResult find(String sid, WSSearchOptions options)
			throws AuthenticationException, PersistenceException, WebserviceException, SearchException {
		return client.find(sid, options);
	}

	@Override
	public List<WSDocument> findByFilename(String sid, String filename)
			throws AuthenticationException, WebserviceException, PersistenceException {
		final List<WSDocument> documents = client.findByFilename(sid, filename);
		if (documents != null)
			return documents;
		else
			return new ArrayList<>();
	}

	@Override
	public List<WSFolder> findFolders(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		final List<WSFolder> folders = client.findFolders(sid, name);
		if (folders != null)
			return folders;
		else
			return new ArrayList<>();
	}
}