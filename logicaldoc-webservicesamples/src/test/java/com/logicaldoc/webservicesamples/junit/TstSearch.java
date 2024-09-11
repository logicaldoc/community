package com.logicaldoc.webservicesamples.junit;

import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;

public class TstSearch extends BaseTestCase {

	private SoapSearchClient searchClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		searchClient = new SoapSearchClient(settings.getProperty("url") + "/services/Search");
	}

	/**
	 * WARNING: because a search returns the results there must be documents
	 * indexed. By default the documents via web-service are indexed in the
	 * background and then it is possible that the search returns no results. If
	 * you want you can force the indexing from the GUI of LogicalDOC.
	 * 
	 * @throws SearchException Error inside the search
	 * @throws WebserviceException Error in the webservice layer
	 * @throws PersistenceException Error in the database layer
	 * @throws AuthenticationException Cannot authenticate
	 */
	@Test
	public void testSearch()
			throws AuthenticationException, PersistenceException, WebserviceException, SearchException {
		String query = "Commercial";

		// FulltextSearchOptions options = new FulltextSearchOptions();
		WSSearchOptions options = new WSSearchOptions();

		// This is the language of the document
		options.setLanguage("en");
		options.setExpression(query);
		// This is the language of the query
		options.setExpressionLanguage("en");

		// This is required and it is the maximum number of results that we want
		// for this search
		options.setMaxHits(50);

		WSSearchResult result = searchClient.find(sid, options);

		System.out.println("HITS: " + result.getTotalHits());
		System.out.println("search completed in ms: " + result.getTime());

		if (result.getHits() != null) {
			for (WSDocument res : result.getHits()) {
				System.out.println("file name: " + res.getFileName());
				System.out.println("res.id: " + res.getId());
				System.out.println("res.summary: " + res.getSummary());
				System.out.println("res.size: " + res.getFileSize());
				System.out.println("res.date: " + res.getDate());
				System.out.println("res.type: " + res.getType());
				System.out.println("res.score: " + res.getScore());
			}
		}
	}

	@Test
	public void testSearchByTemplateFields() throws AuthenticationException, PersistenceException, WebserviceException, SearchException {
		String query = "Windows";

		// FulltextSearchOptions options = new FulltextSearchOptions();
		WSSearchOptions options = new WSSearchOptions();

		// This is the language of the document
		options.setLanguage("en");
		options.setExpression(query);
		// This is the language of the query
		options.setExpressionLanguage("en");

		// This is required and it is the maximum number of results that we
		// want for this search
		options.setMaxHits(50);

		options.setTemplate(1L); // Email template

		// Setting the template fields in which to search
		// String[] fields = new String[]{"fhgfgh", "fhgfgh", "fhgfgh"};
		// options.setFields(fields);

		WSSearchResult result = searchClient.find(sid, options);

		System.out.println("HITS: " + result.getTotalHits());
		System.out.println("search completed in ms: " + result.getTime());

		if (result.getHits() != null) {
			for (WSDocument res : result.getHits()) {
				System.out.println("file name: " + res.getFileName());
				System.out.println("res.id: " + res.getId());
				System.out.println("res.summary: " + res.getSummary());
				System.out.println("res.size: " + res.getFileSize());
				System.out.println("res.date: " + res.getDate());
				System.out.println("res.type: " + res.getType());
				System.out.println("res.score: " + res.getScore());
			}
		}
	}
}