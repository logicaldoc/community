package com.logicaldoc.webservicesamples.junit;

import java.io.IOException;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;

public class TstSearchByFilename extends BaseTestCase {

	private SoapSearchClient searchClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		searchClient = new SoapSearchClient(settings.getProperty("url") + "/services/Search");
	}

	/**
	 * Note: Be careful when searching by file name because in the same document
	 * title and file name could be different; The method "findByFilename" also
	 * allows you to search by file extension, for example, a search for
	 * "%.xlsx" will return all documents of type excel spreadsheet. You can
	 * search for file names even on non-indexed files, as this type of research
	 * works on the database and not on the search indexes.
	 * 
	 * @throws WebserviceException Error in the webservice layer
	 * @throws PersistenceException Error in the database layer
	 * @throws AuthenticationException Cannot authenticate
	 * @throws IOException I/O error
	 */
	public void testFindByFilename() throws AuthenticationException, PersistenceException, WebserviceException {

		System.out.println("SID: " + sid);

		// You can use the jolly char '%' at start or end
		String fileName = "Get-started%";
		List<WSDocument> docs = searchClient.findByFilename(sid, fileName);

		System.out.println("RESULTS*: " + docs.size());

		for (WSDocument wsd : docs) {
			System.out.println("file name: " + wsd.getFileName());
			System.out.println("wsd.id: " + wsd.getId());
			System.out.println("wsd.size: " + wsd.getFileSize());
			System.out.println("wsd.date: " + wsd.getDate());
			System.out.println("wsd.type: " + wsd.getType());
		}
	}
}