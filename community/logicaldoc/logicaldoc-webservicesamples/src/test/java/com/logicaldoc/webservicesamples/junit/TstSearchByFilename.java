package com.logicaldoc.webservicesamples.junit;

import java.io.IOException;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;

public class TstSearchByFilename extends BaseUnit {

	public TstSearchByFilename(String arg0) {
		super(arg0);
	}

	/**
	 * Note: Be careful when searching by file name because in the same document
	 * title and file name could be different; The method "findByFilename" also
	 * allows you to search by file extension, for example, a search for
	 * "%.xlsx" will return all documents of type excel spreadsheet. You can
	 * search for file names even on non-indexed files, as this type of research
	 * works on the database and not on the search indexes.
	 * 
	 * @throws IOException
	 */
	public void testFindByFilename() throws IOException {

		System.out.println("SID: " + sid);

		SoapSearchClient searchc = new SoapSearchClient(SEARCH_ENDPOINT);

		// You can use the jolly char '%' at start or end
		String fileName = "Get-started%";

		try {
			WSDocument[] docs = searchc.findByFilename(sid, fileName);

			if (docs != null) {
				System.out.println("RESULTS*: " + docs.length);

				for (WSDocument wsd : docs) {
					System.out.println("file name: " + wsd.getFileName());
					System.out.println("wsd.id: " + wsd.getId());
					System.out.println("wsd.size: " + wsd.getFileSize());
					System.out.println("wsd.date: " + wsd.getDate());
					System.out.println("wsd.type: " + wsd.getType());
				}
			} else {
				System.out.println("NO RESULT!");
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
