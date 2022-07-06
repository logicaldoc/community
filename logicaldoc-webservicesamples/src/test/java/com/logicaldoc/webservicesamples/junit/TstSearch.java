package com.logicaldoc.webservicesamples.junit;

import java.io.IOException;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;

public class TstSearch extends BaseUnit {

	public TstSearch(String arg0) {
		super(arg0);
	}

	/**
	 * WARNING: because a search returns the results there must be documents indexed. 
	 * By default the documents via web-service are indexed in the
	 * background and then it is possible that the search returns no results. 
	 * If you want you can force the indexing from the GUI of LogicalDOC.
	 * 
	 * @throws IOException
	 */
	public void testSearch() throws IOException {

		SoapSearchClient searchc = new SoapSearchClient(SEARCH_ENDPOINT);

		String query = "Commercial";

		try {
			//FulltextSearchOptions options = new FulltextSearchOptions();
			WSSearchOptions options = new WSSearchOptions();
			
            // This is the language of the document
            options.setLanguage("en");
            options.setExpression(query);
            // This is the language of the query
            options.setExpressionLanguage("en");

            // This is required and it is the maximum number of results that we want for this search
            options.setMaxHits(50);	
            
			WSSearchResult sr = searchc.find(sid, options);
			
			System.out.println("HITS: " + sr.getTotalHits());
			System.out.println("search completed in ms: " + sr.getTime());
			
			if (sr.getHits() != null) {
				for (WSDocument res : sr.getHits()) {
					System.out.println("file name: " + res.getFileName());
					System.out.println("res.id: " + res.getId());
					System.out.println("res.summary: " + res.getSummary());
					System.out.println("res.size: " + res.getFileSize());
					System.out.println("res.date: " + res.getDate());
					System.out.println("res.type: " + res.getType());
					System.out.println("res.score: " + res.getScore());					
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	/**
	 * WARNING: because a search returns the results there must be documents indexed. 
	 * By default the documents via web-service are indexed in the
	 * background and then it is possible that the search returns no results. 
	 * If you want you can force the indexing from the GUI of LogicalDOC.
	 * 
	 * @throws IOException
	 */
	public void testSearchByTemplateFields() throws IOException {

		SoapSearchClient searchc = new SoapSearchClient(SEARCH_ENDPOINT);

		String query = "Windows";

		try {
			//FulltextSearchOptions options = new FulltextSearchOptions();
			WSSearchOptions options = new WSSearchOptions();
			
            // This is the language of the document
            options.setLanguage("en");
            options.setExpression(query);
            // This is the language of the query
            options.setExpressionLanguage("en");

            // This is required and it is the maximum number of results that we want for this search
            options.setMaxHits(50);
            
            options.setTemplate(1L); // Email template
            
            // Setting the template fields in which to search 
            //String[] fields = new String[]{"fhgfgh", "fhgfgh", "fhgfgh"};
            //options.setFields(fields);
            
			WSSearchResult sr = searchc.find(sid, options);
			
			System.out.println("HITS: " + sr.getTotalHits());
			System.out.println("search completed in ms: " + sr.getTime());
			
			if (sr.getHits() != null) {
				for (WSDocument res : sr.getHits()) {
					System.out.println("file name: " + res.getFileName());
					System.out.println("res.id: " + res.getId());
					System.out.println("res.summary: " + res.getSummary());
					System.out.println("res.size: " + res.getFileSize());
					System.out.println("res.date: " + res.getDate());
					System.out.println("res.type: " + res.getType());
					System.out.println("res.score: " + res.getScore());					
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}	
}
