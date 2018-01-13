package com.logicaldoc.bm.loaders;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.bm.AbstractLoader;
import com.logicaldoc.bm.AbstractServerProxy;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

/**
 * Executes a random selection of full-text queries from /expressions.txt
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class Search extends AbstractLoader {
	
	private static Logger log = LoggerFactory.getLogger(Search.class);

	private Random random = new Random();

	private List<String> expressions = new ArrayList<String>();

	private String messageRecord;

	private int searchResults = 50;

	public Search() {
		super(Search.class.getName().substring(Search.class.getName().lastIndexOf('.') + 1));
		
		ContextProperties config = Context.get().getProperties();
		if (config.containsKey("Search.results")) {		
			searchResults = config.getInt("Search.results");
		}
		
		loadExpressions();
	}

	@Override
	protected String doLoading(AbstractServerProxy serverProxy) throws Exception {
		String query = getRandomQuery();
		int results = performFullTextSearch(serverProxy, query);
		String msg = String.format("%d hits for query \"%s\"", results, query);
		this.messageRecord = msg;
		return msg;
	}

	@Override
	public String getSummary() {
		return super.getSummary() + messageRecord;
	}

	private String getRandomQuery() {
		if (expressions != null && expressions.size() > 0) {
			int idx = random.nextInt(expressions.size());
			return expressions.get(idx);
		}
		return null;
	}

	private int performFullTextSearch(AbstractServerProxy serverProxy, String query) throws Exception {
		int results = 0;

		WSSearchOptions options = new WSSearchOptions();

		String lang = session.getLanguage();
		if (StringUtils.isEmpty(lang))
			lang = "en";

		// This is the language of the document
		options.setLanguage(lang);
		options.setExpression(query);

		// This is the language of the query
		options.setExpressionLanguage(lang);

		// This is required and it is the maximum number of results that we want
		// for this search
		options.setMaxHits(searchResults);

		WSSearchResult sr = serverProxy.find(serverProxy.sid, options);

		// System.out.println("HITS: " + sr.getTotalHits());
		// System.out.println("search completed in ms: " + sr.getTime());

		if (sr.getHits() != null) {
			for (WSDocument res : sr.getHits()) {
				if (res.getId() != 0)
					results++;
			}
		}

		return results;
	}

	private void loadExpressions() {
		InputStream fstream = this.getClass().getResourceAsStream("/expressions.txt");

		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(fstream));
			String strLine;
			// Read File Line By Line
			while ((strLine = br.readLine()) != null) {
				// Print the content in a new list item
				expressions.add((String) strLine);
			}
			// Close the input stream
			fstream.close();

			log.info("Loaded {} expressions", expressions.size());
		} catch (Throwable e) {
			System.err.println("Error: " + e.getMessage());
		}
	}
}
