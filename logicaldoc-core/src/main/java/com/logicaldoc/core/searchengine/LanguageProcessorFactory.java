package com.logicaldoc.core.searchengine;

import org.apache.solr.request.SolrQueryRequest;
import org.apache.solr.response.SolrQueryResponse;
import org.apache.solr.update.processor.UpdateRequestProcessor;
import org.apache.solr.update.processor.UpdateRequestProcessorFactory;

/**
 * Basic factory needed to instantiate the LanguageProcessor
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class LanguageProcessorFactory extends UpdateRequestProcessorFactory {

	@Override
	public UpdateRequestProcessor getInstance(SolrQueryRequest request, SolrQueryResponse response,
			UpdateRequestProcessor processor) {
		return new LanguageProcessor(processor);
	}
}