package com.logicaldoc.core.searchengine;

import java.io.IOException;

import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.update.AddUpdateCommand;
import org.apache.solr.update.processor.UpdateRequestProcessor;

import com.logicaldoc.core.searchengine.analyzer.FilteredAnalyzer;

/**
 * Processor that takes care to store the language field of an incoming hit into
 * a thread local variable. This information will be used at analisys time.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class LanguageProcessor extends UpdateRequestProcessor {

	public LanguageProcessor(UpdateRequestProcessor next) {
		super(next);
	}

	@Override
	public void processAdd(AddUpdateCommand cmd) throws IOException {
		SolrInputDocument doc = cmd.getSolrInputDocument();
		FilteredAnalyzer.lang.set((String) doc.getFieldValue(HitField.LANGUAGE.getName()));
		super.processAdd(cmd);
	}
}
