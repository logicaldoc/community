package com.logicaldoc.core.document;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.metadata.initialization.Initializer;

/**
 * This listener takes care of initializing the metadata of a document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class DocumentInitializer extends Initializer implements DocumentListener {

	private static final String INITIALIZED_FLAG = "initialized";

	protected static Logger log = LoggerFactory.getLogger(DocumentInitializer.class);

	@Override
	public void beforeStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws Exception {
		try {
			if ("true".equals(dictionary.get(INITIALIZED_FLAG))
					|| "true".equals(System.getProperty("ld.bulkloadextreme")))
				return;

			initialize(document, document.getTemplate(), transaction);
		} finally {
			dictionary.put(INITIALIZED_FLAG, "true");
		}
	}

	@Override
	public void beforeCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws Exception {
		// Nothing to do
	}

	@Override
	public void afterStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws Exception {
		// Nothing to do
	}

	@Override
	public void afterCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws Exception {
		// Nothing to do
	}

	@Override
	public void afterSaveHistory(Document document, DocumentHistory event, Map<String, Object> dictionary)
			throws Exception {
		// Nothing to do
	}
}