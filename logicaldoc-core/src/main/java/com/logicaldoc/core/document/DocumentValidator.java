package com.logicaldoc.core.document;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.metadata.validation.ValidationException;
import com.logicaldoc.core.metadata.validation.Validator;

/**
 * This listener takes care of logically validate a document. If the document
 * has a template and the template declares a validation script, it is executed
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class DocumentValidator extends Validator implements DocumentListener {

	private static final String VALIDATED_FLAG = "validated";

	private static final Logger log = LoggerFactory.getLogger(DocumentValidator.class);

	@Override
	public void beforeStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		onValidate(document, transaction, dictionary);
	}

	@Override
	public void beforeCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		onValidate(document, transaction, dictionary);
	}

	@Override
	public void afterStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		// Nothing to do
	}

	@Override
	public void afterCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		// Nothing to do
	}

	@Override
	public void afterSaveHistory(Document document, DocumentHistory event, Map<String, Object> dictionary)
			throws PersistenceException {
		// Nothing to do
	}

	private void onValidate(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws ValidationException {
		try {
			if ("true".equals(dictionary.get(VALIDATED_FLAG))
					|| "true".equals(System.getProperty("ld.bulkloadextreme")))
				return;

			// Skip validation if the document is not being changed nor stored
			if (transaction == null || (!DocumentEvent.CHANGED.toString().equals(transaction.getEvent())
					&& !DocumentEvent.STORED.toString().equals(transaction.getEvent())))
				return;

			validate(document, document.getTemplate(), transaction);
		} catch (AutomationException e) {
			log.warn(e.getMessage(), e);
			dictionary.put(VALIDATED_FLAG, "false");
		} finally {
			dictionary.put(VALIDATED_FLAG, "true");
		}
	}
}