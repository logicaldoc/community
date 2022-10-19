package com.logicaldoc.core.folder;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.DocumentValidator;
import com.logicaldoc.core.metadata.validation.Validator;

/**
 * This listener takes care of logically validate a folder. If the document has
 * a template and the template declares a validation script, it is executed
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class FolderValidator extends Validator implements FolderListener {

	private static final String VALIDATED_FLAG = "validated";

	protected static Logger log = LoggerFactory.getLogger(DocumentValidator.class);

	@Override
	public void beforeStore(Folder folder, FolderHistory transaction, Map<String, Object> dictionary) throws Exception {
		try {
			if ("true".equals(dictionary.get(VALIDATED_FLAG))
					|| "true".equals(System.getProperty("ld.bulkloadextreme")))
				return;

			// Skip validation if the folder is not being changed nor stored
			if (transaction == null || (!FolderEvent.CHANGED.toString().equals(transaction.getEvent())
					&& !FolderEvent.CREATED.toString().equals(transaction.getEvent())))
				return;

			validate(folder, folder.getTemplate(), transaction);
		} finally {
			dictionary.put(VALIDATED_FLAG, "true");
		}
	}

	@Override
	public void afterStore(Folder folder, FolderHistory transaction, Map<String, Object> dictionary) throws Exception {
		// Nothing to do
	}
}