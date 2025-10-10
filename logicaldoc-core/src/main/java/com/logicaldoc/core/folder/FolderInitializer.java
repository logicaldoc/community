package com.logicaldoc.core.folder;

import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.initialization.Initializer;

/**
 * This listener takes care of initializing the metadata of a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class FolderInitializer extends Initializer implements FolderListener {

	private static final String INITIALIZED_FLAG = "initialized";

	@Override
	public void beforeStore(Folder folder, FolderHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		try {
			if ("true".equals(dictionary.get(INITIALIZED_FLAG))
					|| "true".equals(System.getProperty("ld.bulkloadextreme")))
				return;

			initialize(folder, folder.getTemplate(), transaction);
		} finally {
			dictionary.put(INITIALIZED_FLAG, "true");
		}
	}

	@Override
	public void afterStore(Folder document, FolderHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		// Nothing to do
	}
}