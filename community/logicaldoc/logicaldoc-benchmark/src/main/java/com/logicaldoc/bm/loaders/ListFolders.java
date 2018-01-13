package com.logicaldoc.bm.loaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.bm.AbstractLoader;
import com.logicaldoc.bm.AbstractServerProxy;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSFolder;

/**
 * A loader thread that retrieves the folders beneath each directory from the
 * root. This is an expensive process but should reach a stable execution time
 * once the folders in the profile have all been created.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @author Alessandro Gasparini - LogicalDOC
 * @since 6.5
 */
public class ListFolders extends AbstractLoader {
	
	private static Logger log = LoggerFactory.getLogger(ListFolders.class);

	private String messageRecord;

	private int totalFolders;

	private long rootFolder = 4;

	public ListFolders() {
		super(ListFolders.class.getName().substring(ListFolders.class.getName().lastIndexOf('.') + 1));
		ContextProperties config = Context.get().getProperties();
		rootFolder = Long.parseLong(config.getProperty("ListFolders.rootFolder"));
	}

	/**
	 * Go to a directory and get a listing of the folders beneath it.
	 */
	@Override
	protected String doLoading(AbstractServerProxy serverProxy) throws Exception {		
		totalFolders = 0;
		listFoldersRecursive(serverProxy, rootFolder);

		// Done
		String msg = String.format("Found %s folders", Long.toString(totalFolders));
		this.messageRecord = msg;
		return msg;
	}

	@Override
	public String getSummary() {
		return super.getSummary() + messageRecord;
	}

	/**
	 * Recursive method to list all folders in the hierarchy.
	 */
	private void listFoldersRecursive(AbstractServerProxy serverProxy, long parentFolder) {

		//log.debug("listFoldersRecursive()");
		
		WSFolder[] folders = new WSFolder[0];
		try {
			folders = serverProxy.listChildren(serverProxy.sid, parentFolder);
			//log.debug("folders: {}", (Object[])folders);
		} catch (Exception e) {
			log.warn("listFoldersRecursive(): ", e);
		} catch (Throwable tw) {
			log.warn("listFoldersRecursive(): ", tw);
			throw tw;
		}

		if (folders != null) {
			//log.debug("folders.length: {}", folders.length);
			totalFolders += folders.length;
			for (WSFolder info : folders) {
				listFoldersRecursive(serverProxy, info.getId());
			}
		}
	}
}
