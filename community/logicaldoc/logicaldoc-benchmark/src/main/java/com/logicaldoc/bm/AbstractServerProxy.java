package com.logicaldoc.bm;

import java.io.File;

import javax.activation.DataHandler;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

/**
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5
 */
public abstract class AbstractServerProxy {
	
	public String url;
	public String sid;

	public AbstractServerProxy() {
	}
	
	public abstract void logout();

	public abstract String login(String username, String password) throws Exception;

	public abstract WSFolder[] listChildren(String sid, long parentFolder) throws Exception;

	public abstract WSSearchResult find(String sid, WSSearchOptions options) throws Exception;

	public abstract WSDocument[] list(String sid, long folderId) throws Exception;

	public abstract void update(String sid, WSDocument doc) throws Exception;

	public abstract WSDocument create(String ticket, WSDocument doc, DataHandler dataHandler) throws Exception;

	public abstract WSDocument create(String ticket, WSDocument doc, File file) throws Exception;

	public abstract WSFolder create(String ticket, WSFolder newFolder) throws Exception;

	public abstract WSFolder createPath(String ticket, Long rootFolder, String currentKey) throws Exception;

	public abstract long create(String ticket, long parentFolder, String fname) throws Exception;
	

}