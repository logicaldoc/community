package com.logicaldoc.bm;

import java.io.File;
import java.io.IOException;

import javax.activation.DataHandler;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.client.RestAuthClient;
import com.logicaldoc.webservice.rest.client.RestDocumentClient;
import com.logicaldoc.webservice.rest.client.RestFolderClient;
import com.logicaldoc.webservice.rest.client.RestSearchClient;

/**
 * Helper class to store remote service connections.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5
 */
public class RestServerProxy extends AbstractServerProxy {

	public RestAuthClient authClient;
	public RestDocumentClient documentClient;
	public RestFolderClient folderClient;
	public RestSearchClient searchClient;
	
	public RestServerProxy(String url, ContextProperties config) throws IOException {
		this.url = url;
		
		String username = config.getProperty("session.username");
		String pasword = config.getProperty("session.password");
		
		this.authClient = new RestAuthClient(url + "/services/rest/auth");
		this.folderClient = new RestFolderClient(url + "/services/rest/folder", username, pasword, 40);
		this.documentClient = new RestDocumentClient(url + "/services/rest/document", username, pasword, 40);
		this.searchClient = new RestSearchClient(url + "/services/rest/search", username, pasword, 40);		
	}

	public void logout() {
		System.out.println("logout sid: " + sid);
		authClient.logout();
	}

	public String login(String username, String password) throws Exception {
		sid = authClient.getSid();
		return sid;
	}
	
	public WSFolder[] listChildren(String sid, long parentFolder) throws Exception {
		return folderClient.listChildren(parentFolder);
	}

	@Override
	public WSSearchResult find(String sid, WSSearchOptions options) throws Exception {
		return searchClient.find(options);
	}

	@Override
	public WSDocument[] list(String sid, long folderId) throws Exception {
		return documentClient.listDocuments(folderId, null);
	}

	@Override
	public void update(String sid, WSDocument doc) throws Exception {
		documentClient.update(doc);
	}

	@Override
	public WSDocument create(String sid, WSDocument doc, DataHandler dataHandler) throws Exception {
		return documentClient.create(doc, dataHandler);
	}

	@Override
	public WSDocument create(String sid, WSDocument doc, File file) throws Exception {
		return documentClient.create(doc, file);
	}

	@Override
	public WSFolder create(String sid, WSFolder newFolder) throws Exception {
		return folderClient.create(newFolder);
	}

	@Override
	public WSFolder createPath(String sid, Long rootFolder, String path) throws Exception {
		return folderClient.createPath(rootFolder, path);
	}

	@Override
	public long create(String sid, long parentFolder, String fname) throws Exception {
		return folderClient.createFolder(parentFolder, fname);
	}	
}