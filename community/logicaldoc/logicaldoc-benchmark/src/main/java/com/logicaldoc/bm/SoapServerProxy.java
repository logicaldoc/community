package com.logicaldoc.bm;

import java.io.File;
import java.io.IOException;

import javax.activation.DataHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.soap.client.SoapAuthClient;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;

/**
 * Helper class to store remote service connections.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5
 */
public class SoapServerProxy extends AbstractServerProxy {
	
	private static Logger log = LoggerFactory.getLogger(SoapServerProxy.class);

	public SoapAuthClient authClient;
	public SoapDocumentClient documentClient;
	public SoapFolderClient folderClient;
	public SoapSearchClient searchClient;
	
	public SoapServerProxy(String url, ContextProperties config) throws IOException {
		
		SoapAuthClient auth = new SoapAuthClient(url + "/services/Auth");
		SoapDocumentClient documentClient = new SoapDocumentClient(url + "/services/Document",
				config.getInt("webservice.gzip"), false, 40);
		SoapFolderClient folderClient = new SoapFolderClient(url + "/services/Folder", config.getInt("webservice.gzip"),
				false, 40);
		SoapSearchClient searchClient = new SoapSearchClient(url + "/services/Search", config.getInt("webservice.gzip"),
				false, 40);		
		
		this.url = url;
		this.authClient = auth;
		this.folderClient = folderClient;
		this.documentClient = documentClient;
		this.searchClient = searchClient;
	}

	public void logout() {
		System.out.println("logout sid: " + sid);
		authClient.logout(sid);
	}

	public String login(String username, String password) throws Exception {
		sid = authClient.login(username, password);
		System.out.println("login sid: " + sid);
		return sid;
	}
	
	public WSFolder[] listChildren(String sid, long parentFolder) throws Exception {
		//log.debug("listChildren {}, {}", sid, parentFolder);
		return folderClient.listChildren(sid, parentFolder);
	}

	@Override
	public WSSearchResult find(String sid, WSSearchOptions options) throws Exception {
		return searchClient.find(sid, options);
	}

	@Override
	public WSDocument[] list(String sid, long folderId) throws Exception {
		return documentClient.listDocuments(sid, folderId, null);
	}

	@Override
	public void update(String sid, WSDocument doc) throws Exception {
		documentClient.update(sid, doc);
	}

	@Override
	public WSDocument create(String ticket, WSDocument doc, DataHandler dataHandler) throws Exception {
		return documentClient.create(ticket, doc, dataHandler);
	}

	@Override
	public WSDocument create(String ticket, WSDocument doc, File file) throws Exception {
		return documentClient.create(ticket, doc, file);
	}

	@Override
	public WSFolder create(String ticket, WSFolder newFolder) throws Exception {
		return folderClient.create(ticket, newFolder);
	}

	@Override
	public WSFolder createPath(String ticket, Long rootFolder, String currentKey) throws Exception {
		return folderClient.createPath(ticket, rootFolder, currentKey);
	}

	@Override
	public long create(String ticket, long parentFolder, String fname) throws Exception {
		return folderClient.createFolder(ticket, parentFolder, fname);
	}	
}
