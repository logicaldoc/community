package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

import javax.jws.WebService;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;
import com.logicaldoc.webservice.soap.TagService;

/**
 * Tag Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@WebService(name = "Tag", serviceName = "Tag")
public class SoapTagClient extends SoapClient<TagService> implements TagService {

	public SoapTagClient(String endpoint, int gzipThreshold, boolean log, int timeout) throws IOException {
		super(endpoint, TagService.class, gzipThreshold, log, timeout);
	}

	public SoapTagClient(String endpoint) throws IOException {
		super(endpoint, TagService.class, -1, true, -1);
	}

	@Override
	public void setDocumentTags(String sid, long docId, String[] tags) throws Exception {
		client.setDocumentTags(sid, docId, tags);
	}

	@Override
	public void addDocumentTags(String sid, long docId, String[] tags) throws Exception {
		client.addDocumentTags(sid, docId, tags);
	}

	@Override
	public String[] getDocumentTags(String sid, long docId) throws Exception {
		return client.getDocumentTags(sid, docId);
	}

	@Override
	public void setFolderTags(String sid, long folderId, String[] tags) throws Exception {
		client.setFolderTags(sid, folderId, tags);
	}

	@Override
	public void addFolderTags(String sid, long folderId, String[] tags) throws Exception {
		client.addFolderTags(sid, folderId, tags);
	}

	@Override
	public String[] getFolderTags(String sid, long folderId) throws Exception {
		return client.getFolderTags(sid, folderId);
	}

	@Override
	public String[] getTags(String sid) throws Exception {
		return client.getTags(sid);
	}

	@Override
	public WSTagCloud[] getTagCloud(String sid) throws Exception {
		return client.getTagCloud(sid);
	}

	@Override
	public WSDocument[] findDocumentsByTag(String sid, String tag) throws Exception {
		return client.findDocumentsByTag(sid, tag);
	}

	@Override
	public WSFolder[] findFoldersByTag(String sid, String tag) throws Exception {
		return client.findFoldersByTag(sid, tag);
	}

	@Override
	public String[] getTagsPreset(String sid) throws Exception {
		return client.getTagsPreset(sid);
	}

}
