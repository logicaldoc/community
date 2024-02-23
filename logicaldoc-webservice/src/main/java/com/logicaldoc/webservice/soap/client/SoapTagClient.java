package com.logicaldoc.webservice.soap.client;

import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
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

	public SoapTagClient(String endpoint, int gzipThreshold, boolean log, int timeout) {
		super(endpoint, TagService.class, gzipThreshold, log, timeout);
	}

	public SoapTagClient(String endpoint) {
		super(endpoint, TagService.class, -1, true, -1);
	}

	@Override
	public void setDocumentTags(String sid, long docId, String[] tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.setDocumentTags(sid, docId, tags);
	}

	@Override
	public void addDocumentTags(String sid, long docId, String[] tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.addDocumentTags(sid, docId, tags);
	}

	@Override
	public String[] getDocumentTags(String sid, long docId) throws PermissionException, AuthenticationException,
			PersistenceException, WebserviceException, UnexistingResourceException {
		return client.getDocumentTags(sid, docId);
	}

	@Override
	public void setFolderTags(String sid, long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.setFolderTags(sid, folderId, tags);
	}

	@Override
	public void addFolderTags(String sid, long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.addFolderTags(sid, folderId, tags);
	}

	@Override
	public String[] getFolderTags(String sid, long folderId)
			throws PermissionException, AuthenticationException, PersistenceException, WebserviceException {
		return client.getFolderTags(sid, folderId);
	}

	@Override
	public String[] getTags(String sid) throws AuthenticationException, PersistenceException, WebserviceException {
		return client.getTags(sid);
	}

	@Override
	public WSTagCloud[] getTagCloud(String sid)
			throws AuthenticationException, PersistenceException, WebserviceException {
		return client.getTagCloud(sid);
	}

	@Override
	public WSDocument[] findDocumentsByTag(String sid, String tag)
			throws AuthenticationException, PersistenceException, WebserviceException {
		return client.findDocumentsByTag(sid, tag);
	}

	@Override
	public WSFolder[] findFoldersByTag(String sid, String tag)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.findFoldersByTag(sid, tag);
	}

	@Override
	public String[] getTagsPreset(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getTagsPreset(sid);
	}

}
