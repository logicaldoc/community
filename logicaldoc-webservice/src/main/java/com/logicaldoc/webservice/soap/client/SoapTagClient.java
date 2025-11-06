package com.logicaldoc.webservice.soap.client;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;
import com.logicaldoc.webservice.soap.TagService;

import jakarta.jws.WebService;

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
	public void setDocumentTags(String sid, long docId, List<String> tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.setDocumentTags(sid, docId, tags);
	}

	@Override
	public void addDocumentTags(String sid, long docId, List<String> tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.addDocumentTags(sid, docId, tags);
	}

	@Override
	public List<String> getDocumentTags(String sid, long docId) throws PermissionException, AuthenticationException,
			PersistenceException, WebserviceException, UnexistingResourceException {
		return client.getDocumentTags(sid, docId);
	}

	@Override
	public void setFolderTags(String sid, long folderId, List<String> tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.setFolderTags(sid, folderId, tags);
	}

	@Override
	public void addFolderTags(String sid, long folderId, List<String> tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.addFolderTags(sid, folderId, tags);
	}

	@Override
	public List<String> getFolderTags(String sid, long folderId)
			throws PermissionException, AuthenticationException, PersistenceException, WebserviceException {
		return client.getFolderTags(sid, folderId);
	}

	@Override
	public List<String> getTags(String sid) throws AuthenticationException, PersistenceException, WebserviceException {
		final List<String> tags = client.getTags(sid);
		if (tags != null)
			return tags;
		else
			return new ArrayList<>();
	}

	@Override
	public List<WSTagCloud> getTagCloud(String sid)
			throws AuthenticationException, PersistenceException, WebserviceException {
		final List<WSTagCloud> tags = client.getTagCloud(sid);
		if (tags != null)
			return tags;
		else
			return new ArrayList<>();
	}

	@Override
	public List<WSDocument> findDocumentsByTag(String sid, String tag)
			throws AuthenticationException, PersistenceException, WebserviceException {
		final List<WSDocument> documents = client.findDocumentsByTag(sid, tag);
		if (documents != null)
			return documents;
		else
			return new ArrayList<>();
	}

	@Override
	public List<WSFolder> findFoldersByTag(String sid, String tag)
			throws AuthenticationException, WebserviceException, PersistenceException {
		final List<WSFolder> folders = client.findFoldersByTag(sid, tag);
		if (folders != null)
			return folders;
		else
			return new ArrayList<>();
	}

	@Override
	public List<String> getTagsPreset(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		final List<String> tags = client.getTagsPreset(sid);
		if (tags != null)
			return tags;
		else
			return new ArrayList<>();
	}
}