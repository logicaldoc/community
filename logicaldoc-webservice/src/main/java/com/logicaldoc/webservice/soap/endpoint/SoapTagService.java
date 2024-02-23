package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.TagCloud;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.TagService;

/**
 * Tag Web Service Implementation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class SoapTagService extends AbstractService implements TagService {
	protected static Logger log = LoggerFactory.getLogger(SoapTagService.class);

	private SoapDocumentService getDocumentService() {
		SoapDocumentService docService = new SoapDocumentService();
		docService.setValidateSession(isValidateSession());
		return docService;
	}

	private SoapFolderService getFolderService() {
		SoapFolderService folderService = new SoapFolderService();
		folderService.setValidateSession(isValidateSession());
		return folderService;
	}

	@Override
	public void setDocumentTags(String sid, long docId, String[] tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		SoapDocumentService docService = getDocumentService();
		WSDocument doc = docService.getDocument(sid, docId);
		if (doc == null)
			return;

		checkDocumentPermission(Permission.WRITE, validateSession(sid), doc.getId());

		doc.setTags(tags);
		docService.update(sid, doc);
	}

	@Override
	public void addDocumentTags(String sid, long docId, String[] tags) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		SoapDocumentService docService = getDocumentService();
		WSDocument doc = docService.getDocument(sid, docId);
		if (doc == null)
			return;

		checkDocumentPermission(Permission.WRITE, validateSession(sid), doc.getId());
		for (String tag : tags)
			doc.addTag(tag);
		docService.update(sid, doc);
	}

	@Override
	public String[] getDocumentTags(String sid, long docId) throws PermissionException, PersistenceException,
			AuthenticationException, WebserviceException, UnexistingResourceException {
		SoapDocumentService docService = getDocumentService();
		WSDocument doc = docService.getDocument(sid, docId);
		if (doc == null)
			return new String[0];

		checkDocumentPermission(Permission.READ, validateSession(sid), doc.getId());
		return doc.getTags() != null ? doc.getTags() : new String[0];
	}

	@Override
	public void setFolderTags(String sid, long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		SoapFolderService folderService = getFolderService();
		WSFolder folder = folderService.getFolder(sid, folderId);
		if (folder == null)
			return;

		checkFolderPermission(Permission.WRITE, validateSession(sid), folderId);
		folder.setTags(tags);
		folderService.update(sid, folder);
	}

	@Override
	public void addFolderTags(String sid, long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		SoapFolderService folderService = getFolderService();
		WSFolder folder = folderService.getFolder(sid, folderId);
		if (folder == null)
			return;

		checkFolderPermission(Permission.WRITE, validateSession(sid), folderId);
		for (String tag : tags)
			folder.addTag(tag);
		folderService.update(sid, folder);
	}

	@Override
	public String[] getFolderTags(String sid, long folderId)
			throws PermissionException, PersistenceException, AuthenticationException, WebserviceException {
		SoapFolderService folderService = getFolderService();
		WSFolder folder = folderService.getFolder(sid, folderId);
		if (folder == null)
			return new String[0];

		checkFolderPermission(Permission.READ, validateSession(sid), folderId);
		return folder.getTags() != null ? folder.getTags() : new String[0];
	}

	@Override
	public String[] getTags(String sid) throws PersistenceException, AuthenticationException, WebserviceException {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<String> tags = docDao.findAllTags(null, user.getTenantId());
		String[] wsTags = new String[tags.size()];
		for (int i = 0; i < tags.size(); i++) {
			wsTags[i] = tags.get(i);
		}
		return wsTags;
	}

	@Override
	public String[] getTagsPreset(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		Session session = SessionManager.get().get(sid);
		List<String> tags = new ArrayList<>();

		ContextProperties config = Context.get().getProperties();
		String mode = config.getProperty(session.getTenantName() + ".tag.mode");
		if ("preset".equals(mode)) {
			GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			List<Generic> buf = gDao.findByTypeAndSubtype("tag", null, null, session.getTenantId());
			for (Generic generic : buf)
				tags.add(generic.getSubtype());
		}

		String[] wsTags = new String[tags.size()];
		for (int i = 0; i < tags.size(); i++) {
			wsTags[i] = tags.get(i);
		}

		return wsTags;
	}

	@Override
	public WSTagCloud[] getTagCloud(String sid)
			throws PersistenceException, AuthenticationException, WebserviceException {
		validateSession(sid);

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<TagCloud> list = dao.getTagCloud(sid);

		WSTagCloud[] tagClouds = new WSTagCloud[list.size()];
		int i = 0;
		for (TagCloud tag : list) {
			tagClouds[i] = WSTagCloud.fromTagCloud(tag);
			i++;
		}

		return tagClouds;
	}

	@Override
	public WSDocument[] findDocumentsByTag(String sid, String tag)
			throws PersistenceException, AuthenticationException, WebserviceException {
		User user = validateSession(sid);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByUserIdAndTag(user.getId(), tag, null);
		WSDocument[] wsDocs = new WSDocument[docs.size()];
		for (int i = 0; i < docs.size(); i++) {
			try {
				checkPublished(user, docs.get(i));
				checkNotArchived(docs.get(i));
			} catch (Exception e) {
				continue;
			}
			docDao.initialize(docs.get(i));
			wsDocs[i] = WSUtil.toWSDocument(docs.get(i));
		}

		return wsDocs;
	}

	@Override
	public WSFolder[] findFoldersByTag(String sid, String tag)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		List<Folder> folders = folderDao.findByUserIdAndTag(user.getId(), tag, null);
		WSFolder[] wsFolders = new WSFolder[folders.size()];
		for (int i = 0; i < folders.size(); i++) {
			folderDao.initialize(folders.get(i));
			wsFolders[i] = WSFolder.fromFolder(folders.get(i));
		}

		return wsFolders;
	}
}