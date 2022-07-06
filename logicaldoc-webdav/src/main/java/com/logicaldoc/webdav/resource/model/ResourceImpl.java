package com.logicaldoc.webdav.resource.model;

import java.io.InputStream;
import java.util.Date;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.util.Context;
import com.logicaldoc.webdav.session.DavSession;

/**
 * @see Resource
 * 
 * @author Sebastian Wenzky
 */
public class ResourceImpl implements Resource {

	protected static Logger log = LoggerFactory.getLogger(ResourceImpl.class);

	private String id;

	private String name;

	private Long contentLength;

	private boolean isFolder;

	private boolean isWorkspace;

	private boolean isLocked = false;

	private InputStream is;

	private long personRequest;

	private boolean isCheckedOut = false;

	private String versionLabel;

	private Date lastModified;

	private Date versionDate;

	private String lockUser;

	private String author;

	private String comment;

	private Date creationDate;

	private Long docRef;

	private String folderId;

	private Boolean writeEnabled;

	private Boolean deleteEnabled;

	private Boolean renameEnabled;

	private Boolean addChildEnabled;

	private Boolean downloadEnabled;

	private Boolean moveEnabled;

	DavSession session;

	private String eTag;

	public Long getContentLength() {
		return contentLength;
	}

	public String getName() {
		return name;
	}

	public void setContentLength(Long contentLength) {
		this.contentLength = contentLength;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void isFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}

	public void isWorkspace(boolean isWorkspace) {
		this.isFolder = isWorkspace;
	}

	public boolean isFolder() {
		return this.isFolder;
	}

	public boolean isWorkspace() {
		return this.isWorkspace;
	}

	public void setLocked(boolean isLocked) {
		this.isLocked = isLocked;
	}

	public boolean isLocked() {
		// log.debug("isLocked({}): {}", getName(), (this.isLocked ||
		// this.isCheckedOut));
		return this.isLocked || this.isCheckedOut;
	}

	public String getID() {
		return this.id;
	}

	public void setID(String id) {
		this.id = id;
	}

	@Override
	public InputStream getInputStream() {
		return this.is;
	}

	@Override
	public void setInputStream(InputStream is) {
		this.is = is;
	}

	@Override
	public long getRequestedPerson() {
		return this.personRequest;
	}

	@Override
	public void setRequestedPerson(long id) {
		this.personRequest = id;
	}

	@Override
	public boolean isCheckedOut() {
		// log.debug("isCheckedOut({}): {}", getName(), this.isCheckedOut);
		return this.isCheckedOut;
	}

	@Override
	public void setCheckedOut(boolean isCheckedOut) {
		this.isCheckedOut = isCheckedOut;
	}

	@Override
	public String getVersionLabel() {
		return this.versionLabel;
	}

	@Override
	public void setVersionLabel(String versionLabel) {
		this.versionLabel = versionLabel;
	}

	public Date getLastModified() {
		return this.lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	@Override
	public Date getVersionDate() {
		return this.versionDate;
	}

	@Override
	public void setVersionDate(Date date) {
		this.versionDate = date;
	}

	@Override
	public void setAuthor(String author) {
		this.author = author;
	}

	@Override
	public String getAuthor() {
		return this.author;
	}

	@Override
	public void setComment(String comment) {
		this.comment = comment;
	}

	@Override
	public String getComment() {
		return this.comment;
	}

	public void setCreationDate(Date creation) {
		this.creationDate = creation;
	}

	public Date getCreationDate() {
		return this.creationDate;
	}

	public boolean isDeleteEnabled() {
		initPermissions();
		return this.deleteEnabled;
	}

	public boolean isMoveEnabled() {
		initPermissions();
		return this.moveEnabled;
	}

	public boolean isRenameEnabled() {
		initPermissions();
		return this.renameEnabled;
	}

	public void setDeleteEnabled(boolean deleteEnabled) {
		this.deleteEnabled = deleteEnabled;
	}

	public void setRenameEnabled(boolean renameEnabled) {
		this.renameEnabled = renameEnabled;
	}

	public boolean isWriteEnabled() {
		initPermissions();
		return writeEnabled;
	}

	public void setWriteEnabled(boolean writeEnabled) {
		this.writeEnabled = writeEnabled;
	}

	public boolean isAddChildEnabled() {
		initPermissions();
		return this.addChildEnabled;
	}

	public void setAddChildEnabled(boolean addChildEnabled) {
		this.addChildEnabled = addChildEnabled;
	}

	public DavSession getSession() {
		return session;
	}

	public void setSession(DavSession session) {
		this.session = session;
	}

	private void initPermissions() {
		if (writeEnabled != null)
			return;

		if (personRequest == 0) {
			personRequest = (Long) session.getObject("id");
		}

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Set<Permission> permissions = null;
		long fid = 0;
		if (isFolder)
			fid = Long.parseLong(id);
		else
			fid = Long.parseLong(folderId);

		Folder folder;
		try {
			folder = fdao.findById(fid);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return;
		}
		isWorkspace = folder.getType() == Folder.TYPE_WORKSPACE;

		long rootId = fdao.findRoot(folder.getTenantId()).getId();

		if (fid == rootId) {
			writeEnabled = false;
			deleteEnabled = false;
			renameEnabled = false;
			addChildEnabled = false;
			downloadEnabled = true;
			moveEnabled = true;
		} else {
			permissions = fdao.getEnabledPermissions(fid, personRequest);
			writeEnabled = permissions.contains(Permission.WRITE);
			deleteEnabled = permissions.contains(Permission.DELETE);
			renameEnabled = permissions.contains(Permission.RENAME);
			addChildEnabled = permissions.contains(Permission.ADD);
			downloadEnabled = permissions.contains(Permission.DOWNLOAD);
			moveEnabled = permissions.contains(Permission.MOVE);
		}
	}

	public boolean isDownloadEnabled() {
		initPermissions();
		return this.downloadEnabled;
	}

	public void setDownloadEnabled(boolean downloadEnabled) {
		this.downloadEnabled = downloadEnabled;
	}

	@Override
	public Long getDocRef() {
		return this.docRef;
	}

	@Override
	public void setDocRef(Long docRef) {
		this.docRef = docRef;
	}

	@Override
	public String getFolderID() {
		return this.folderId;
	}

	@Override
	public void setFolderID(String folderId) {
		this.folderId = folderId;
	}

	public String getLockUser() {
		return lockUser;
	}

	public void setLockUser(String lockUser) {
		this.lockUser = lockUser;
	}

	@Override
	public void setETag(String eTag) {
		this.eTag = eTag;
	}

	public String getETag() {
		return this.eTag;
	}
}