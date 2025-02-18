package com.logicaldoc.webdav.resource.model;

import java.io.InputStream;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.util.Context;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * Implementation of a generic WebDAV resource
 * 
 * @see Resource
 * 
 * @author Sebastian Wenzky
 */
public class ResourceImpl implements Resource {

	private static final Logger log = LoggerFactory.getLogger(ResourceImpl.class);

	private String id;

	private String name;

	private Long contentLength;

	private boolean folder;

	private boolean workspace;

	private boolean locked = false;

	private boolean checkedOut = false;
	
	private InputStream is;

	private long personRequest;

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

	WebdavSession session;

	private String eTag;
	
	private long size;

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

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public void setWorkspace(boolean workspace) {
		this.workspace = workspace;
	}

	public boolean isFolder() {
		return this.folder;
	}

	public boolean isWorkspace() {
		return this.workspace;
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public boolean isLocked() {
		return this.locked || this.checkedOut;
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
		return this.checkedOut;
	}

	@Override
	public void setCheckedOut(boolean isCheckedOut) {
		this.checkedOut = isCheckedOut;
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

	@Override
	public void setCreationDate(Date creation) {
		this.creationDate = creation;
	}

	@Override
	public Date getCreationDate() {
		return this.creationDate;
	}

	@Override
	public boolean isDeleteEnabled() {
		initPermissions();
		return this.deleteEnabled;
	}

	@Override
	public boolean isMoveEnabled() {
		initPermissions();
		return this.moveEnabled;
	}

	@Override
	public boolean isRenameEnabled() {
		initPermissions();
		return this.renameEnabled;
	}

	@Override
	public void setDeleteEnabled(boolean deleteEnabled) {
		this.deleteEnabled = deleteEnabled;
	}

	@Override
	public void setRenameEnabled(boolean renameEnabled) {
		this.renameEnabled = renameEnabled;
	}

	@Override
	public boolean isWriteEnabled() {
		initPermissions();
		return writeEnabled;
	}

	@Override
	public void setWriteEnabled(boolean writeEnabled) {
		this.writeEnabled = writeEnabled;
	}

	@Override
	public boolean isAddChildEnabled() {
		initPermissions();
		return this.addChildEnabled;
	}

	@Override
	public void setAddChildEnabled(boolean addChildEnabled) {
		this.addChildEnabled = addChildEnabled;
	}

	@Override
	public WebdavSession getSession() {
		return session;
	}

	@Override
	public void setSession(WebdavSession session) {
		this.session = session;
	}

	private void initPermissions() {
		if (writeEnabled != null)
			return;

		if (personRequest == 0) {
			personRequest = (Long) session.getObject("id");
		}

		FolderDAO fdao = Context.get(FolderDAO.class);
		Set<Permission> permissions = new HashSet<>();
		long fid = 0;
		if (folder)
			fid = Long.parseLong(id);
		else
			fid = Long.parseLong(folderId);

		Folder fold;
		try {
			fold = fdao.findById(fid);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return;
		}
		workspace = fold.getType() == Folder.TYPE_WORKSPACE;

		long rootId = 0;
		try {
			rootId = fdao.findRoot(fold.getTenantId()).getId();
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return;
		}

		if (fid == rootId) {
			writeEnabled = false;
			deleteEnabled = false;
			renameEnabled = false;
			addChildEnabled = false;
			downloadEnabled = true;
			moveEnabled = true;
		} else {
			try {
				permissions = fdao.getAllowedPermissions(fid, personRequest);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
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
	

	@Override
	public void setSize(long size) {
		this.size = size;
	}
	@Override
	public long getSize() {
		return size;
	}	
}