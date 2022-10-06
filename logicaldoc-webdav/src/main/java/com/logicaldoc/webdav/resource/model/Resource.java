package com.logicaldoc.webdav.resource.model;

import java.io.InputStream;
import java.util.Date;

import com.logicaldoc.webdav.session.DavSession;

/**
 * Main-Class that contains all information about one particular resource within
 * logicalDOC. Important attributes are ID, Name, RequestedPerson <b>ID</b>
 * identifies a resource against logicalDOC.<br>
 * <b>Name</b> Is the Title of a given resource that appears on the client site
 * as "file name"<br>
 * <b>RequestedPerson</b> shows the user that wants to do something with this
 * resource. Therefore the passed ID corresponds with the userid within
 * logicalDOC. Secure handlings will be managed through this.
 * 
 * @author Sebastian Wenzky
 */
public interface Resource {

	public void setID(String ID);

	public String getID();

	public Long getContentLength();

	public String getName();

	public void setContentLength(Long contentLength);

	public void setName(String name);

	public boolean isFolder();

	public boolean isWorkspace();

	public boolean isLocked();

	public boolean isDeleteEnabled();

	public boolean isRenameEnabled();

	public boolean isDownloadEnabled();

	public boolean isWriteEnabled();

	public boolean isMoveEnabled();
	
	public boolean isAddChildEnabled();

	public void setDeleteEnabled(boolean deleteEnabled);

	public void setDownloadEnabled(boolean downloadEnabled);

	public void setRenameEnabled(boolean renameEnabled);

	public void setWriteEnabled(boolean writeEnabled);

	public void setAddChildEnabled(boolean renameEnabled);

	public void setLocked(boolean locked);

	public void isFolder(boolean isFolder);

	public void isWorkspace(boolean isWorkspace);

	public void setInputStream(InputStream is);

	public InputStream getInputStream();

	public void setRequestedPerson(long id);

	public long getRequestedPerson();

	public boolean isCheckedOut();

	public void setCheckedOut(boolean checkedOut);

	public void setVersionLabel(String versionLabel);

	public String getVersionLabel();

	public Date getLastModified();

	public void setLastModified(Date lastModified);

	public void setVersionDate(Date date);

	public Date getVersionDate();

	public String getAuthor();

	public void setAuthor(String author);

	public String getComment();

	public void setComment(String comment);

	public void setCreationDate(Date creation);

	public Date getCreationDate();

	public DavSession getSession();

	public void setSession(DavSession session);

	public Long getDocRef();

	public void setDocRef(Long docRef);

	public void setFolderID(String folderID);

	public String getFolderID();

	public String getLockUser();

	public void setLockUser(String lockUser);

	public void setETag(String string);
	
	public String getETag();
}
