package com.logicaldoc.core.document;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;

/**
 * A bookmark over a document
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.2
 * 
 */
public class Bookmark extends PersistentObject {
	public static final int TYPE_DOCUMENT = 0;

	public static final int TYPE_FOLDER = 1;

	private long userId;

	private long targetId;

	private String title = "";

	private String description = "";

	private int position = 0;

	// The document file extension
	private String fileType;

	private int type = TYPE_DOCUMENT;

	public Bookmark() {
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public long getTargetId() {
		return targetId;
	}

	public void setTargetId(long targetId) {
		this.targetId = targetId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getFileType() {
		return fileType;
	}

	public void setFileType(String fileType) {
		this.fileType = fileType;
	}

	/**
	 * The icon for the document associated to the subscription
	 */
	public String getIcon() {
		String icon = IconSelector.selectIcon("");
		try {
			icon = IconSelector.selectIcon(getFileType());
		} catch (Exception e) {
		}
		return icon;
	}

	/**
	 * The path of the document associated to the bookmark.
	 */
	public String getPath() {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		return folderDao.computePathExtended(docDao.findById(targetId).getFolder().getId());
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}
}
