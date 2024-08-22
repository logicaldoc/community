package com.logicaldoc.core.document;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;

/**
 * A bookmark over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class Bookmark extends PersistentObject {

	private static Logger log = LoggerFactory.getLogger(Bookmark.class);

	private static final long serialVersionUID = 1L;

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
	 * 
	 * @return name of the icon file
	 */
	public String getIcon() {
		String icon = IconSelector.selectIcon("");
		try {
			icon = IconSelector.selectIcon(getFileType());
		} catch (Exception e) {
			// Nothing to do
		}
		return icon;
	}

	/**
	 * The path of the document associated to the bookmark.
	 * 
	 * @return full path to the document
	 */
	public String getPath() {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			return folderDao.computePathExtended(docDao.findById(targetId).getFolder().getId());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		result = prime * result + type;
		result = prime * result + (int) (userId ^ (userId >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Bookmark other = (Bookmark) obj;
		if (title == null) {
			if (other.title != null)
				return false;
		} else if (!title.equals(other.title))
			return false;
		if (type != other.type)
			return false;
		return userId == other.userId;
	}
}
