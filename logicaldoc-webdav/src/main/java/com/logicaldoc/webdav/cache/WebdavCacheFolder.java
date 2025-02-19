package com.logicaldoc.webdav.cache;

/**
 * An entry in the webdav cache
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1.1
 */
public class WebdavCacheFolder {

	private long id;

	private String path;

	private long size;

	public WebdavCacheFolder() {
		super();
	}

	public WebdavCacheFolder(long folderId, String path, long folderSize) {
		this.id = folderId;
		this.path = path;
		this.size = folderSize;
	}

	public long getId() {
		return id;
	}

	public String getPath() {
		return path;
	}

	public long getSize() {
		return size;
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public void setSize(long size) {
		this.size = size;
	}
}