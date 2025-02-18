package com.logicaldoc.webdav.cache;

public class WDCacheFolder {
	
	public long id;
	public String path;
	public long size;
	
	public WDCacheFolder(long folderID, String path, long folderSize) {
		this.id = folderID;
		this.path = path;
		this.size = folderSize;
	}

	public WDCacheFolder() {
	}	

}
