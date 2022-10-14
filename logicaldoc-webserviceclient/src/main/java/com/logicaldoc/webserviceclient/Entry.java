package com.logicaldoc.webserviceclient;

import java.io.File;
import java.util.Date;

/**
 * An entry for checkin operations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class Entry {
	private long id;

	private String fileName;

	private String version;

	private File file;

	private Date downloaded = new Date();
	
	public final static int CHECKEDIN = 0;
	
	public final static int LOCALLY_MODIFIED = 1;
	
	public Entry(long id) {
		super();
		this.id = id;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	@Override
	public String toString() {
		return fileName;
	}

	public File getFile() {
		return file;
	}

	public void setFile(File file) {
		this.file = file;
	}

	public Date getDownloaded() {
		return downloaded;
	}

	public void setDownloaded(Date downloaded) {
		this.downloaded = downloaded;
	}
	
	public int getStatus(){
		if(downloaded.before(new Date(getFile().lastModified())))
			return LOCALLY_MODIFIED;
		else
			return CHECKEDIN;
	}
}