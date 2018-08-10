package com.logicaldoc.core.communication;

/**
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 */
public class EMailAttachment {

	private String icon = "";

	private byte[] data;

	private long size;

	private String mimeType = "";

	private String fileName = "";

	public EMailAttachment() {
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String string) {
		mimeType = string;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public byte[] getData() {
		return data;
	}

	public void setData(byte[] data) {
		this.data = data;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}
}
