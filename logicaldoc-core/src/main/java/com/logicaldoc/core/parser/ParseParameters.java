package com.logicaldoc.core.parser;

import java.util.Locale;

import com.logicaldoc.core.document.Document;

/**
 * Some parameters to parse documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class ParseParameters {

	private Document document;

	private String fileName;

	private String fileVersion;

	private String encoding;

	private Locale locale;

	private String tenant;

	public ParseParameters(Document document, String filename, String fileVersion, String encoding, Locale locale,
			String tenant) {
		this.document = document;
		this.fileName = filename;
		this.fileVersion = fileVersion;
		this.encoding = encoding;
		this.locale = locale;
		this.tenant = tenant;
	}

	public Document getDocument() {
		return document;
	}

	public void setDocument(Document document) {
		this.document = document;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getEncoding() {
		return encoding;
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public String getTenant() {
		return tenant;
	}

	public void setTenant(String tenant) {
		this.tenant = tenant;
	}
}