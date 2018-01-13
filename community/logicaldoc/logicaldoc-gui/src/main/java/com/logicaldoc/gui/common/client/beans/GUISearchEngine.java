package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a search engine handled by the GUI
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class GUISearchEngine implements Serializable {

	private static final long serialVersionUID = 1L;

	private String languages = "standard, en, it, de, fr, es";

	private boolean locked;

	private String includePatters;

	private String excludePatters = "*.exe,*.bin,*.iso,*.zip,*.rar";

	private Integer batch = 200;

	private Integer parsingTimeout;

	private Integer maxText;

	private Integer maxTextFileSize;

	private String dir;

	public String getLanguages() {
		return languages;
	}

	public void setLanguages(String languages) {
		this.languages = languages;
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public String getIncludePatters() {
		return includePatters;
	}

	public void setIncludePatters(String includePatters) {
		this.includePatters = includePatters;
	}

	public String getExcludePatters() {
		return excludePatters;
	}

	public void setExcludePatters(String excludePatters) {
		this.excludePatters = excludePatters;
	}

	public Integer getBatch() {
		return batch;
	}

	public void setBatch(Integer batch) {
		this.batch = batch;
	}

	public Integer getParsingTimeout() {
		return parsingTimeout;
	}

	public void setParsingTimeout(Integer parsingTimeout) {
		this.parsingTimeout = parsingTimeout;
	}

	public Integer getMaxText() {
		return maxText;
	}

	public void setMaxText(Integer maxText) {
		this.maxText = maxText;
	}

	public String getDir() {
		return dir;
	}

	public void setDir(String dir) {
		this.dir = dir;
	}

	public Integer getMaxTextFileSize() {
		return maxTextFileSize;
	}

	public void setMaxTextFileSize(Integer maxTextFileSize) {
		this.maxTextFileSize = maxTextFileSize;
	}
}