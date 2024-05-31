package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a search engine handled by the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUISearchEngine implements Serializable {

	private static final long serialVersionUID = 1L;

	private String languages = "standard, en, it, de, fr, es";

	private boolean locked;

	private String includePatterns;

	private String excludePatterns;

	private String includePatternsMetadata;

	private String excludePatternsMetadata = "*.exe,*.bin,*.iso,*.zip,*.rar";

	private Integer batch = 200;

	private Integer parsingTimeout;
	
	private boolean parsingTimeoutRetain=true;

	private Integer maxText;

	private Integer maxTextFileSize;

	private String dir;

	private String sorting;

	private String customSorting;

	private int threads = 2;

	private boolean skipOnError = false;

	public int getThreads() {
		return threads;
	}

	public void setThreads(int threads) {
		this.threads = threads;
	}

	public String getSorting() {
		return sorting;
	}

	public void setSorting(String sorting) {
		this.sorting = sorting;
	}

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
		return includePatterns;
	}

	public void setIncludePatterns(String includePatterns) {
		this.includePatterns = includePatterns;
	}

	public String getExcludePatters() {
		return excludePatterns;
	}

	public void setExcludePatterns(String excludePatterns) {
		this.excludePatterns = excludePatterns;
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

	public String getIncludePattersMetadata() {
		return includePatternsMetadata;
	}

	public void setIncludePatternsMetadata(String includePatternsMetadata) {
		this.includePatternsMetadata = includePatternsMetadata;
	}

	public String getExcludePatternsMetadata() {
		return excludePatternsMetadata;
	}

	public void setExcludePatternsMetadata(String excludePattersMetadata) {
		this.excludePatternsMetadata = excludePattersMetadata;
	}

	public String getCustomSorting() {
		return customSorting;
	}

	public void setCustomSorting(String customSorting) {
		this.customSorting = customSorting;
	}

	public boolean isSkipOnError() {
		return skipOnError;
	}

	public void setSkipOnError(boolean skipOnError) {
		this.skipOnError = skipOnError;
	}

	public boolean isParsingTimeoutRetain() {
		return parsingTimeoutRetain;
	}

	public void setParsingTimeoutRetain(boolean parsingTimeoutRetain) {
		this.parsingTimeoutRetain = parsingTimeoutRetain;
	}
}