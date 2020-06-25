package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * This the model for a barcode specification
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIBarcodeSpec implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * Comma separated list of patterns
	 */
	private String patterns;

	/**
	 * Regular expression used as inclusion filter
	 */
	private String include;

	/**
	 * Regular expression used as exclusion filter
	 */
	private String exclude;

	/**
	 * Comma separated list of admitted barcode types (eg: CODE_128,QR_CODE)
	 */
	private String formats;

	public GUIBarcodeSpec() {

	}

	public String getPatterns() {
		return patterns;
	}

	public void setPatterns(String patterns) {
		this.patterns = patterns;
	}

	public String getInclude() {
		return include;
	}

	public void setInclude(String include) {
		this.include = include;
	}

	public String getExclude() {
		return exclude;
	}

	public void setExclude(String exclude) {
		this.exclude = exclude;
	}

	public String getFormats() {
		return formats;
	}

	public void setFormats(String formats) {
		this.formats = formats;
	}
}