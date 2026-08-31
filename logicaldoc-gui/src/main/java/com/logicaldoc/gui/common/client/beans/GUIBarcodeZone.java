package com.logicaldoc.gui.common.client.beans;

import java.util.Date;

import com.logicaldoc.gui.common.client.util.Util;

/**
 * This the model for a barcode specification
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIBarcodeZone extends GUIZone {

	private static final long serialVersionUID = 1L;

	private int index = 0;

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


	@Override
	public String getName() {
		return Integer.toString(index);
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

	
	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	@Override
	public String getSampleUrl() {
		return Util.contextPath() + "barcodetemplateimage/" + getTemplateId() + "?zone=" + index + "&random="
				+ new Date().getTime();
	}
	
	@Override
	public String getDisplayContent() {
		return getFormats();		
	}
}