package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a barcode engine handled by the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class GUIBarcodeEngine implements Serializable {

	private static final long serialVersionUID = 1L;

	private String includePatters;

	private String excludePatters;

	private Integer batch = 200;

	private Integer imageThreshold = 900;

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

	public Integer getImageThreshold() {
		return imageThreshold;
	}

	public void setImageThreshold(Integer imageThreshold) {
		this.imageThreshold = imageThreshold;
	}
}