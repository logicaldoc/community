package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.util.Util;

/**
 * This the model for a barcode specification
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIBarcodeSpec implements Serializable {

	private static final long serialVersionUID = 1L;

	private int index = 0;

	private long templateId = 0;

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

	/**
	 * Upper-left corner of the zone (percentage of the width 0..1)
	 */
	private double left = 0.01;

	/**
	 * Upper-left corner of the zone (percentage of the height 0..1)
	 */
	private double top = 0.01;

	/**
	 * Width of the zone (percentage of the width 0..1)
	 */
	private double width = 0.15;

	/**
	 * Height of the zone (percentage of the height 0..1)
	 */
	private double height = 0.02;

	/**
	 * The image cropped from the sample
	 */
	private String sample;

	/**
	 * The text extracted from the sample
	 */
	private String sampleText;

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

	public double getLeft() {
		return left;
	}

	public void setLeft(double left) {
		this.left = left;
	}

	public double getTop() {
		return top;
	}

	public void setTop(double top) {
		this.top = top;
	}

	public double getWidth() {
		return width;
	}

	public void setWidth(double width) {
		this.width = width;
	}

	public double getHeight() {
		return height;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public String getSample() {
		return sample;
	}

	public void setSample(String sample) {
		this.sample = sample;
	}

	public String getSampleText() {
		return sampleText;
	}

	public void setSampleText(String sampleText) {
		this.sampleText = sampleText;
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public String getSampleUrl() {
		return Util.contextPath() + "barcodetemplateimage/" + templateId + "?zone=" + index + "&random="
				+ new Date().getTime();
	}

	public long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(long templateId) {
		this.templateId = templateId;
	}
}