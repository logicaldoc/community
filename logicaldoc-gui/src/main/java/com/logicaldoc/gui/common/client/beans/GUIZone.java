package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.util.Util;

/**
 * This is the model for a Zone used in the OCR Template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIZone implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * Name of the zone
	 */
	private String name;

	/**
	 * Type of the zone, taken from {@link GUIAttribute#getType()}
	 */
	private int type;

	private long templateId;

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
	 * An optional format used to parse the value
	 */
	private String format;

	/**
	 * The decimal separator used in case of numbers
	 */
	private String decimalSeparator = ",";

	/**
	 * The grouping separator used in case of numbers
	 */
	private String groupingSeparator = ".";

	/**
	 * Automation script that will be invoked to parse the value
	 */
	private String parsing;

	/**
	 * The image cropped from the sample
	 */
	private String sample;

	/**
	 * The text extracted from the sample
	 */
	private String sampleText;

	/**
	 * The language in which the zone content is written
	 */
	private String language;

	public GUIZone() {

	}

	public GUIZone(String name, int type) {
		super();
		this.name = name;
		this.type = type;
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

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public String getDecimalSeparator() {
		return decimalSeparator;
	}

	public void setDecimalSeparator(String decimalSeparator) {
		this.decimalSeparator = decimalSeparator;
	}

	public String getGroupingSeparator() {
		return groupingSeparator;
	}

	public void setGroupingSeparator(String groupingSeparator) {
		this.groupingSeparator = groupingSeparator;
	}

	public String getParsing() {
		return parsing;
	}

	public void setParsing(String parsing) {
		this.parsing = parsing;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(long templateId) {
		this.templateId = templateId;
	}

	public String getSampleUrl() {
		return Util.contextPath() + "ocrtemplateimage/" + templateId + "?zone=" + name + "&random="
				+ new Date().getTime();
	}

	public String getSampleText() {
		return sampleText;
	}

	public void setSampleText(String sampleText) {
		this.sampleText = sampleText;
	}

	public String getSample() {
		return sample;
	}

	public void setSample(String sample) {
		this.sample = sample;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}
}