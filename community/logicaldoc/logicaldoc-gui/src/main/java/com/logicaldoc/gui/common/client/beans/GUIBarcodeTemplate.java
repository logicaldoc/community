package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * This user interface bean to model a barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIBarcodeTemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	/**
	 * Unique name of the template
	 */
	private String name;

	private String description;

	/**
	 * The assigned document template
	 */
	private GUITemplate template;

	/**
	 * The barcode specifications
	 */
	private GUIBarcodeSpec[] barcodeSpecs = new GUIBarcodeSpec[0];

	private Integer batch = 200;

	private Integer imageThreshold = 900;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public GUITemplate getTemplate() {
		return template;
	}

	public void setTemplate(GUITemplate template) {
		this.template = template;
	}

	public GUIBarcodeSpec[] getBarcodeSpecs() {
		return barcodeSpecs;
	}

	public void setBarcodeSpecs(GUIBarcodeSpec[] barcodeSpecs) {
		this.barcodeSpecs = barcodeSpecs;
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