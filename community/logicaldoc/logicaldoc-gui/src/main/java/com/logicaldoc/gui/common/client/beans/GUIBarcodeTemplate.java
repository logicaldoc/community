package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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

	private Integer threshold = 100;
	
	private Integer rendRes = 100;

	private boolean zonal = false;

	private String sample;

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

	public Integer getThreshold() {
		return threshold;
	}

	public void setThreshold(Integer threshold) {
		this.threshold = threshold;
	}

	public boolean isZonal() {
		return zonal;
	}

	public void setZonal(boolean zonal) {
		this.zonal = zonal;
	}

	public String getSample() {
		return sample;
	}

	public void setSample(String sample) {
		this.sample = sample;
	}

	public void removeBarcodeSpec(int index) {
		if (barcodeSpecs == null || barcodeSpecs.length < 1 || index > barcodeSpecs.length - 1)
			return;

		List<GUIBarcodeSpec> newSpecs = new ArrayList<GUIBarcodeSpec>();
		for (int i = 0; i < barcodeSpecs.length; i++) {
			if (i != index)
				newSpecs.add(barcodeSpecs[i]);
		}

		barcodeSpecs = newSpecs.toArray(new GUIBarcodeSpec[0]);
	}

	public void appendBarcodeSpec(GUIBarcodeSpec a) {
		List<GUIBarcodeSpec> newSpecs = new ArrayList<GUIBarcodeSpec>();
		if (getBarcodeSpecs() != null)
			newSpecs.addAll(Arrays.asList(getBarcodeSpecs()));
		newSpecs.add(a);
		barcodeSpecs = newSpecs.toArray(new GUIBarcodeSpec[0]);
	}

	public Integer getRendRes() {
		return rendRes;
	}

	public void setRendRes(Integer rendRes) {
		this.rendRes = rendRes;
	}
}