package com.logicaldoc.gui.common.client.beans;

import java.util.ArrayList;
import java.util.List;

/**
 * This user interface bean to model a barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIBarcodeTemplate extends GUIOCRTemplate {

	private static final long serialVersionUID = 1L;

	private Integer threshold = 100;

	private Integer rendRes = 100;

	private boolean zonal = false;

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

	public Integer getRendRes() {
		return rendRes;
	}

	public void setRendRes(Integer rendRes) {
		this.rendRes = rendRes;
	}

	public void removeBarcodeZone(int index) {
		if (getZones() == null || getZones().length < 1 || index > getZones().length - 1)
			return;

		List<GUIBarcodeZone> newSpecs = new ArrayList<>();
		for (int i = 0; i < getZones().length; i++) {
			if (i != index)
				newSpecs.add((GUIBarcodeZone) getZones()[i]);
		}

		setZones(newSpecs.toArray(new GUIBarcodeZone[0]));
	}

	public void appendBarcodeZone(GUIBarcodeZone a) {
		List<GUIBarcodeZone> newSpecs = new ArrayList<>();
		if (getZones() != null)
			for (GUIZone barcodeZone : getZones())
				newSpecs.add((GUIBarcodeZone)barcodeZone);
		newSpecs.add(a);
		setZones(newSpecs.toArray(new GUIBarcodeZone[0]));
	}
}