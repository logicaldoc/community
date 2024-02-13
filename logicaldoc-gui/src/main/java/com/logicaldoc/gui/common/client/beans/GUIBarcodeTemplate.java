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
		if (zones.isEmpty() || index > zones.size() - 1)
			return;

		List<GUIZone> newSpecs = new ArrayList<>();
		for (int i = 0; i < zones.size(); i++) {
			if (i != index)
				newSpecs.add(zones.get(i));
		}

		zones = newSpecs;
	}

	public void appendBarcodeZone(GUIBarcodeZone a) {
		zones.add(a);
	}
}