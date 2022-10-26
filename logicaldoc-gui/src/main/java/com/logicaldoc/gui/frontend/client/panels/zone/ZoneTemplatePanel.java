package com.logicaldoc.gui.frontend.client.panels.zone;

import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A generic panel that displays zones
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 *
 */
public abstract class ZoneTemplatePanel extends VLayout {

	protected ImageWithCanvases sample;

	protected GUIOCRTemplate selectedOcrTemplate;

	public ZoneTemplatePanel() {
		super();
	}

	public ImageWithCanvases getSample() {
		return sample;
	}

	public GUIOCRTemplate getSelectedOcrTemplate() {
		return selectedOcrTemplate;
	}
	
	public void setSelectedOcrTemplate(GUIOCRTemplate selectedOcrTemplate) {
		this.selectedOcrTemplate = selectedOcrTemplate;
	}

	public void showZones() {
		if (selectedOcrTemplate.getZones() != null)
			for (GUIZone zone : selectedOcrTemplate.getZones()) {		
				zone.setTemplateId(selectedOcrTemplate.getId());
				ZoneCanvas zoneCanvas = newZoneCanvas(zone);
				sample.addCanvas(zoneCanvas);
			}
	}

	/**
	 * Factory method for instantiating the correct ZoneCanvas for the given
	 * Zone
	 * 
	 * @param zone the zone to use
	 * 
	 * @return the ZoneCanvas instance
	 */
	protected abstract ZoneCanvas newZoneCanvas(GUIZone zone);
}