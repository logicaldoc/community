package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneCanvas;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;

/**
 * A canvas that contains a note to be displayed in a page of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ZonalOCRZoneCanvas extends ZoneCanvas {

	/**
	 * Constructor
	 * 
	 * @param zone the zone associated to this canvas
	 * @param ocrPanel the OCR panel with the zones editor
	 */
	public ZonalOCRZoneCanvas(GUIZone zone, ZonalOCRTemplatesPanel ocrPanel) {
		super(zone, ocrPanel);
	}

	@Override
	public void onEdit() {
		if (zone.getSample() == null || zone.getSampleText() == null) {
			LD.contactingServer();
			ZonalOCRService.Instance.get().updateZone(zone, new AsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
					LD.clearPrompt();
				}

				@Override
				public void onSuccess(GUIZone newZone) {
					LD.clearPrompt();

					ZonalOCRZoneCanvas.this.zone.setSample(newZone.getSample());
					ZonalOCRZoneCanvas.this.zone.setSampleText(newZone.getSampleText());
					ZoneEditor editor = new ZoneEditor(zone);
					editor.show();
				}
			});
		} else {
			ZoneEditor editor = new ZoneEditor(zone);
			editor.show();
		}
	}
}