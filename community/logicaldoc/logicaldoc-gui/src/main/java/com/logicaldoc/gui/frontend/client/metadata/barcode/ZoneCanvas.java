package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeSpec;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.MovedEvent;
import com.smartgwt.client.widgets.events.MovedHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.events.RightMouseDownEvent;
import com.smartgwt.client.widgets.events.RightMouseDownHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * A canvas that contains a note to be displayed in a page of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class ZoneCanvas extends Label {

	private GUIBarcodeSpec zone;

	private BarcodeTemplatesPanel barcodePanel;

	/**
	 * Constructor
	 * 
	 * @param zone the zone associated to this canvas
	 * @param ocrPanel the Barcode panel with the zones editor
	 */
	public ZoneCanvas(GUIBarcodeSpec zone, BarcodeTemplatesPanel barcodePanel) {
		this.zone = zone;
		this.barcodePanel = barcodePanel;

		setOverflow(Overflow.HIDDEN);
		setBorder("1px solid #AAAAAA");
		setBackgroundColor("#FFFF88");
		setOpacity(60);

		setOverflow(Overflow.HIDDEN);
		setBorder("1px solid #999999");

		setContents("<span style='color: blue'>" + Util.escapeHTML(zone.getPatterns()) + "</span>");
		setAlign(Alignment.LEFT);
		setValign(VerticalAlignment.TOP);

		setCanDragReposition(true);
		setDragAppearance(DragAppearance.TARGET);
		setCanDragResize(true);
		setSmoothFade(true);

		ImageWithCanvases sample = barcodePanel.getZonalCanvas();
		setLeft(sample.computeLeftPixel(zone.getLeft()));
		setTop(sample.computeTopPixel(zone.getTop()));
		setWidth(sample.computeWidthPixels(zone.getWidth()));
		setHeight(sample.computeHeightPixels(zone.getHeight()));

		addDoubleClickHandler(new DoubleClickHandler() {

			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				showContextMenu();
			}
		});

		addRightMouseDownHandler(new RightMouseDownHandler() {

			@Override
			public void onRightMouseDown(RightMouseDownEvent event) {
				showContextMenu();
			}
		});

		addMovedHandler(new MovedHandler() {

			@Override
			public void onMoved(MovedEvent event) {
				captureZonePosition();
			}
		});

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				captureZonePosition();
			}
		});
	}

	private void onEdit() {
		if (zone.getSample() == null || zone.getSampleText() == null) {
			ContactingServer.get().show();
			BarcodeService.Instance.get().updateZone(zone, new AsyncCallback<GUIBarcodeSpec>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
					ContactingServer.get().hide();
				}

				@Override
				public void onSuccess(GUIBarcodeSpec newZone) {
					ContactingServer.get().hide();

					ZoneCanvas.this.zone.setSample(newZone.getSample());
					ZoneCanvas.this.zone.setSampleText(newZone.getSampleText());
					ZoneEditor editor = new ZoneEditor(zone);
					editor.show();
				}
			});
		} else {
			ZoneEditor editor = new ZoneEditor(zone);
			editor.show();
		}
	}

	private void captureZonePosition() {
		ImageWithCanvases sample = barcodePanel.getZonalCanvas();
		zone.setLeft(sample.getLeft(getLeft()));
		zone.setTop(sample.getTop(getTop()));
		zone.setWidth(sample.getWidth(getWidth()));
		zone.setHeight(sample.getHeight(getHeight()));

		zone.setSample(null);
		zone.setSampleText(null);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							barcodePanel.getSelectedBarcodeTemplate().removeBarcodeSpec(zone.getIndex());
							barcodePanel.getZonalCanvas().clearCanvases();
							barcodePanel.showZones();
						}
					}
				});
			}
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onEdit();
			}
		});

		contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}
}