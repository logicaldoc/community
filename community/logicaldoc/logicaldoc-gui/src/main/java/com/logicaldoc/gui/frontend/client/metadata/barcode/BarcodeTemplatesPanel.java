package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.Date;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeSpec;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.PrintUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the barcodes patterns configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class BarcodeTemplatesPanel extends VLayout {

	private PositionalBarcodesGrid positionalGrid;

	private ImageWithCanvases zonalCanvas;

	private SelectItem templateSelector;

	private SelectItem barcodeTemplateSelector;

	private GUIBarcodeTemplate selectedBarcodeTemplate;

	private GUITemplate selectedDocumentTemplate;

	private ToolStripButton save;

	private ToolStripButton append;

	private ToolStripButton settings;

	private ToolStripButton newTemplate;

	private ToolStripButton delete;

	private ToolStripButton print;

	private ToolStripButton close;

	private ToolStripButton zoomIn;

	private ToolStripButton zoomOut;

	private ToolStrip toolStrip;

	private HLayout editorPanel;
	
	public BarcodeTemplatesPanel(GUITemplate selectedDocumentTemplate) {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		this.selectedDocumentTemplate = selectedDocumentTemplate;
	}

	@Override
	public void onDraw() {
		HTMLFlow hint = new HTMLFlow(I18N.message("barcodepatternhint"));
		hint.setMargin(3);
		setMembers(hint);

		refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null, null);
	}

	/**
	 * Sends the patterns to the server to save them.
	 */
	private void onSave() {
		if (!selectedBarcodeTemplate.isZonal()) {
			Record[] records = positionalGrid.getRecords();
			GUIBarcodeSpec[] patterns = new GUIBarcodeSpec[records.length];
			int i = 0;
			for (Record record : records) {
				GUIBarcodeSpec patt = new GUIBarcodeSpec();
				patt.setPatterns(record.getAttributeAsString("pattern"));
				patt.setInclude(record.getAttributeAsString("include"));
				patt.setExclude(record.getAttributeAsString("exclude"));
				patt.setFormats(record.getAttributeAsString("formats"));
				patterns[i++] = patt;
				patt.setIndex(i);
			}
			selectedBarcodeTemplate.setBarcodeSpecs(patterns);
		}

		BarcodeService.Instance.get().save(selectedBarcodeTemplate, new AsyncCallback<GUIBarcodeTemplate>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeTemplate template) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void refresh(Long documentTemplateId, Long barcodeTemplateId) {
		if (toolStrip != null)
			removeMember(toolStrip);
		if (editorPanel != null)
			removeMember(editorPanel);

		templateSelector = ItemFactory.newTemplateSelector(true, documentTemplateId);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				selectedBarcodeTemplate = null;

				ListGridRecord record = templateSelector.getSelectedRecord();
				if (record == null || record.getAttributeAsLong("id") == null
						|| record.getAttributeAsLong("id").longValue() == 0L) {
					selectedDocumentTemplate = null;
					refresh(null, null);
				} else {
					selectedDocumentTemplate = new GUITemplate();
					selectedDocumentTemplate.setId(record.getAttributeAsLong("id"));
					selectedDocumentTemplate.setName(record.getAttributeAsString("name"));
					selectedDocumentTemplate.setDescription(record.getAttributeAsString("description"));
					refresh(selectedDocumentTemplate.getId(), null);
				}
			}
		});

		barcodeTemplateSelector = ItemFactory.newBarcodeTemplateSelector(false, documentTemplateId, barcodeTemplateId);
		barcodeTemplateSelector.setWrapTitle(false);
		barcodeTemplateSelector.setMultiple(false);
		barcodeTemplateSelector.setEndRow(false);
		barcodeTemplateSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord record = barcodeTemplateSelector.getSelectedRecord();
				BarcodeService.Instance.get().getTemplate(record.getAttributeAsLong("id"),
						new AsyncCallback<GUIBarcodeTemplate>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIBarcodeTemplate tmpl) {
								setSelectedBarcodeTemplate(tmpl);
							}
						});
			}
		});

		save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		append = new ToolStripButton();
		append.setAutoFit(true);
		append.setTitle(selectedBarcodeTemplate != null && selectedBarcodeTemplate.isZonal() ? I18N.message("addzone")
				: I18N.message("appendpattern"));
		append.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				event.cancel();

				if (selectedBarcodeTemplate != null && selectedBarcodeTemplate.isZonal()) {
					GUIBarcodeSpec zone = new GUIBarcodeSpec();
					zone.setTemplateId(selectedBarcodeTemplate.getId());
					zone.setPatterns("<customId>");
					selectedBarcodeTemplate.appendBarcodeSpec(zone);
					Canvas zoneCanvas = new ZoneCanvas(zone, BarcodeTemplatesPanel.this);
					zonalCanvas.addCanvas(zoneCanvas);
				} else {
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("pattern", "");
					positionalGrid.getRecordList().add(record);
				}
			}
		});

		settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				BarcodeTemplateSettings editor = new BarcodeTemplateSettings(BarcodeTemplatesPanel.this,
						selectedBarcodeTemplate);
				editor.show();
			}
		});

		newTemplate = new ToolStripButton();
		newTemplate.setTitle(I18N.message("new"));
		newTemplate.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GUIBarcodeTemplate newTemplate = new GUIBarcodeTemplate();
				newTemplate.setTemplate(selectedDocumentTemplate);
				BarcodeTemplateSettings editor = new BarcodeTemplateSettings(BarcodeTemplatesPanel.this, newTemplate);
				editor.show();
			}
		});

		delete = new ToolStripButton();
		delete.setAutoFit(true);
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdeletebarcodetemplate"), new BooleanCallback() {

					@Override
					public void execute(Boolean value) {
						if (value)
							BarcodeService.Instance.get().delete(selectedBarcodeTemplate.getId(),
									new AsyncCallback<Void>() {

										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void arg0) {
											selectedBarcodeTemplate = null;
											refresh(selectedDocumentTemplate.getId(), null);
										}
									});
					}
				});
			}
		});

		close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				selectedBarcodeTemplate = null;
				refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null, null);
			}
		});

		print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (selectedBarcodeTemplate.isZonal())
					PrintUtil.printScreenShot(zonalCanvas.getID(),
							I18N.message("zonalocr") + " - " + selectedBarcodeTemplate.getName());
				else
					GridUtil.print(positionalGrid);

			}
		});

		zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (zonalCanvas != null) {
					zonalCanvas.clearCanvases();
					zonalCanvas.resize(+100);
					showZones();
				}
			}
		});

		zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (zonalCanvas != null) {
					zonalCanvas.clearCanvases();
					zonalCanvas.resize(-100);
					showZones();
				}
			}
		});

		toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		toolStrip.addFormItem(templateSelector);
		toolStrip.addFormItem(barcodeTemplateSelector);
		toolStrip.addButton(newTemplate);
		toolStrip.addButton(settings);
		toolStrip.addButton(save);
		toolStrip.addButton(append);
		toolStrip.addButton(delete);
		toolStrip.addSeparator();
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addButton(print);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();
		
		editorPanel=new HLayout();
		editorPanel.setWidth100();
		editorPanel.setHeight100();
		editorPanel.setOverflow(Overflow.AUTO);
		setMembers(toolStrip, editorPanel);

		if (selectedBarcodeTemplate != null && selectedBarcodeTemplate.isZonal()) {
			String url = Util.contextPath() + "barcodetemplateimage/" + selectedBarcodeTemplate.getId() + "?random="
					+ new Date().getTime();
			zonalCanvas = new ImageWithCanvases(url, getWidth().intValue() - 50, null, new CallBack() {

				@Override
				public void onImagesLoaded(ImageElement[] imageElements) {
					showZones();
				}
			});
			editorPanel.addMember(zonalCanvas);
		} else {
			positionalGrid = new PositionalBarcodesGrid(selectedBarcodeTemplate);
			editorPanel.addMember(positionalGrid);
		}
		
		
		settings.setDisabled(selectedBarcodeTemplate == null);
		save.setDisabled(selectedBarcodeTemplate == null);
		append.setDisabled(selectedBarcodeTemplate == null);
		delete.setDisabled(selectedBarcodeTemplate == null || selectedBarcodeTemplate.getId() == 0L);
		close.setDisabled(selectedBarcodeTemplate == null);
		print.setDisabled(selectedBarcodeTemplate == null);
		zoomIn.setDisabled(selectedBarcodeTemplate == null || zonalCanvas == null);
		zoomOut.setDisabled(selectedBarcodeTemplate == null || zonalCanvas == null);
	}

	void showZones() {
		if (selectedBarcodeTemplate.getBarcodeSpecs() != null)
			for (GUIBarcodeSpec zone : selectedBarcodeTemplate.getBarcodeSpecs()) {
				zone.setTemplateId(selectedBarcodeTemplate.getId());
				Canvas zoneCanvas = new ZoneCanvas(zone, BarcodeTemplatesPanel.this);
				zonalCanvas.addCanvas(zoneCanvas);
			}
	}

	public GUIBarcodeTemplate getSelectedBarcodeTemplate() {
		return selectedBarcodeTemplate;
	}

	public void setSelectedBarcodeTemplate(GUIBarcodeTemplate selectedBarcodeTemplate) {
		this.selectedBarcodeTemplate = selectedBarcodeTemplate;
		refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null,
				selectedBarcodeTemplate.getId());
	}

	public ImageWithCanvases getZonalCanvas() {
		return zonalCanvas;
	}
}