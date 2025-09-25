package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.ArrayList;
import java.util.Date;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;
import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.PrintUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneCanvas;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneTemplatePanel;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the barcodes patterns configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class BarcodeTemplatesPanel extends ZoneTemplatePanel {

	private PositionalBarcodesGrid positionalGrid;

	private ToolStripButton save;

	private ToolStripButton append;

	private ToolStripButton settings;

	private ToolStripButton delete;

	private ToolStripButton print;

	private ToolStripButton close;

	private ToolStripButton zoomIn;

	private ToolStripButton zoomOut;

	private ToolStrip toolStrip;

	private HLayout editorPanel;

	private HTMLFlow hint;

	private GUITemplate selectedDocumentTemplate;

	public BarcodeTemplatesPanel(GUITemplate selectedDocumentTemplate) {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		this.selectedDocumentTemplate = selectedDocumentTemplate;
	}

	@Override
	public void onDraw() {
		refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null, null);
	}

	/**
	 * Sends the patterns to the server to save them.
	 */
	private void onSave() {
		if (!((GUIBarcodeTemplate) selectedOcrTemplate).isZonal()) {
			Record[] records = positionalGrid.getRecords();
			ArrayList<GUIZone> patterns = new ArrayList<>();
			int i = 0;
			for (Record rec : records) {
				GUIBarcodeZone patt = new GUIBarcodeZone();
				patt.setPatterns(rec.getAttributeAsString("pattern"));
				patt.setInclude(rec.getAttributeAsString("include"));
				patt.setExclude(rec.getAttributeAsString("exclude"));
				patt.setFormats(rec.getAttributeAsString("formats"));
				patt.setIndex(i++);
				patterns.add(patt);
			}
			selectedOcrTemplate.setZones(patterns);
		}

		BarcodeService.Instance.get().save((GUIBarcodeTemplate) selectedOcrTemplate, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIBarcodeTemplate template) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void refresh(Long documentTemplateId, Long barcodeTemplateId) {
		if (hint != null)
			removeMember(hint);
		if (toolStrip != null)
			removeMember(toolStrip);
		if (editorPanel != null)
			removeMember(editorPanel);

		hint = new HTMLFlow(I18N.message("barcodepatternhint"));
		hint.setMargin(3);

		toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		addTemplateSelector(documentTemplateId);

		addBarcodeTemplateSelector(documentTemplateId, barcodeTemplateId);

		addNewTemplateButton();

		addSettingsButton();

		addSaveButton();

		addAppendButton();

		addDeleteButton();

		toolStrip.addSeparator();

		addZoomInButton();

		addZoomOutButton();

		addPrintButton();

		toolStrip.addSeparator();

		addCloseButton();

		toolStrip.addFill();

		editorPanel = new HLayout();
		editorPanel.setWidth100();
		editorPanel.setHeight100();
		editorPanel.setOverflow(Overflow.AUTO);
		setMembers(hint, toolStrip, editorPanel);

		settings.setDisabled(selectedOcrTemplate == null);
		save.setDisabled(selectedOcrTemplate == null);
		append.setDisabled(selectedOcrTemplate == null);
		delete.setDisabled(selectedOcrTemplate == null || selectedOcrTemplate.getId() == 0L);
		close.setDisabled(selectedOcrTemplate == null);
		print.setDisabled(selectedOcrTemplate == null);
		zoomIn.setDisabled(
				selectedOcrTemplate == null || sample == null || !((GUIBarcodeTemplate) selectedOcrTemplate).isZonal());
		zoomOut.setDisabled(
				selectedOcrTemplate == null || sample == null || !((GUIBarcodeTemplate) selectedOcrTemplate).isZonal());

		if (selectedOcrTemplate != null && !((GUIBarcodeTemplate) selectedOcrTemplate).isZonal()) {
			positionalGrid = new PositionalBarcodesGrid((GUIBarcodeTemplate) selectedOcrTemplate);
			editorPanel.addMember(positionalGrid);
		}

		updateSample();
	}

	private void addCloseButton() {
		close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler((ClickEvent closeClick) -> {
			selectedOcrTemplate = null;
			refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null, null);
		});
		toolStrip.addButton(close);
	}

	private void addPrintButton() {
		print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler((ClickEvent printClick) -> {
			if (((GUIBarcodeTemplate) selectedOcrTemplate).isZonal())
				PrintUtil.printScreenShot(sample.getID(),
						I18N.message("zonalocr") + " - " + selectedOcrTemplate.getName());
			else
				GridUtil.print(positionalGrid);
		});
		toolStrip.addButton(print);
	}

	private void addZoomOutButton() {
		zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler((ClickEvent zoomOutClick) -> {
			if (sample != null) {
				sample.clearCanvases();
				sample.resize(-100);
				showZones();
			}
		});
		toolStrip.addButton(zoomOut);
	}

	private void addZoomInButton() {
		zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler((ClickEvent zoomInClick) -> {
			if (sample != null) {
				sample.clearCanvases();
				sample.resize(+100);
				showZones();
			}
		});
		toolStrip.addButton(zoomIn);
	}

	private void addDeleteButton() {
		delete = new ToolStripButton();
		delete.setAutoFit(true);
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(deleteClick -> LD.ask(I18N.message("question"),
				I18N.message("confirmdeletebarcodetemplate"), (Boolean yes) -> {
					if (Boolean.TRUE.equals(yes))
						BarcodeService.Instance.get().delete(selectedOcrTemplate.getId(), new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								selectedOcrTemplate = null;
								refresh(selectedDocumentTemplate.getId(), null);
							}
						});
				}));
		toolStrip.addButton(delete);
	}

	private void addAppendButton() {
		append = new ToolStripButton();
		append.setAutoFit(true);
		append.setTitle(selectedOcrTemplate != null && ((GUIBarcodeTemplate) selectedOcrTemplate).isZonal()
				? I18N.message("addzone")
				: I18N.message("appendpattern"));
		append.addClickHandler((ClickEvent appendClick) -> {
			appendClick.cancel();

			if (selectedOcrTemplate != null && ((GUIBarcodeTemplate) selectedOcrTemplate).isZonal()) {
				GUIBarcodeZone zone = new GUIBarcodeZone();
				zone.setTemplateId(selectedOcrTemplate.getId());
				zone.setPatterns("<customId>");
				((GUIBarcodeTemplate) selectedOcrTemplate).appendBarcodeZone(zone);
				Canvas zoneCanvas = new BarcodeZoneCanvas(zone, BarcodeTemplatesPanel.this);
				sample.addCanvas(zoneCanvas);
			} else {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("pattern", "");
				positionalGrid.getRecordList().add(rec);
			}
		});
		toolStrip.addButton(append);
	}

	private void addSaveButton() {
		save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(saveClick -> onSave());
		toolStrip.addButton(save);
	}

	private void addSettingsButton() {
		settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler(settingsClick -> new BarcodeTemplateSettings(BarcodeTemplatesPanel.this,
				((GUIBarcodeTemplate) selectedOcrTemplate)).show());
		toolStrip.addButton(settings);
	}

	private void addNewTemplateButton() {
		ToolStripButton newTemplate = new ToolStripButton();
		newTemplate.setTitle(I18N.message("new"));
		newTemplate.addClickHandler(click -> {
			GUIBarcodeTemplate newBarcodeTemplate = new GUIBarcodeTemplate();
			newBarcodeTemplate.setTemplate(selectedDocumentTemplate);
			new BarcodeTemplateSettings(BarcodeTemplatesPanel.this, newBarcodeTemplate).show();
		});
		toolStrip.addButton(newTemplate);
	}

	private void addBarcodeTemplateSelector(Long documentTemplateId, Long barcodeTemplateId) {
		SelectItem barcodeTemplateSelector = ItemFactory.newBarcodeTemplateSelector(false, documentTemplateId,
				barcodeTemplateId);
		barcodeTemplateSelector.setWrapTitle(false);
		barcodeTemplateSelector.setMultiple(false);
		barcodeTemplateSelector.setEndRow(false);
		barcodeTemplateSelector.addChangedHandler(changed -> {
			ListGridRecord rec = barcodeTemplateSelector.getSelectedRecord();
			BarcodeService.Instance.get().getTemplate(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIBarcodeTemplate tmpl) {
					setSelectedOcrTemplate(tmpl);
				}
			});
		});
		toolStrip.addFormItem(barcodeTemplateSelector);
	}

	private void addTemplateSelector(Long documentTemplateId) {
		SelectItem templateSelector = ItemFactory.newTemplateSelector(true, documentTemplateId);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler((ChangedEvent templateSelectorChanged) -> {
			selectedOcrTemplate = null;

			ListGridRecord rec = templateSelector.getSelectedRecord();
			if (rec == null || rec.getAttributeAsLong("id") == null || rec.getAttributeAsLong("id").longValue() == 0L) {
				selectedDocumentTemplate = null;
				refresh(null, null);
			} else {
				selectedDocumentTemplate = new GUITemplate();
				selectedDocumentTemplate.setId(rec.getAttributeAsLong("id"));
				selectedDocumentTemplate.setName(rec.getAttributeAsString("name"));
				selectedDocumentTemplate.setDescription(rec.getAttributeAsString("description"));
				refresh(selectedDocumentTemplate.getId(), null);
			}
		});
		toolStrip.addFormItem(templateSelector);
	}

	/**
	 * Redisplays the sample image where the zones can be visually defined
	 */
	private void updateSample() {
		if (selectedOcrTemplate != null && selectedOcrTemplate.getSample() != null) {
			String url = Util.contextPath() + "barcodetemplateimage/" + selectedOcrTemplate.getId() + "?random="
					+ new Date().getTime();
			sample = new ImageWithCanvases(url, getWidth().intValue() - 50, null, imageElements -> showZones());

			editorPanel.addMember(sample);
		}
	}

	@Override
	public void setSelectedOcrTemplate(GUIOCRTemplate selectedOcrTemplate) {
		super.setSelectedOcrTemplate(selectedOcrTemplate);
		refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null,
				selectedOcrTemplate.getId());
	}

	@Override
	protected ZoneCanvas newZoneCanvas(GUIZone zone) {
		return new BarcodeZoneCanvas(zone, this);
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}