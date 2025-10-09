package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.PrintUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.metadata.template.AttributeTypeFormatter;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneCanvas;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneTemplatePanel;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the OCR templates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ZonalOCRTemplatesPanel extends ZoneTemplatePanel {

	private ToolStrip toolStrip;

	private HLayout editorPanel = new HLayout();

	private GUITemplate selectedDocumentTemplate;

	public ZonalOCRTemplatesPanel(GUITemplate selectedDocumentTemplate, GUIOCRTemplate selectedOcrTemplate) {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		this.selectedDocumentTemplate = selectedDocumentTemplate;
		this.selectedOcrTemplate = selectedOcrTemplate;
	}

	@Override
	public void onDraw() {
		refresh(selectedDocumentTemplate != null ? selectedDocumentTemplate.getId() : null,
				selectedOcrTemplate != null ? selectedOcrTemplate.getId() : null);
	}

	private void refresh(Long templateId, Long ocrTemplateId) {
		if (toolStrip != null)
			removeMember(toolStrip);

		ToolStrip toolBar = new ToolStrip();
		toolBar.setWidth100();
		toolBar.addSpacer(2);

		addTemplateSelector(templateId, toolBar);

		addOcrTemplateSelector(templateId, ocrTemplateId, toolBar);

		addNewButton(toolBar);

		addSettingsButton(toolBar);

		addSaveButton(toolBar);

		addAddZoneButton(toolBar);

		addDeleteZonesButton(toolBar);

		addDeleteButton(toolBar);

		toolBar.addSeparator();

		addZoomInButton(toolBar);

		addZoomOutButton(toolBar);

		addPrintButton(toolBar);

		toolBar.addSeparator();

		addCloseButton(toolBar);

		toolBar.addFill();

		editorPanel.setWidth100();
		editorPanel.setHeight100();
		editorPanel.setOverflow(Overflow.AUTO);
		setMembers(toolBar, editorPanel);

		updateSample();
	}

	private void addCloseButton(ToolStrip toolStrip) {
		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(event -> setSelectedOcrTemplate(null));
		close.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(close);
	}

	private void addPrintButton(ToolStrip toolStrip) {
		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(event -> PrintUtil.printScreenShot(sample.getID(),
				I18N.message("zonalocr") + " - " + selectedOcrTemplate.getName()));
		print.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(print);
	}

	private void addZoomOutButton(ToolStrip toolStrip) {
		ToolStripButton zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(event -> {
			if (sample != null) {
				sample.clearCanvases();
				sample.resize(-100);
				showZones();
			}
		});
		zoomOut.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(zoomOut);
	}

	private void addZoomInButton(ToolStrip toolStrip) {
		ToolStripButton zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(event -> {
			if (sample != null) {
				sample.clearCanvases();
				sample.resize(+100);
				showZones();
			}
		});
		zoomIn.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(zoomIn);
	}

	private void addDeleteZonesButton(ToolStrip toolStrip) {
		ToolStripButton deleteZones = new ToolStripButton();
		deleteZones.setTitle(I18N.message("deletezones"));
		deleteZones.addClickHandler(
				event -> LD.ask(I18N.message("deletezones"), I18N.message("deletezonesquestion"), (Boolean answer) -> {
					if (Boolean.TRUE.equals(answer)) {
						selectedOcrTemplate.setZones(new ArrayList<>());
						setSelectedOcrTemplate(selectedOcrTemplate);
					}
				}));
		deleteZones.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(deleteZones);
	}

	private void addAddZoneButton(ToolStrip toolStrip) {
		ToolStripButton addZone = new ToolStripButton();
		addZone.setTitle(I18N.message("addzone"));
		addZone.addClickHandler((ClickEvent event) -> {
			FormItem select = new SelectItem("zone", I18N.message("zone"));

			LinkedHashMap<String, String> map = new LinkedHashMap<>();
			for (GUIAttribute att : selectedOcrTemplate.getTemplate().getAttributes()) {
				if (att.getParent() == null && selectedOcrTemplate.getZone(att.getName()) == null && !att.isSection())
					map.put(att.getName(), att.getName() + " (" + AttributeTypeFormatter.format(att.getType()) + ")");
			}
			select.setValueMap(map);

			LD.askForValue(I18N.message("addzone"), "zone", "", select, 300, (String value) -> {
				GUIZone zone = new GUIZone(value, selectedOcrTemplate.getTemplate().getAttribute(value).getType());
				zone.setTemplateId(selectedOcrTemplate.getId());
				if (zone.getType() == GUIAttribute.TYPE_DATE)
					zone.setFormat(I18N.message("format_dateshort"));
				else if (zone.getType() == GUIAttribute.TYPE_DOUBLE)
					zone.setFormat("#,###.00");

				selectedOcrTemplate.appendZone(zone);
				Canvas zoneCanvas = new ZonalOCRZoneCanvas(zone, ZonalOCRTemplatesPanel.this);
				sample.addCanvas(zoneCanvas);
			});
		});
		addZone.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(addZone);
	}

	private void addSaveButton(ToolStrip toolStrip) {
		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(
				event -> ZonalOCRService.Instance.get().save(selectedOcrTemplate, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIOCRTemplate template) {
						ZonalOCRTemplatesPanel.this.selectedOcrTemplate = template;
						setSelectedOcrTemplate(template);
					}
				}));
		save.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(save);
	}

	private void addDeleteButton(ToolStrip toolStrip) {
		ToolStripButton delete = new ToolStripButton();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> {
			if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L)
				LD.ask(I18N.message("question"), I18N.message("confirmdeleteocrtemplate"), (Boolean yes) -> {
					if (Boolean.TRUE.equals(yes)) {
						ZonalOCRService.Instance.get().delete(selectedOcrTemplate.getId(), new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(Void result) {
								selectedOcrTemplate = null;
								AdminScreen.get().setContent(new ZonalOCRPanel(selectedDocumentTemplate, null));
							}
						});
					}
				});
		});
		delete.setDisabled(selectedOcrTemplate == null || selectedOcrTemplate.getId() == 0L);
		toolStrip.addButton(delete);
	}

	private void addSettingsButton(ToolStrip toolStrip) {
		ToolStripButton settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler(event -> {
			if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L) {
				ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
				editor.show();
			}
		});
		settings.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(settings);
	}

	private void addNewButton(ToolStrip toolStrip) {
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("new"));
		add.addClickHandler(event -> {
			selectedOcrTemplate = new GUIOCRTemplate();
			selectedOcrTemplate.setTemplate(selectedDocumentTemplate);
			ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
			editor.show();
		});
		add.setDisabled(selectedDocumentTemplate == null);
		toolStrip.addButton(add);
	}

	private void addOcrTemplateSelector(Long templateId, Long barcodeTemplateId, ToolStrip toolStrip) {
		SelectItem ocrTemplateSelector = ItemFactory.newOCRTemplateSelector(false, templateId, barcodeTemplateId);
		ocrTemplateSelector.setWrapTitle(false);
		ocrTemplateSelector.setMultiple(false);
		ocrTemplateSelector.setEndRow(false);
		ocrTemplateSelector.addChangedHandler(event -> {
			ListGridRecord rec = ocrTemplateSelector.getSelectedRecord();
			ZonalOCRService.Instance.get().getTemplate(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIOCRTemplate tmpl) {
					setSelectedOcrTemplate(tmpl);
				}
			});
		});
		ocrTemplateSelector.setDisabled(selectedDocumentTemplate == null);
		toolStrip.addFormItem(ocrTemplateSelector);
	}

	private void addTemplateSelector(Long templateId, ToolStrip toolStrip) {
		SelectItem templateSelector = ItemFactory.newTemplateSelector(true, templateId);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler(changed -> {
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
			String url = Util.contextPath() + "ocrtemplateimage/" + selectedOcrTemplate.getId() + "?random="
					+ new Date().getTime();
			sample = new ImageWithCanvases(url, getWidth().intValue() - 50, null, imageElements -> showZones());
			editorPanel.addMember(sample);
		}
	}

	@Override
	public void setSelectedOcrTemplate(GUIOCRTemplate selectedOcrTemplate) {
		super.setSelectedOcrTemplate(selectedOcrTemplate);
		AdminScreen.get().setContent(new ZonalOCRPanel(selectedDocumentTemplate, selectedOcrTemplate));
	}

	@Override
	public GUIOCRTemplate getSelectedOcrTemplate() {
		return selectedOcrTemplate;
	}

	@Override
	protected ZoneCanvas newZoneCanvas(GUIZone zone) {
		return new ZonalOCRZoneCanvas(zone, this);
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