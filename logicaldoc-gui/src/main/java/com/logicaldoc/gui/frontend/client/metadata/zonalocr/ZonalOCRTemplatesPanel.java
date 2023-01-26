package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import java.util.Date;
import java.util.LinkedHashMap;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
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
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
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

	private SelectItem templateSelector;

	private SelectItem ocrTemplateSelector;

	private ToolStripButton settings;

	private ToolStripButton add;

	private ToolStripButton delete;

	private ToolStripButton close;

	private ToolStripButton save;

	private ToolStripButton zoomIn;

	private ToolStripButton zoomOut;

	private ToolStripButton addZone;

	private ToolStripButton deleteZones;

	private ToolStripButton print;

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

	private void refresh(Long templateId, Long barcodeTemplateId) {
		if (toolStrip != null)
			removeMember(toolStrip);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		addTemplateSelector(templateId, toolStrip);

		addOcrTemplateSelector(templateId, barcodeTemplateId, toolStrip);

		addNewButton(toolStrip);

		addSettingsButton(toolStrip);

		addDeleteButton(toolStrip);

		addSaveButton(toolStrip);

		toolStrip.addSeparator();

		addAddZoneButton(toolStrip);

		addDeleteZonesButton(toolStrip);

		toolStrip.addSeparator();

		addZoomInButton(toolStrip);

		addZoomOutButton(toolStrip);

		addPrintButton(toolStrip);

		toolStrip.addSeparator();

		addCloseButton(toolStrip);

		toolStrip.addFill();

		editorPanel.setWidth100();
		editorPanel.setHeight100();
		editorPanel.setOverflow(Overflow.AUTO);
		setMembers(toolStrip, editorPanel);

		updateSample();
	}

	private void addCloseButton(ToolStrip toolStrip) {
		close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler((ClickEvent event) -> {
			setSelectedOcrTemplate(null);
		});
		close.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(close);
	}

	private void addPrintButton(ToolStrip toolStrip) {
		print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler((ClickEvent event) -> {
			PrintUtil.printScreenShot(sample.getID(), I18N.message("zonalocr") + " - " + selectedOcrTemplate.getName());
		});
		print.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(print);
	}

	private void addZoomOutButton(ToolStrip toolStrip) {
		zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler((ClickEvent event) -> {
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
		zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler((ClickEvent event) -> {
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
		deleteZones = new ToolStripButton();
		deleteZones.setTitle(I18N.message("deletezones"));
		deleteZones.addClickHandler((ClickEvent event) -> {
			LD.ask(I18N.message("deletezones"), I18N.message("deletezonesquestion"), (Boolean yes) -> {
				if (yes) {
					selectedOcrTemplate.setZones(new GUIZone[0]);
					setSelectedOcrTemplate(selectedOcrTemplate);
				}
			});
		});
		deleteZones.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(deleteZones);
	}

	private void addAddZoneButton(ToolStrip toolStrip) {
		addZone = new ToolStripButton();
		addZone.setTitle(I18N.message("addzone"));
		addZone.addClickHandler((ClickEvent event) -> {
			FormItem select = new SelectItem("zone", I18N.message("zone"));

			LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
			for (GUIAttribute att : selectedOcrTemplate.getTemplate().getAttributes()) {
				if (att.getParent() == null && selectedOcrTemplate.getZone(att.getName()) == null)
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
		save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			ZonalOCRService.Instance.get().save(selectedOcrTemplate, new AsyncCallback<GUIOCRTemplate>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIOCRTemplate template) {
					ZonalOCRTemplatesPanel.this.selectedOcrTemplate = template;
					setSelectedOcrTemplate(template);
				}
			});
		});
		save.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(save);
	}

	private void addDeleteButton(ToolStrip toolStrip) {
		delete = new ToolStripButton();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((ClickEvent event) -> {
			if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L)
				LD.ask(I18N.message("question"), I18N.message("confirmdeleteocrtemplate"), (Boolean yes) -> {
					if (yes) {
						ZonalOCRService.Instance.get().delete(selectedOcrTemplate.getId(), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
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
		settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler((ClickEvent event) -> {
			if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L) {
				ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
				editor.show();
			}
		});
		settings.setDisabled(selectedOcrTemplate == null);
		toolStrip.addButton(settings);
	}

	private void addNewButton(ToolStrip toolStrip) {
		add = new ToolStripButton();
		add.setTitle(I18N.message("new"));
		add.addClickHandler((ClickEvent event) -> {
			selectedOcrTemplate = new GUIOCRTemplate();
			selectedOcrTemplate.setTemplate(selectedDocumentTemplate);
			ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
			editor.show();
		});
		add.setDisabled(selectedDocumentTemplate == null);
		toolStrip.addButton(add);
	}

	private void addOcrTemplateSelector(Long templateId, Long barcodeTemplateId, ToolStrip toolStrip) {
		ocrTemplateSelector = ItemFactory.newOCRTemplateSelector(false, templateId, barcodeTemplateId);
		ocrTemplateSelector.setWrapTitle(false);
		ocrTemplateSelector.setMultiple(false);
		ocrTemplateSelector.setEndRow(false);
		ocrTemplateSelector.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord record = ocrTemplateSelector.getSelectedRecord();
			ZonalOCRService.Instance.get().getTemplate(record.getAttributeAsLong("id"),
					new AsyncCallback<GUIOCRTemplate>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIOCRTemplate tmpl) {
							setSelectedOcrTemplate(tmpl);
						}
					});
		});
		ocrTemplateSelector.setDisabled(selectedDocumentTemplate == null);
		toolStrip.addFormItem(ocrTemplateSelector);
	}

	private void addTemplateSelector(Long templateId, ToolStrip toolStrip) {
		templateSelector = ItemFactory.newTemplateSelector(true, templateId);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler((ChangedEvent event) -> {
			selectedOcrTemplate = null;

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
			sample = new ImageWithCanvases(url, getWidth().intValue() - 50, null, new CallBack() {

				@Override
				public void onImagesLoaded(ImageElement[] imageElements) {
					showZones();
				}
			});

			editorPanel.addMember(sample);
		}
	}

	@Override
	public void setSelectedOcrTemplate(GUIOCRTemplate selectedOcrTemplate) {
		super.setSelectedOcrTemplate(selectedOcrTemplate);
		AdminScreen.get().setContent(new ZonalOCRPanel(selectedDocumentTemplate, selectedOcrTemplate));
	}

	public GUIOCRTemplate getSelectedOcrTemplate() {
		return selectedOcrTemplate;
	}

	@Override
	protected ZoneCanvas newZoneCanvas(GUIZone zone) {
		return new ZonalOCRZoneCanvas(zone, this);
	}
}