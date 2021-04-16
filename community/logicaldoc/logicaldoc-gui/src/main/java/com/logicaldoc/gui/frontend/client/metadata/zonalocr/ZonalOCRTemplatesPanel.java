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
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the OCR templates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ZonalOCRTemplatesPanel extends VLayout {

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

	private ImageWithCanvases sample;

	private GUIOCRTemplate selectedOcrTemplate;

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

		templateSelector = ItemFactory.newTemplateSelector(true, templateId);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
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
			}
		});

		ocrTemplateSelector = ItemFactory.newOCRTemplateSelector(false, templateId, barcodeTemplateId);
		ocrTemplateSelector.setWrapTitle(false);
		ocrTemplateSelector.setMultiple(false);
		ocrTemplateSelector.setEndRow(false);
		ocrTemplateSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
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
			}
		});

		settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L) {
					ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
					editor.show();
				}
			}
		});

		add = new ToolStripButton();
		add.setTitle(I18N.message("new"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				selectedOcrTemplate = new GUIOCRTemplate();
				selectedOcrTemplate.setTemplate(selectedDocumentTemplate);
				ZonalOCRTemplateSettings editor = new ZonalOCRTemplateSettings(ZonalOCRTemplatesPanel.this);
				editor.show();
			}
		});

		delete = new ToolStripButton();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (selectedOcrTemplate != null && selectedOcrTemplate.getId() != 0L)
					LD.ask(I18N.message("question"), I18N.message("confirmdeleteocrtemplate"), new BooleanCallback() {
						@Override
						public void execute(Boolean value) {
							if (value) {
								ZonalOCRService.Instance.get().delete(selectedOcrTemplate.getId(),
										new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void result) {
												selectedOcrTemplate = null;
												AdminScreen.get()
														.setContent(new ZonalOCRPanel(selectedDocumentTemplate, null));
											}
										});
							}
						}
					});
			}
		});

		print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				PrintUtil.printScreenShot(sample.getID(),
						I18N.message("zonalocr") + " - " + selectedOcrTemplate.getName());
			}
		});

		close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				setSelectedOcrTemplate(null);
			}
		});

		save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
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
			}
		});

		zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (sample != null) {
					sample.clearCanvases();
					sample.resize(+100);
					showZones();
				}
			}
		});

		zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (sample != null) {
					sample.clearCanvases();
					sample.resize(-100);
					showZones();
				}
			}
		});

		addZone = new ToolStripButton();
		addZone.setTitle(I18N.message("addzone"));
		addZone.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				FormItem select = new SelectItem("zone", I18N.message("zone"));

				LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
				for (GUIAttribute att : selectedOcrTemplate.getTemplate().getAttributes()) {
					if (att.getParent() == null && selectedOcrTemplate.getZone(att.getName()) == null)
						map.put(att.getName(),
								att.getName() + " (" + AttributeTypeFormatter.format(att.getType()) + ")");
				}
				select.setValueMap(map);

				LD.askForValue(I18N.message("addzone"), "zone", "", select, 300, new ValueCallback() {

					@Override
					public void execute(String value) {
						GUIZone zone = new GUIZone(value,
								selectedOcrTemplate.getTemplate().getAttribute(value).getType());
						zone.setTemplateId(selectedOcrTemplate.getId());
						if (zone.getType() == GUIAttribute.TYPE_DATE)
							zone.setFormat(I18N.message("format_dateshort"));
						else if (zone.getType() == GUIAttribute.TYPE_DOUBLE)
							zone.setFormat("#,###.00");

						selectedOcrTemplate.appendZone(zone);
						Canvas zoneCanvas = new ZoneCanvas(zone, ZonalOCRTemplatesPanel.this);
						sample.addCanvas(zoneCanvas);
					}
				});
			}
		});

		deleteZones = new ToolStripButton();
		deleteZones.setTitle(I18N.message("deletezones"));
		deleteZones.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("deletezones"), I18N.message("deletezonesquestion"), new BooleanCallback() {

					@Override
					public void execute(Boolean value) {
						if (value) {
							selectedOcrTemplate.setZones(new GUIZone[0]);
							setSelectedOcrTemplate(selectedOcrTemplate);
						}
					}
				});
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		toolStrip.addFormItem(templateSelector);
		toolStrip.addFormItem(ocrTemplateSelector);
		toolStrip.addButton(add);
		toolStrip.addButton(settings);
		toolStrip.addButton(save);
		toolStrip.addButton(delete);
		toolStrip.addSeparator();
		toolStrip.addButton(addZone);
		toolStrip.addButton(deleteZones);
		toolStrip.addSeparator();
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addButton(print);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();

		editorPanel.setWidth100();
		editorPanel.setHeight100();
		editorPanel.setOverflow(Overflow.AUTO);
		setMembers(toolStrip, editorPanel);

		add.setDisabled(selectedDocumentTemplate == null);
		settings.setDisabled(selectedOcrTemplate == null);
		save.setDisabled(selectedOcrTemplate == null);
		zoomIn.setDisabled(selectedOcrTemplate == null);
		zoomOut.setDisabled(selectedOcrTemplate == null);
		addZone.setDisabled(selectedOcrTemplate == null);
		deleteZones.setDisabled(selectedOcrTemplate == null);
		delete.setDisabled(selectedOcrTemplate == null || selectedOcrTemplate.getId() == 0L);
		close.setDisabled(selectedOcrTemplate == null);
		print.setDisabled(selectedOcrTemplate == null);
		ocrTemplateSelector.setDisabled(selectedDocumentTemplate == null);

		updateSample();
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

	void showZones() {
		if (selectedOcrTemplate.getZones() != null)
			for (GUIZone zone : selectedOcrTemplate.getZones()) {
				zone.setTemplateId(selectedOcrTemplate.getId());
				Canvas zoneCanvas = new ZoneCanvas(zone, ZonalOCRTemplatesPanel.this);
				sample.addCanvas(zoneCanvas);
			}
	}

	public ImageWithCanvases getSample() {
		return sample;
	}

	public void setSelectedOcrTemplate(GUIOCRTemplate selectedOcrTemplate) {
		this.selectedOcrTemplate = selectedOcrTemplate;
		AdminScreen.get().setContent(new ZonalOCRPanel(selectedDocumentTemplate, selectedOcrTemplate));
	}

	public GUIOCRTemplate getSelectedOcrTemplate() {
		return selectedOcrTemplate;
	}
}