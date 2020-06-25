package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeSpec;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the barcodes patterns configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class BarcodeTemplatesPanel extends VLayout {

	private ListGrid patternsGrid;

	private SelectItem templateSelector;

	private SelectItem barcodeTemplateSelector;

	private GUIBarcodeTemplate selectedBarcodeTemplate;

	private GUITemplate selectedDocumentTemplate;

	private ToolStripButton save;

	private ToolStripButton append;

	private ToolStripButton settings;

	private ToolStripButton newTemplate;

	private ToolStripButton delete;

	private ToolStripButton close;

	private ToolStrip toolStrip;

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

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("ddelete"));
		clean.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							patternsGrid.removeData(patternsGrid.getSelectedRecord());
						}
					}
				});
			}
		});

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}

	/**
	 * Sends the patterns in the grid to the server to save them.
	 */
	private void onSave() {
		Record[] records = patternsGrid.getRecords();
		GUIBarcodeSpec[] patterns = new GUIBarcodeSpec[records.length];
		int i = 0;
		for (Record record : records) {
			GUIBarcodeSpec patt = new GUIBarcodeSpec();
			patt.setPatterns(record.getAttributeAsString("pattern"));
			patt.setInclude(record.getAttributeAsString("include"));
			patt.setExclude(record.getAttributeAsString("exclude"));
			patt.setFormats(record.getAttributeAsString("formats"));
			patterns[i++] = patt;
		}
		selectedBarcodeTemplate.setBarcodeSpecs(patterns);

		BarcodeService.Instance.get().save(selectedBarcodeTemplate, new AsyncCallback<GUIBarcodeTemplate>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeTemplate template) {
				Log.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void refresh(Long documentTemplateId, Long barcodeTemplateId) {
		if (toolStrip != null)
			removeMember(toolStrip);
		if (patternsGrid != null) {
			removeMember(patternsGrid);
			patternsGrid.destroy();
		}

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
								Log.serverError(caught);
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
		append.setTitle(I18N.message("appendpattern"));
		append.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				event.cancel();
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("pattern", "");
				patternsGrid.getRecordList().add(record);
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
											Log.serverError(caught);
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
		toolStrip.addButton(close);
		toolStrip.addFill();

		ListGridField pattern = new ListGridField("pattern", I18N.message("patternscomma"));
		pattern.setWidth(300);
		pattern.setRequired(true);
		pattern.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return Util.strip(record.getAttributeAsString("pattern"));
			}
		});

		ListGridField include = new ListGridField("include", I18N.message("include"));
		include.setWidth(200);
		include.setRequired(false);
		include.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return Util.strip(record.getAttributeAsString("include"));
			}
		});

		ListGridField exclude = new ListGridField("exclude", I18N.message("exclude"));
		exclude.setWidth(200);
		exclude.setRequired(false);
		exclude.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return Util.strip(record.getAttributeAsString("exclude"));
			}
		});

		ListGridField formats = new ListGridField("formats", I18N.message("formats"));
		formats.setWidth(200);
		formats.setRequired(false);
		formats.setCanEdit(true);
		formats.setEditorProperties(ItemFactory.newBarcodeFormatsComboBoxItem("formats", "formats", (String) null));

		patternsGrid = new ListGrid();
		patternsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		patternsGrid.setShowAllRecords(true);
		patternsGrid.setCanEdit(true);
		patternsGrid.setWidth100();
		patternsGrid.setHeight100();
		patternsGrid.setFields(pattern, include, exclude, formats);
		patternsGrid.setSelectionType(SelectionStyle.SINGLE);
		patternsGrid.setModalEditing(true);
		patternsGrid.setShowRowNumbers(true);
		patternsGrid.setCanReorderRecords(true);
		patternsGrid.setCanDragRecordsOut(true);
		patternsGrid.setCanAcceptDroppedRecords(true);
		patternsGrid.setDragDataAction(DragDataAction.MOVE);
		patternsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		addMember(toolStrip);
		addMember(patternsGrid);

		settings.setDisabled(selectedBarcodeTemplate == null);
		save.setDisabled(selectedBarcodeTemplate == null);
		append.setDisabled(selectedBarcodeTemplate == null);
		delete.setDisabled(selectedBarcodeTemplate == null || selectedBarcodeTemplate.getId() == 0L);
		close.setDisabled(selectedBarcodeTemplate == null);

		updateGrid();
	}

	private void updateGrid() {
		if (selectedBarcodeTemplate != null) {
			ArrayList<ListGridRecord> records = new ArrayList<ListGridRecord>();
			if (selectedBarcodeTemplate.getBarcodeSpecs() != null)
				for (GUIBarcodeSpec pat : selectedBarcodeTemplate.getBarcodeSpecs()) {
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("pattern", pat.getPatterns());
					record.setAttribute("include", pat.getInclude());
					record.setAttribute("exclude", pat.getExclude());

					String frmts = pat.getFormats();
					if (frmts == null || frmts.trim().isEmpty())
						record.setAttribute("formats", (String) null);
					else if (!frmts.trim().contains(","))
						record.setAttribute("formats", frmts.trim());
					else
						record.setAttribute("formats", pat.getFormats().replaceAll(" ", "").split(","));
					records.add(record);
				}
			patternsGrid.setRecords(records.toArray(new ListGridRecord[0]));
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
}