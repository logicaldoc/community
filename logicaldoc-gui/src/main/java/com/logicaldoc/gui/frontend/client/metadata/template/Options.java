package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.data.AttributeOptionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the attribute options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class Options extends com.smartgwt.client.widgets.Window {

	private static final String CATEGORY = "category";

	private static final String VALUE = "value";

	private ListGrid list;

	private long setId;

	private String attribute;

	private boolean readOnly = false;

	public Options(final long setId, final String attribute, final boolean readOnly) {
		super();

		this.setId = setId;
		this.attribute = attribute;
		this.readOnly = readOnly;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("options"));
		setWidth(500);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setCanDragResize(true);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh());

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addoption"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> new AddAttributeOptionDialog(setId, attribute, list).show());
		add.setDisabled(readOnly);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		toolStrip.addButton(save);
		save.addClickHandler(event -> onSave());
		save.setDisabled(readOnly);

		ToolStripButton importCsv = new ToolStripButton();
		importCsv.setTitle(I18N.message("iimport"));
		importCsv.setTooltip(I18N.message("importfromcsv"));
		importCsv.addClickHandler(
				event -> DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						OptionsUploader uploader = new OptionsUploader(Options.this);
						uploader.show();
					}
				}));

		toolStrip.addSeparator();
		toolStrip.addButton(importCsv);

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(event -> GridUtil.exportCSV(list, false));
		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addButton(export);
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();
		addItem(toolStrip);

		prepareGrid();
		list.fetchData();

		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight("*");
		layout.setMembers(toolStrip, list);

		addItem(layout);
	}

	private void prepareGrid() {
		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth("*");
		value.setCanFilter(true);

		ListGridField category = new ListGridField(CATEGORY, I18N.message(CATEGORY));
		category.setCanFilter(true);
		category.setAutoFitWidth(true);
		category.setMinWidth(80);

		list = new ListGrid();
		list.setWidth100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(false);
		list.setDataSource(new AttributeOptionsDS(setId, attribute, null, false));
		list.setCanReorderRecords(!readOnly);
		list.setCanDragRecordsOut(!readOnly);
		list.setCanAcceptDroppedRecords(!readOnly);

		list.setShowRowNumbers(true);
		ListGridField numbers = new ListGridField(" ", 60);
		numbers.setAutoFitWidth(true);
		list.setRowNumberFieldProperties(numbers);

		list.setDragDataAction(DragDataAction.MOVE);
		list.setFields(id, value, category);
		list.sort("position", SortDirection.ASCENDING);

		if (!readOnly)
			list.addCellContextClickHandler(event -> {
				showContextMenu();
				event.cancel();
			});

		addResizedHandler(event -> list.setHeight(getHeight() - 68));
	}

	/**
	 * Sends the options in the grid to the server to save them.
	 */
	private void onSave() {
		Record[] records = list.getRecords();
		List<GUIValue> values = new ArrayList<>();
		for (Record rec : records)
			values.add(new GUIValue(rec.getAttributeAsString(CATEGORY), rec.getAttributeAsString(VALUE)));

		LD.contactingServer();
		AttributeSetService.Instance.get().saveOptions(setId, attribute, values, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				LD.clearPrompt();
				SC.say(I18N.message("optionssaved"));
			}
		});
	}

	/**
	 * Removs the selected options
	 */
	private void onDelete() {
		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		List<String> values = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			values.add(selection[i].getAttributeAsString(VALUE));

		AttributeSetService.Instance.get().deleteOptions(setId, attribute, values, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				list.removeSelectedData();
				list.deselectAllRecords();
			}
		});
	}

	public void refresh() {
		list.setDataSource(new AttributeOptionsDS(setId, attribute, null, false));
		list.fetchData();
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				onDelete();
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public String getAttribute() {
		return attribute;
	}

	public long getSetId() {
		return setId;
	}
}