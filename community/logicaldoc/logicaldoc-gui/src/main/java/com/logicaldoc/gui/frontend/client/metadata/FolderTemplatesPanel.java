package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the folder templates configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class FolderTemplatesPanel extends AdminPanel {

	private ListGrid grid;

	public FolderTemplatesPanel() {
		super("foldertemplates");

		HTMLFlow hint = new HTMLFlow(I18N.message("foldertemplatehint"));
		hint.setMargin(3);

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				saveTemplates();
			}
		});

		ToolStripButton append = new ToolStripButton();
		append.setAutoFit(true);
		append.setTitle(I18N.message("appendtemplate"));
		append.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("pattern", "");
				grid.getRecordList().add(rec);
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(append);
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		body.setMembers(hint, toolStrip);

		FolderService.Instance.get().loadTemplates(new AsyncCallback<GUIValue[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIValue[] p) {
				reloadTemplates(p);
			}
		});
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
							grid.removeData(grid.getSelectedRecord());
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
	private void saveTemplates() {
		Record[] records = grid.getRecords();

		GUIValue[] templates = new GUIValue[records != null ? records.length : 0];
		int i = 0;
		if (records != null)
			for (Record record : records) {
				templates[i] = new GUIValue();
				templates[i].setCode(record.getAttributeAsString("name"));
				templates[i++].setValue(record.getAttributeAsString("folders"));
			}

		FolderService.Instance.get().saveTemplates(templates, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	protected void reloadTemplates(GUIValue[] templates) {
		if (grid != null)
			body.removeMember(grid);

		ListGridField folders = new ListGridField("folders", I18N.message("folders"));
		folders.setWidth(400);
		folders.setRequired(true);
		folders.setEditorType(new TextAreaItem());

		ListGridField name = new ListGridField("name", I18N.message("foldertemplates"));
		name.setWidth(150);
		name.setRequired(true);

		grid = new ListGrid();
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowAllRecords(true);
		grid.setCanEdit(true);
		grid.setWidth100();
		grid.setHeight100();
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setModalEditing(true);
		grid.setFields(name, folders);

		grid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		body.removeMember(grid);
		ListGridRecord[] records = new ListGridRecord[templates.length];
		int i = 0;
		for (GUIValue template : templates) {
			ListGridRecord record = new ListGridRecord();
			record.setAttribute("name", template.getCode());
			record.setAttribute("folders", template.getValue());
			records[i++] = record;
		}
		grid.setData(records);

		body.addMember(grid);
	}
}