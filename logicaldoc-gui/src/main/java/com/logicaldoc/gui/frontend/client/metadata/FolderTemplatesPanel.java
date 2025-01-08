package com.logicaldoc.gui.frontend.client.metadata;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the folder templates configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class FolderTemplatesPanel extends AdminPanel {

	private static final String FOLDERS = "folders";

	private ListGrid grid;

	public FolderTemplatesPanel() {
		super("foldertemplates");

		HTMLFlow hint = new HTMLFlow(I18N.message("foldertemplatehint"));
		hint.setMargin(3);

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> saveTemplates());

		ToolStripButton append = new ToolStripButton();
		append.setAutoFit(true);
		append.setTitle(I18N.message("appendtemplate"));
		append.addClickHandler(event -> {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("pattern", "");
			grid.getRecordList().add(rec);
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(append);
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		body.setMembers(hint, toolStrip);

		FolderService.Instance.get().loadTemplates(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIValue> p) {
				reloadTemplates(p);
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("ddelete"));
		clean.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm))
				grid.removeData(grid.getSelectedRecord());
		}));

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}

	/**
	 * Sends the patterns in the grid to the server to save them.
	 */
	private void saveTemplates() {
		Record[] records = grid.getRecords();

		List<GUIValue> templates = new ArrayList<>();
		if (records != null)
			for (Record rec : records) {
				GUIValue val = new GUIValue();
				val.setCode(rec.getAttributeAsString("name"));
				val.setValue(rec.getAttributeAsString(FOLDERS));
				templates.add(val);
			}

		FolderService.Instance.get().saveTemplates(templates, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	protected void reloadTemplates(List<GUIValue> templates) {
		if (grid != null)
			body.removeMember(grid);

		ListGridField folders = new ListGridField(FOLDERS, I18N.message(FOLDERS));
		folders.setWidth(400);
		folders.setRequired(true);
		folders.setEditorProperties(new TextAreaItem());

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

		grid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		body.removeMember(grid);
		List<ListGridRecord> records = new ArrayList<>();
		for (GUIValue template : templates) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("name", template.getCode());
			rec.setAttribute(FOLDERS, template.getValue());
			records.add(rec);
		}
		grid.setData(records.toArray(new ListGridRecord[0]));

		body.addMember(grid);
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