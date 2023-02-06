package com.logicaldoc.gui.frontend.client.impex.folders;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.data.ImportFolderHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows import folder's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class ImportFolderHistoryPanel extends ImportFolderDetailsTab {

	private static final String DOC_ID = "docId";
	private static final String CLOSE_SPAN = "</span>";
	private CheckboxItem recordHistory;

	public ImportFolderHistoryPanel(GUIImportFolder importFolder, final ChangedHandler changedHandler) {
		super(importFolder, changedHandler);
	}

	@Override
	protected void onDraw() {
		setWidth100();
		setHeight100();
		refresh();
	}

	private void refresh() {
		ColoredListGridField id = new ColoredListGridField("id");
		id.setHidden(true);

		ListGridField event = new ListGridField("event", I18N.message("event"));
		event.setAutoFitWidth(true);
		event.setCanFilter(true);
		event.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			if (value.toString().contains("importfolder.imported"))
				return "<span class='event-ok'>" + I18N.message("iimport").toLowerCase() + CLOSE_SPAN;
			else if (value.toString().contains("importfolder.updated"))
				return "<span class='event-med'>" + I18N.message("update").toLowerCase() + CLOSE_SPAN;
			else if (value.toString().contains("importfolder.error"))
				return "<span class='event-error'>" + I18N.message("error").toLowerCase() + CLOSE_SPAN;
			else
				return value.toString();
		});

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ColoredListGridField comment = new ColoredListGridField("comment", I18N.message("comment"));
		comment.setHidden(true);

		ListGridField size = new FileSizeListGridField(I18N.getAttributeLabel("size"), 70);
		size.setCanFilter(false);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setAutoFitWidth(true);
		ColoredListGridField path = new ColoredListGridField("path", I18N.message("path"));
		ColoredListGridField source = new ColoredListGridField("source", I18N.message("source"));

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new ImportFolderHistoryDS(importFolder.getId(), null));
		list.setFields(date, event, fileName, size, path, source, comment);

		list.addCellDoubleClickHandler((CellDoubleClickEvent evnt) -> {
			Record rec = evnt.getRecord();
			DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString(DOC_ID)));
		});

		list.addCellContextClickHandler((CellContextClickEvent e) -> {
			Menu contextMenu = new Menu();

			MenuItem openInFolder = new MenuItem();
			openInFolder.setTitle(I18N.message("openinfolder"));
			openInFolder.addClickHandler((MenuItemClickEvent evnt) -> {
				Record rec = list.getSelectedRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString(DOC_ID)));
			});

			MenuItem preview = new MenuItem();
			preview.setTitle(I18N.message("preview"));
			preview.addClickHandler((MenuItemClickEvent evnt) -> {
				Record rec = list.getSelectedRecord();
				GUIDocument doc = new GUIDocument();
				doc.setId(rec.getAttributeAsLong(DOC_ID));
				doc.setFileName(rec.getAttributeAsString("filename"));

				GUIFolder folder = new GUIFolder(rec.getAttributeAsLong("folderId"));
				doc.setFolder(folder);

				PreviewPopup iv = new PreviewPopup(doc);
				iv.show();
			});
			preview.setEnabled(
					com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

			contextMenu.setItems(preview, openInFolder);
			contextMenu.showContextMenu();
			if (e != null)
				e.cancel();
		});

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		SpinnerItem maxItem = ItemFactory.newSpinnerItem("max", "display",
				Session.get().getConfigAsInt("gui.maxhistories"), 1, (Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		maxItem.addChangedHandler((ChangedEvent evnt) -> {
			list.refresh(new ImportFolderHistoryDS(importFolder.getId(), Integer.parseInt(maxItem.getValueAsString())));
		});

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler((ClickEvent evnt) -> list.refresh(
				new ImportFolderHistoryDS(importFolder.getId(), Integer.parseInt(maxItem.getValueAsString()))));

		buttons.addButton(refresh);
		buttons.addFormItem(maxItem);
		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler((ClickEvent evnt) -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler((ClickEvent evnt) -> GridUtil.print(list));

		buttons.addSeparator();

		recordHistory = ItemFactory.newCheckbox("recordHistory", "enablehistory");
		recordHistory.setValue(importFolder.getRecordHistory() == 1);
		recordHistory.addChangedHandler(changedHandler);
		buttons.addFormItem(recordHistory);

		Layout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);
		container.addMember(buttons);
		addMember(container);
	}

	boolean validate() {
		importFolder.setRecordHistory(Boolean.TRUE.equals(recordHistory.getValueAsBoolean()) ? 1 : 0);
		return true;
	}
}