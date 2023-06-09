package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.OCRHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows import folder's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class OCRHistoryPanel extends VLayout {

	private static final String COMMENT = "comment";

	private static final String DOC_ID = "docId";

	private static final String CLOSE_SPAN = "</span>";

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
		event.setAlign(Alignment.CENTER);
		event.setAutoFitWidth(true);
		event.setCanFilter(true);
		event.setCellFormatter((value, record, rowNum, colNum) -> {
			if (value.toString().contains("ocr.success"))
				return "<span class='event-ok'>" + I18N.message("success").toLowerCase() + CLOSE_SPAN;
			else if (value.toString().contains("ocr.failure"))
				return "<span class='event-error'>" + I18N.message("failure").toLowerCase() + CLOSE_SPAN;
			else
				return value.toString();
		});

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ColoredListGridField comment = new ColoredListGridField(COMMENT, I18N.message(COMMENT));
		comment.setWidth("*");

		ListGridField size = new FileSizeListGridField(I18N.getAttributeLabel("size"), 70);
		size.setCanFilter(false);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setAutoFitWidth(true);
		
		ColoredListGridField path = new ColoredListGridField("path", I18N.message("path"));
		path.setCanFilter(true);
		path.setWidth(300);
		
		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new OCRHistoryDS(null));
		list.setFields(date, event, fileName, size, path, comment);

		list.addCellDoubleClickHandler(evnt -> {
			Record record = evnt.getRecord();
			ListGridField field = list.getField(evnt.getColNum());
			String title = field.getTitle();
			if (evnt.getColNum() == 4 || evnt.getColNum() == 5) {
				LD.askForValue(title, title, record.getAttribute(field.getName()), 350, val -> {
					// Nothing to do
				});
			} else {
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString(DOC_ID)));
			}
		});

		list.addCellContextClickHandler(e -> {
			Menu contextMenu = new Menu();

			MenuItem openInFolder = new MenuItem();
			openInFolder.setTitle(I18N.message("openinfolder"));
			openInFolder.addClickHandler(evnt -> {
				Record rec = list.getSelectedRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString(DOC_ID)));
			});

			MenuItem preview = new MenuItem();
			preview.setTitle(I18N.message("preview"));
			preview.addClickHandler(evnt -> {
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

			MenuItem downloadIndexed = new MenuItem();
			downloadIndexed.setTitle(I18N.message("downloadindexedtext"));
			downloadIndexed.addClickHandler(evnt -> {
				Record rec = list.getSelectedRecord();
				FolderService.Instance.get().getFolder(rec.getAttributeAsLong("folderId"), false, false, false,
						new AsyncCallback<GUIFolder>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIFolder folder) {
								if (folder.isDownload())
									Util.download(
											Util.downloadURL(rec.getAttributeAsLong(DOC_ID)) + "&downloadText=true");
							}
						});

			});

			contextMenu.setItems(preview, downloadIndexed, openInFolder);
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
		maxItem.addChangedHandler(evnt -> list.refresh(new OCRHistoryDS(Integer.parseInt(maxItem.getValueAsString()))));

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(evnt -> list.refresh(new OCRHistoryDS(Integer.parseInt(maxItem.getValueAsString()))));

		buttons.addButton(refresh);
		buttons.addFormItem(maxItem);
		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(evnt -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(evnt -> GridUtil.print(list));

		buttons.addSeparator();

		Layout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(buttons);
		container.addMember(list);
		addMember(container);
	}
}