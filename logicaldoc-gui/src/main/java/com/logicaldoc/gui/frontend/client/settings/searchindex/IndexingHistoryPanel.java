package com.logicaldoc.gui.frontend.client.settings.searchindex;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IndexedListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows indexing history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.5
 */
public class IndexingHistoryPanel extends VLayout {

	private static final String EVENTS = "event.indexed,event.indexed.error";

	private static final String COMMENT = "comment";

	private static final String DOC_ID = "docId";

	private static final String CLOSE_SPAN = "</span>";

	private SpinnerItem maxItem;

	@Override
	protected void onDraw() {
		setWidth100();
		setHeight100();
		initGUI();
	}

	private void initGUI() {
		ColoredListGridField id = new ColoredListGridField("id");
		id.setHidden(true);

		ListGridField eventLabel = new ListGridField("event", I18N.message("result"));
		eventLabel.setAlign(Alignment.CENTER);
		eventLabel.setAutoFitWidth(true);
		eventLabel.setCanFilter(true);
		eventLabel.setCellFormatter((val, recrd, rowNum, colNum) -> {
			if (val.toString().equals(I18N.message("event.indexed")))
				return "<span class='event-ok'>" + I18N.message("success").toLowerCase() + CLOSE_SPAN;
			else if (val.toString().equals(I18N.message("event.indexed.error")))
				return "<span class='event-error'>" + I18N.message("failure").toLowerCase() + CLOSE_SPAN;
			else
				return val.toString();
		});

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ColoredListGridField comment = new ColoredListGridField(COMMENT, I18N.message("extract"));
		comment.setWidth("*");

		ListGridField size = new FileSizeListGridField("fileSize", I18N.getAttributeLabel("size"), 70);
		size.setCanFilter(false);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setAutoFitWidth(true);

		ColoredListGridField path = new ColoredListGridField("path", I18N.message("path"));
		path.setCanFilter(true);
		path.setWidth(300);

		ListGridField indexable = new IndexedListGridField("reason");

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(Session.get().getTenantId(), EVENTS,
				Session.get().getConfigAsInt("gui.maxhistories")));
		list.setFields(date, eventLabel, fileName, size, path, indexable, comment);

		list.addCellDoubleClickHandler(evnt -> {
			Record rec = evnt.getRecord();
			ListGridField field = list.getField(evnt.getColNum());
			String title = field.getTitle();
			if (evnt.getColNum() == 4 || evnt.getColNum() == 5) {
				LD.askForValue(title, title, rec.getAttribute(field.getName()), 350, val -> {
					// Nothing to do
				});
			} else {
				DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString(DOC_ID)));
			}
		});

		list.addCellContextClickHandler(e -> {
			showContexMenu(list);
			if (e != null)
				e.cancel();
		});

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		maxItem = ItemFactory.newSpinnerItem("max", "display", Session.get().getConfigAsInt("gui.maxhistories"), 1,
				(Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		maxItem.addChangedHandler(evnt -> refresh(list));

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(evnt -> refresh(list));

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

	private void refresh(RefreshableListGrid list) {
		list.refresh(new DocumentHistoryDS(Session.get().getTenantId(), EVENTS,
				Integer.parseInt(maxItem.getValueAsString())));
	}

	private void showContexMenu(RefreshableListGrid list) {
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
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIFolder folder) {
							if (folder.isDownload())
								Util.download(Util.downloadURL(rec.getAttributeAsLong(DOC_ID)) + "&downloadText=true");
						}
					});

		});

		MenuItem index = new MenuItem();
		index.setTitle(I18N.message("index"));
		index.addClickHandler(event -> {
			LD.contactingServer();
			DocumentService.Instance.get().indexDocuments(getSelectedDocIds(list), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void result) {
					LD.clearPrompt();
					refresh(list);
				}
			});
		});

		MenuItem markIndexMetadataOnly = new MenuItem();
		markIndexMetadataOnly.setTitle(I18N.message("markindexablemetadataonly"));
		markIndexMetadataOnly.addClickHandler(event -> DocumentService.Instance.get()
				.markIndexable(getSelectedDocIds(list), Constants.INDEX_TO_INDEX_METADATA, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						refresh(list);
					}
				}));

		MenuItem markUnindexable = new MenuItem();
		markUnindexable.setTitle(I18N.message("markunindexable"));
		markUnindexable.addClickHandler(event -> DocumentService.Instance.get().markUnindexable(getSelectedDocIds(list),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						refresh(list);
					}
				}));

		MenuItem markIndexable = new MenuItem();
		markIndexable.setTitle(I18N.message("markindexable"));
		markIndexable.addClickHandler(event -> DocumentService.Instance.get().markIndexable(getSelectedDocIds(list),
				Constants.INDEX_TO_INDEX, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						refresh(list);
					}
				}));

		contextMenu.setItems(preview, downloadIndexed, openInFolder, index, markIndexable, markIndexMetadataOnly,
				markUnindexable);
		contextMenu.showContextMenu();
	}

	private List<Long> getSelectedDocIds(RefreshableListGrid list) {
		ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> docIds = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			docIds.add(selection[i].getAttributeAsLong(DOC_ID));
		return docIds;
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