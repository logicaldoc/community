package com.logicaldoc.gui.frontend.client.reports;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.data.DuplicatesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.GroupStartOpen;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of duplicates documents in a tabular way.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class DuplicatesReport extends ReportPanel implements FolderChangeListener {

	private static final String NEWEST = "newest";

	private static final String DIGEST = "digest";

	private static final String FILENAME = "filename";

	private SpinnerItem max;

	private FolderSelector folderSelector;

	public DuplicatesReport() {
		super("duplicates", "showndocuments");
	}

	@Override
	protected void fillToolBar(ToolStrip toolStrip) {
		max = ItemFactory.newSpinnerItem("max", "", 500, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler(event -> refresh());

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(click -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		folderSelector = new FolderSelector("folder", null);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		final SelectItem groupBy = new SelectItem("groupBy", I18N.message("groupby"));
		groupBy.setWrapTitle(false);
		groupBy.setWidth(100);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("", " ");
		map.put(FILENAME, I18N.message(FILENAME));
		map.put(DIGEST, I18N.message(DIGEST));
		groupBy.setValueMap(map);
		groupBy.setPickListWidth(100);
		groupBy.addChangedHandler((ChangedEvent event) -> {
			if (event.getValue() != null && !"".equals(event.getValue())) {
				list.ungroup();
				list.groupBy((String) event.getValue());
			}
		});
		toolStrip.addFormItem(groupBy);

		ToolStripButton deDuplicate = new ToolStripButton();
		deDuplicate.setTitle(I18N.message("deduplicate"));
		deDuplicate.addClickHandler((ClickEvent event) -> {
			RadioGroupItem maintain = ItemFactory.newRadioGroup("maintain", I18N.message("maintain"));
			maintain.setRequired(true);
			maintain.setEndRow(true);
			LinkedHashMap<String, String> mp = new LinkedHashMap<>();
			mp.put(NEWEST, I18N.message(NEWEST));
			mp.put("oldest", I18N.message("oldest"));
			maintain.setValueMap(mp);
			maintain.setValue(NEWEST);

			LD.askForValue(I18N.message("deduplicate"), I18N.message("deduplicatequestion"), NEWEST, maintain, null,
					value -> {
						if (value != null) {
							LD.contactingServer();
							DocumentService.Instance.get().deDuplicate(folderSelector.getFolderId(),
									NEWEST.equals(value), new DefaultAsyncCallback<>() {
										@Override
										public void onSuccess(Void arg) {
											LD.clearPrompt();
											refresh();
										}
									});
						}
					});
		});
		toolStrip.addSeparator();
		toolStrip.addButton(deDuplicate);
	}

	@Override
	protected void prepareListGrid() {

		ListGridField id = new ColoredListGridField("id");
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setCanFilter(false);
		size.setCanGroupBy(false);

		ListGridField version = new VersionListGridField();
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);
		version.setCanGroupBy(false);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified",
				DateCellFormatter.FORMAT_LONG);
		lastModified.setCanGroupBy(false);

		ListGridField publisher = new ColoredListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);
		publisher.setCanFilter(true);
		publisher.setHidden(true);
		publisher.setCanGroupBy(false);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);

		ListGridField digest = new ColoredListGridField(DIGEST, I18N.message(DIGEST), 250);
		digest.setType(ListGridFieldType.TEXT);
		digest.setCanFilter(true);

		ListGridField filename = new FileNameListGridField(FILENAME, "icon", I18N.message(FILENAME), 200);
		filename.setCanFilter(true);

		ListGridField folderName = new FolderListGridField("foldername", "folder");
		folderName.setWidth(200);
		folderName.setCanFilter(true);

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);

		// Initial group by
		list.setGroupStartOpen(GroupStartOpen.ALL);
		list.setGroupByField(DIGEST);

		filename.setGroupTitleRenderer((groupValue, groupNode, field, fieldName, grid) -> I18N.message(FILENAME) + ": "
				+ groupValue.toString());

		digest.setGroupTitleRenderer(
				(groupValue, groupNode, field, fieldName, grid) -> I18N.message(DIGEST) + ": " + groupValue.toString());

		list.setCanDrag(true);
		list.setCanDragRecordsOut(true);
		list.setFields(filename, folderName, lastModified, size, version, publisher, customId, digest, type);

		list.addDoubleClickHandler(event -> {
			String idValue = list.getSelectedRecord().getAttribute("id");
			if (FolderController.get().getCurrentFolder().isDownload())
				DocUtil.download(Long.parseLong(idValue), null);
		});
	}

	@Override
	protected void refresh() {
		Long folderId = folderSelector.getFolderId();
		int maxElements = max.getValueAsInteger();
		list.refresh(new DuplicatesDS(folderId, maxElements));
	}

	@Override
	protected void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem delete = prepareDeleteContextMenuItem(selection);

		MenuItem preview = preparePreviewContextMenuItem();

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> {
			Long id = list.getSelectedRecord().getAttributeAsLong("id");
			WindowUtils.openUrl(Util.downloadURL(id));
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString("folderId")),
					Long.parseLong(rec.getAttributeAsString("id")));
		});

		if (!(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1)) {
			download.setEnabled(false);
			preview.setEnabled(false);
			openInFolder.setEnabled(false);
		}

		for (ListGridRecord rec : selection) {
			if (!"blank".equals(rec.getAttribute("locked")) || !"blank".equals(rec.getAttribute("immutable"))) {
				delete.setEnabled(false);
			}
		}

		contextMenu.setItems(download, preview, delete, openInFolder);
		contextMenu.showContextMenu();
	}

	private MenuItem preparePreviewContextMenuItem() {
		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
		preview.addClickHandler(event -> {
			long id = Long.parseLong(list.getSelectedRecord().getAttribute("id"));
			DocumentService.Instance.get().getById(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					PreviewPopup iv = new PreviewPopup(doc);
					iv.show();
				}
			});
		});
		return preview;
	}

	private MenuItem prepareDeleteContextMenuItem(final ListGridRecord[] selection) {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), yes -> {
				if (Boolean.TRUE.equals(yes)) {
					DocumentService.Instance.get().delete(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void result) {
							list.removeSelectedData();
							DocumentsPanel.get().getDocumentsMenu().refresh("trash");
						}
					});
				}
			});
		});
		return delete;
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
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