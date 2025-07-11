package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.LockedDocsDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.FileVersionListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of locked documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class LockedDocsReport extends ReportPanel {

	private static final String IMMUTABLE = "immutable";

	private SelectItem userSelector;

	private SpinnerItem max;

	public LockedDocsReport() {
		super("lockeddocs", "showndocuments");
	}

	@Override
	protected void fillToolBar(ToolStrip toolStrip) {
		max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setStep(10);
		max.setShowTitle(false);
		max.addChangedHandler(event -> refresh());

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(event -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		userSelector = ItemFactory.newUserSelector("user", "user", null, false, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(150);
		userSelector.addChangedHandler(changed -> refresh());
		toolStrip.addFormItem(userSelector);
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
		version.setCanFilter(false);
		version.setCanGroupBy(false);

		FileVersionListGridField fileVersion = new FileVersionListGridField();
		fileVersion.setCanFilter(false);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified",
				DateCellFormatter.FORMAT_LONG);
		lastModified.setCanGroupBy(false);

		ListGridField user = new UserListGridField("username", "userId", "lockedby");
		user.setCanFilter(true);
		user.setCanGroupBy(true);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);
		customId.setCanFilter(true);

		FileNameListGridField filename = new FileNameListGridField();
		filename.setWidth(200);
		filename.setCanFilter(true);

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);
		type.setCanFilter(true);

		ListGridField statusIcons = new ColoredListGridField("statusIcons", " ");
		statusIcons.setWidth(110);
		statusIcons.setCanFilter(false);
		statusIcons.setCanSort(false);
		statusIcons.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			String color = rec.getAttributeAsString("color");

			String content = "<div style='display: flex; text-align: center; justify-content: center;'>";

			// Put the status icon
			if (rec.getAttribute("status") != null) {
				Integer status = rec.getAttributeAsInt("status");
				if (status != null && status.intValue() > 0)
					content += AwesomeFactory.getLockedButtonHTML(status, rec.getAttributeAsString("lockUser"), color);
			}

			// Put the immutable icon
			if (rec.getAttribute(IMMUTABLE) != null) {
				Integer immutable = rec.getAttributeAsInt(IMMUTABLE);
				if (immutable != null && immutable.intValue() == 1)
					content += AwesomeFactory.getIconButtonHTML("hand-paper", null, IMMUTABLE, color, null);
			}

			content += "</div>";
			return content;
		});

		list.setFields(statusIcons, filename, version, fileVersion, size, lastModified, user, customId, type);

		list.addDoubleClickHandler(event -> DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null));
	}

	@Override
	protected void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem unlock = new MenuItem();
		unlock.setTitle(I18N.message("unlock"));
		unlock.addClickHandler((MenuItemClickEvent event) -> {
			if (selection == null || selection.length == 0)
				return;

			DocumentService.Instance.get().unlock(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void result) {
					refresh();
				}
			});
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
		preview.addClickHandler(click -> {
			long id = Long.parseLong(list.getSelectedRecord().getAttribute("id"));
			DocumentService.Instance.get().getById(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					new PreviewPopup(doc).show();
				}
			});
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(click -> DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null));

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(click -> {
			ListGridRecord rec = list.getSelectedRecord();
			DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString("folderId")),
					Long.parseLong(rec.getAttributeAsString("id")));
		});

		if (!(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1)) {
			download.setEnabled(false);
			preview.setEnabled(false);
			openInFolder.setEnabled(false);
		}

		contextMenu.setItems(download, preview, unlock, openInFolder);
		contextMenu.showContextMenu();
	}

	@Override
	protected void refresh() {
		Long userId = null;
		if (userSelector.getValueAsString() != null && !"".equals(userSelector.getValueAsString()))
			userId = Long.parseLong(userSelector.getValueAsString());
		list.refresh(new LockedDocsDS(userId, max.getValueAsInteger()));
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