package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.SubscriptionsDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderSubscriptionOptionListGridField;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
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
 * This panel shows a list of subscriptions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class SubscriptionsReport extends ReportPanel implements FolderChangeListener {

	private static final String USER_ID = "userId";

	protected SelectItem userSelector;

	protected FolderSelector folderSelector;

	protected SelectItem typeSelector;

	protected SpinnerItem max;

	public SubscriptionsReport() {
		super("subscriptions", "shownsubscriptions");
	}

	@Override
	protected void refresh() {
		Long folderId = folderSelector.getFolderId();
		Long userId = null;
		if (userSelector.getValueAsString() != null && !"".equals(userSelector.getValueAsString()))
			userId = Long.parseLong(userSelector.getValueAsString());
		String type = null;
		if (typeSelector.getValueAsString() != null && !"".equals(typeSelector.getValueAsString()))
			type = typeSelector.getValueAsString();

		list.refresh(new SubscriptionsDS(folderId, userId, type, null, max.getValueAsInteger()));
	}

	@Override
	protected void fillToolBar(ToolStrip toolStrip) {
		max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler((ChangedEvent event) -> refresh());

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(click -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		userSelector = ItemFactory.newUserSelector("user", "user", null, false, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(150);
		userSelector.addChangedHandler((ChangedEvent event) -> refresh());
		toolStrip.addFormItem(userSelector);

		folderSelector = new FolderSelector("folder", null);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		typeSelector = ItemFactory.newSubscriptionTypeSelector();
		typeSelector.addChangedHandler((ChangedEvent event) -> refresh());
		toolStrip.addFormItem(typeSelector);
	}

	@Override
	protected void prepareListGrid() {
		ListGridField userId = new ColoredListGridField(USER_ID, USER_ID);
		userId.setWidth(50);
		userId.setCanEdit(false);
		userId.setHidden(true);

		ListGridField userName = new UserListGridField("userName", USER_ID, "user");
		userName.setCanEdit(false);

		ListGridField created = new DateListGridField("created", "subscription", DateCellFormatter.FORMAT_LONG);

		ColoredListGridField option = new FolderSubscriptionOptionListGridField();

		ListGridField events = new EventsListGridField();
		events.setWidth(200);

		ListGridField path = new ColoredListGridField("path", I18N.message("path"));
		path.setWidth("*");
		path.setCanEdit(false);

		ListGridField id = new ColoredListGridField("id");
		id.setWidth(50);
		id.setHidden(true);

		ListGridField icon = new ListGridField("icon", " ", 25);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".svg");
		icon.setCanFilter(false);

		list.setFields(id, userId, userName, created, option, icon, path, events);
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				AuditService.Instance.get().deleteSubscriptions(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
					}
				});
			}
		}));

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(event -> new SubscriptionDialog(list).show());

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			String type = rec.getAttribute("type");
			String id = rec.getAttribute("objectid");
			if ("folder".equals(type))
				DocumentsPanel.get().openInFolder(Long.parseLong(id), null);
			else {
				DocumentService.Instance.get().getById(Long.parseLong(id), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIDocument result) {
						DocumentsPanel.get().openInFolder(result.getFolder().getId(), result.getId());
					}
				});
			}
		});

		contextMenu.setItems(openInFolder, edit, delete);
		contextMenu.showContextMenu();
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