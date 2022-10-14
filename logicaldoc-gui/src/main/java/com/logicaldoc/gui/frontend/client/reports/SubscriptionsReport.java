package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.SubscriptionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderSubscriptionOptionListGridField;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of subscriptions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class SubscriptionsReport extends ReportPanel implements FolderChangeListener {

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
		max.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (max.validate())
					refresh();
			}
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		userSelector = ItemFactory.newUserSelector("user", "user", null, false, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(150);
		userSelector.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});
		toolStrip.addFormItem(userSelector);

		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		typeSelector = ItemFactory.newSubscriptionTypeSelector();
		typeSelector.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});
		toolStrip.addFormItem(typeSelector);
	}

	@Override
	protected void prepareListGrid() {
		ListGridField userId = new ColoredListGridField("userId", "userId");
		userId.setWidth(50);
		userId.setCanEdit(false);
		userId.setHidden(true);

		ListGridField userName = new UserListGridField("userName", "userId", "user");
		userName.setCanEdit(false);

		ListGridField created = new DateListGridField("created", "subscription");

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
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		list.setFields(id, userId, userName, created, option, icon, path, events);
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++) {
			ids[i] = Long.parseLong(selection[i].getAttribute("id"));
		}

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							AuditService.Instance.get().deleteSubscriptions(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
								}
							});
						}
					}
				});
			}
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SubscriptionDialog dialog = new SubscriptionDialog(list);
				dialog.show();
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				String type = record.getAttribute("type");
				String id = record.getAttribute("objectid");
				if ("folder".equals(type))
					DocumentsPanel.get().openInFolder(Long.parseLong(id), null);
				else {
					DocumentService.Instance.get().getById(Long.parseLong(id), new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument result) {
							DocumentsPanel.get().openInFolder(result.getFolder().getId(), result.getId());
						}
					});
				}
			}
		});

		contextMenu.setItems(openInFolder, edit, delete);
		contextMenu.showContextMenu();
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}