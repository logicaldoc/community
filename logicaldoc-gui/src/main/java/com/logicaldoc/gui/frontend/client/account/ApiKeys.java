package com.logicaldoc.gui.frontend.client.account;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.data.ApiKeysDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.CopyTextFormItemIcon;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the user's API Keys.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class ApiKeys extends com.smartgwt.client.widgets.Window {

	private RefreshableListGrid list;

	public ApiKeys() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("apikeys"));
		setWidth(600);
		setHeight(300);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		setAutoSize(true);
		centerInPage();

		initGUI();
	}

	private void initGUI() {
		ToolStripButton newKey = new ToolStripButton(I18N.message("createnewapikey"));

		newKey.addClickHandler(click -> LD.askForString("createnewapikey", "createnewapikeymessage", "My Key",
				keyName -> SecurityService.Instance.get().createApiKey(keyName, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(String apikey) {
						list.refresh(new ApiKeysDS());
						StaticTextItem item = ItemFactory.newStaticTextItem("apikey", "apikey", apikey);
						item.setIcons(new CopyTextFormItemIcon(apikey, "copytext"));
						LD.askForValue("saveyourkey", "saveyourkeymessage", apikey, item, value -> {
							list.refresh(new ApiKeysDS());
							Util.copyText(apikey);
							destroy();
						});
						
					}
				})));

		ToolStrip buttons = new ToolStrip();
		buttons.addButton(newKey);
		buttons.addFill();
		buttons.setWidth100();

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);
		name.setCanEdit(true);
		name.addCellSavedHandler(
				saved -> SecurityService.Instance.get().updateApiKey(saved.getRecord().getAttributeAsLong("id"),
						saved.getNewValue() != null ? saved.getNewValue().toString() : null, new DefaultAsyncCallback<>() {

							@Override
							public void onSuccess(Void arg) {
								// Nothing to do
							}
						}));

		ListGridField key = new ListGridField("key", I18N.message("secretkey"), 150);
		key.setCanEdit(false);

		ListGridField creation = new DateListGridField("creation", "createdon");
		creation.setCanEdit(false);

		ListGridField lastUsed = new DateListGridField("lastUsed", "lastused");
		lastUsed.setCanEdit(false);

		list = new RefreshableListGrid(new ApiKeysDS());
		list.setWidth100();
		list.setHeight100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setCanEdit(true);
		list.setEditByCell(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(id, name, key, creation, lastUsed);

		list.addCellContextClickHandler(click -> {
			showContextMenu();
			click.cancel();
		});

		addItem(buttons);
		addItem(list);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				SecurityService.Instance.get().deleteApiKey(list.getSelectedRecord().getAttributeAsLong("id"),
						new DefaultAsyncCallback<>() {

							@Override
							public void onSuccess(Void result) {
								list.removeSelectedData();
								list.deselectAllRecords();
								list.refresh(new ApiKeysDS());
							}
						});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ApiKeys)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}