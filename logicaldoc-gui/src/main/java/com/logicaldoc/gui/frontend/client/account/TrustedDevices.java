package com.logicaldoc.gui.frontend.client.account;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.DevicesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows the list of the user's trusted devices.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class TrustedDevices extends com.smartgwt.client.widgets.Window {

	private ListGrid list;

	public TrustedDevices() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("trusteddevices"));
		setWidth(600);
		setHeight(300);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		setAutoSize(true);
		centerInPage();

		prepareGrid();
		addItem(list);

		list.fetchData();
	}

	private void prepareGrid() {
		ListGridField id = new ListGridField("id", I18N.message("device"), 80);
		id.setHidden(true);
		id.setCanEdit(false);

		ListGridField label = new ListGridField("label", I18N.message("label"), 150);
		label.setCanEdit(true);
		label.addCellSavedHandler(
				event -> SecurityService.Instance.get().updateDeviceLabel(event.getRecord().getAttributeAsLong("id"),
						event.getNewValue() != null ? event.getNewValue().toString() : null, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								// Nothing to do
							}
						}));

		ListGridField deviceId = new ListGridField("deviceId", I18N.message("deviceid"), 150);
		deviceId.setHidden(true);
		deviceId.setCanEdit(false);

		ListGridField browser = new ListGridField("browser", I18N.message("browser"), 110);
		browser.setCanEdit(false);

		ListGridField os = new ListGridField("os", I18N.message("operativesystem"), 110);
		os.setCanEdit(false);

		ListGridField type = new ListGridField("type", I18N.message("type"), 80);
		type.setAlign(Alignment.CENTER);
		type.setCanEdit(false);

		ListGridField lastlogin = new DateListGridField("lastlogin", "lastlogin");
		os.setCanEdit(false);

		ListGridField creation = new DateListGridField("creation", "createdon");
		creation.setHidden(true);
		creation.setCanEdit(false);

		list = new ListGrid();
		list.setWidth100();
		list.setHeight100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setCanEdit(true);
		list.setEditByCell(true);
		list.setDataSource(new DevicesDS());
		list.setFields(id, label, deviceId, browser, os, type, lastlogin, creation);
		list.sort("date", SortDirection.ASCENDING);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final String[] ids = new String[selection.length];
		for (int i = 0; i < selection.length; i++)
			ids[i] = selection[i].getAttributeAsString("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						SecurityService.Instance.get().deleteTrustedDevices(ids, new AsyncCallback<Void>() {
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
				}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}