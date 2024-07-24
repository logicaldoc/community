package com.logicaldoc.gui.frontend.client.subscription;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.SubscriptionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IconGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the user's subscriptions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class PersonalSubscriptions extends com.smartgwt.client.widgets.Window {

	private ListGrid list;

	private Layout listing = new VLayout();

	public PersonalSubscriptions() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("subscriptions"));
		setWidth(400);
		setHeight(350);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		// Initialize the listing panel as placeholder
		listing.setAlign(Alignment.CENTER);
		listing.setHeight100();
		listing.setWidth100();
		initListGrid();

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> initListGrid());
		toolStrip.addFill();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.addMember(toolStrip);
		layout.addMember(listing);

		addItem(layout);
	}

	private void initListGrid() {
		if (list != null) {
			listing.removeMember(list);
			list.destroy();
		}

		ListGridField id = new ColoredListGridField("id");
		id.setWidth(50);
		id.setHidden(true);

		ListGridField icon = new IconGridField();

		FileNameListGridField name = new FileNameListGridField("name", "icon", "name", 210);
		name.setWidth("*");
		name.setCanFilter(true);

		ListGridField created = new DateListGridField("created", "date");

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(false);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setDataSource(new SubscriptionsDS(null, null));
		list.setFields(id, icon, name, created);

		list.sort(1, SortDirection.DESCENDING);
		listing.addMember(list);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addDoubleClickHandler(event -> {
			String type = list.getSelectedRecord().getAttribute("type");
			Long oid = list.getSelectedRecord().getAttributeAsLong("objectid");
			if ("document".equals(type)) {
				DocUtil.download(oid, null);
			} else {
				DocumentsPanel.get().openInFolder(oid, null);
				close();
			}
		});

		// Count the total of events and the total of unchecked events
		list.addDataArrivedHandler(e -> {
			int total = list.getTotalRows();
			Session.get().getUser().setSubscriptions(total);
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		List<Long> ids = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			ids.add(selection[i].getAttributeAsLong("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						AuditService.Instance.get().deleteSubscriptions(ids, new AsyncCallback<>() {
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
				DocumentService.Instance.get().getById(Long.parseLong(id), new AsyncCallback<>() {

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
		});

		contextMenu.setItems(openInFolder, edit, delete);
		contextMenu.showContextMenu();
	}
}