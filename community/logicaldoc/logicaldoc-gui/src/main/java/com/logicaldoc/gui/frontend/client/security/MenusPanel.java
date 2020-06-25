package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.MenusDS;
import com.logicaldoc.gui.common.client.formatters.I18NCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * This panel shows the list of menus visible by the current user allowing for
 * security management.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MenusPanel extends VLayout {
	private TreeGrid menus;

	private Layout listing = new VLayout();

	private Layout rightsContainer = new VLayout();

	final static Canvas SELECT_MENU = new HTMLPanel("&nbsp;" + I18N.message("selectmenu"));

	private Canvas rights = SELECT_MENU;

	public MenusPanel() {
		setWidth100();

		// Initialize the listing panel as placeholder
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 40);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 350);
		name.setCanFilter(true);
		name.setCellFormatter(new I18NCellFormatter());

		menus = new TreeGrid();
		menus.setWidth100();
		menus.setShowHeader(false);
		menus.setLeaveScrollbarGap(false);
		menus.setCanReorderRecords(false);
		menus.setCanDragRecordsOut(false);
		menus.setAutoFetchData(true);
		menus.setLoadDataOnDemand(true);
		menus.setCanSelectAll(false);
		menus.setShowConnectors(true);
		menus.setShowRoot(false);
		menus.setCanAcceptDrop(false);
		menus.setCanAcceptDroppedRecords(false);
		menus.setNodeIcon(Util.imageUrl("cube_yellow16.png"));
		menus.setFolderIcon(Util.imageUrl("cube_yellow16.png"));
		menus.setDataSource(new MenusDS());
		menus.setFields(id, name);

		listing.addMember(menus);

		rightsContainer.setAlign(Alignment.CENTER);
		rightsContainer.addMember(rights);

		setMembers(listing, rightsContainer);

		menus.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record record = menus.getSelectedRecord();
				if (record != null)
					SecurityService.Instance.get().getMenu(Long.parseLong(record.getAttributeAsString("id")),
							new AsyncCallback<GUIMenu>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUIMenu menu) {
									showRights(menu);
								}
							});
			}
		});
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param user the user to update
	 */
	public void updateRecord(GUIUser user) {
		ListGridRecord record = menus.getSelectedRecord();
		if (record == null)
			record = new ListGridRecord();

		record.setAttribute("username", user.getUsername());
		record.setAttribute("name", user.getName());
		record.setAttribute("firstName", user.getFirstName());
		record.setAttribute("email", user.getEmail());
		record.setAttribute("cell", user.getCell());
		record.setAttribute("phone", user.getPhone());
		if (user.isEnabled())
			record.setAttribute("eenabled", "0");
		else
			record.setAttribute("eenabled", "2");

		if (record.getAttributeAsString("id") != null
				&& (user.getId() == Long.parseLong(record.getAttributeAsString("id")))) {
			menus.refreshRow(menus.getRecordIndex(record));
		} else {
			// Append a new record
			record.setAttribute("id", user.getId());
			menus.addData(record);
			menus.selectRecord(record);
		}
	}

	public void showRights(GUIMenu menu) {
		rightsContainer.removeMember(rights);
		rights = new MenuRightsPanel(menu);
		rightsContainer.addMember(rights);
	}
}