package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.MenusDS;
import com.logicaldoc.gui.common.client.formatters.I18NCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
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

	static final Canvas SELECT_MENU = new HTMLPanel("&nbsp;" + I18N.message("selectmenu"));

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

		menus.addSelectionChangedHandler(event -> {
			Record rec = menus.getSelectedRecord();
			if (rec != null)
				SecurityService.Instance.get().getMenu(Long.parseLong(rec.getAttributeAsString("id")), I18N.getLocale(),
						new AsyncCallback<GUIMenu>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIMenu menu) {
								showRights(menu);
							}
						});
		});
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param user the user to update
	 */
	public void updateRecord(GUIUser user) {
		ListGridRecord rec = menus.getSelectedRecord();
		if (rec == null)
			rec = new ListGridRecord();

		rec.setAttribute("username", user.getUsername());
		rec.setAttribute("name", user.getName());
		rec.setAttribute("firstName", user.getFirstName());
		rec.setAttribute("email", user.getEmail());
		rec.setAttribute("cell", user.getCell());
		rec.setAttribute("phone", user.getPhone());
		if (user.isEnabled())
			rec.setAttribute("eenabled", "0");
		else
			rec.setAttribute("eenabled", "2");

		if (rec.getAttributeAsString("id") != null
				&& (user.getId() == Long.parseLong(rec.getAttributeAsString("id")))) {
			menus.refreshRow(menus.getRecordIndex(rec));
		} else {
			// Append a new rec
			rec.setAttribute("id", user.getId());
			menus.addData(rec);
			menus.selectRecord(rec);
		}
	}

	public void showRights(GUIMenu menu) {
		rightsContainer.removeMember(rights);
		rights = new MenuRightsPanel(menu, true);
		rightsContainer.addMember(rights);
	}
}