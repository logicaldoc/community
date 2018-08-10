package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.security.twofactorsauth.TwoFactorsAuthenticationDialog;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of users and a detail area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UsersPanel extends AdminPanel {

	private RefreshableListGrid list;

	private Layout detailsContainer = null;

	final static Canvas SELECT_USER = new HTMLPanel("&nbsp;" + I18N.message("selectuser"));

	private Canvas details = SELECT_USER;

	public UsersPanel() {
		super("users");
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.refresh(new UsersDS(null, true));
				details = SELECT_USER;
				detailsContainer.removeMembers(detailsContainer.getMembers());
				detailsContainer.setMembers(details);
			}
		});
		toolStrip.addSeparator();

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("adduser"));
		toolStrip.addButton(add);
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				showUserDetails(new GUIUser());
			}
		});
		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(list, true);
			}
		});
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		toolStrip.addSeparator();

		ToolStripButton print = new ToolStripButton();
		print.setTitle(I18N.message("print"));
		toolStrip.addButton(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});
		toolStrip.addFill();

		final Layout listing = new VLayout();

		detailsContainer = new VLayout();

		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel as placeholder
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField username = new ListGridField("username", I18N.message("username"), 100);
		username.setCanFilter(true);

		ListGridField name = new ListGridField("name", I18N.message("lastname"), 100);
		name.setCanFilter(true);

		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"), 100);
		firstName.setCanFilter(true);

		ListGridField phone = new ListGridField("phone", I18N.message("phone"), 90);
		phone.setCanFilter(true);

		ListGridField cell = new ListGridField("cell", I18N.message("cell"), 90);
		cell.setCanFilter(true);

		ListGridField email = new ListGridField("email", I18N.message("email"), 200);
		email.setCanFilter(true);

		ListGridField enabled = new ListGridField("eenabled", " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField groups = new ListGridField("groups", I18N.message("groups"), 200);
		groups.setCanFilter(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new UsersDS(null, true));
		list.setFields(id, enabled, username, firstName, name, email, phone, cell, groups);

		listing.addMember(infoPanel);
		listing.addMember(list);

		detailsContainer.setAlign(Alignment.CENTER);
		details = SELECT_USER;
		detailsContainer.addMember(details);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record record = list.getSelectedRecord();
				if (record != null)
					SecurityService.Instance.get().getUser(Long.parseLong(record.getAttributeAsString("id")),
							new AsyncCallback<GUIUser>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUIUser user) {
									showUserDetails(user);
								}
							});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showusers", Integer.toString(list.getTotalRows())));
			}
		});

		body.addMembers(toolStrip, listing, detailsContainer);
	}

	/**
	 * Updates the selected record with new data
	 */
	public void updateRecord(GUIUser user) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, user.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", user.getId());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("username", user.getUserName());
		record.setAttribute("name", user.getName());
		record.setAttribute("firstName", user.getFirstName());
		record.setAttribute("email", user.getEmail());
		record.setAttribute("cell", user.getCell());
		record.setAttribute("phone", user.getPhone());
		if (user.isEnabled())
			record.setAttribute("eenabled", "0");
		else
			record.setAttribute("eenabled", "2");

		GUIGroup[] groups = user.getGroups();
		ArrayList<String> gnames = new ArrayList<String>();
		for (GUIGroup group : groups) {
			if (!group.getName().startsWith("_user_")) {
				if (!gnames.isEmpty())
					gnames.add(", " + group.getName());
				else
					gnames.add(group.getName());
			}
		}
		record.setAttribute("groups", gnames);

		list.refreshRow(list.getRecordIndex(record));
	}

	public void showUserDetails(GUIUser user) {
		if (!(details instanceof UserDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new UserDetailsPanel(UsersPanel.this);
			detailsContainer.addMember(details);
		}
		((UserDetailsPanel) details).setUser(user);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SecurityService.Instance.get().deleteUser(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									details = SELECT_USER;
									detailsContainer.setMembers(details);
								}
							});
						}
					}
				});
			}
		});

		MenuItem password = new MenuItem();
		password.setTitle(I18N.message("changepassword"));
		password.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SetPassword dialog = new SetPassword(Long.parseLong(record.getAttributeAsString("id")));
				dialog.show();
			}
		});
		password.setEnabled(!Session.get().isDemo());

		MenuItem twoTactorsAuth = new MenuItem();
		twoTactorsAuth.setTitle(I18N.message("twofactorsauth"));
		twoTactorsAuth.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SecurityService.Instance.get().getUser(record.getAttributeAsLong("id"), new AsyncCallback<GUIUser>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIUser user) {
						TwoFactorsAuthenticationDialog dialog = new TwoFactorsAuthenticationDialog(user, true);
						dialog.show();
					}
				});
			}
		});
		twoTactorsAuth.setEnabled(!Session.get().isDemo());

		if (Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION))
			contextMenu.setItems(password, twoTactorsAuth, delete);
		else
			contextMenu.setItems(password, delete);

		if ("admin".equals(record.getAttributeAsString("username"))) {
			delete.setEnabled(false);
			if (!Session.get().getUser().getUserName().equalsIgnoreCase("admin")) {
				password.setEnabled(false);
			}
		}

		contextMenu.showContextMenu();
	}
}