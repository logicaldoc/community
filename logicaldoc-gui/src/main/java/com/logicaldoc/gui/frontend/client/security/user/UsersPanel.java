package com.logicaldoc.gui.frontend.client.security.user;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.formatters.UserCellFormatter;
import com.logicaldoc.gui.common.client.grid.formatters.UserDateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.security.twofactorsauth.TwoFactorsAuthenticationDialog;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of users and a detail area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UsersPanel extends AdminPanel {

	private static final String SFA = "sfa";

	private static final String DEPARTMENT = "department";

	private static final String BUILDING = "building";

	private static final String COMPANY = "company";

	private static final String ADMIN = "admin";

	private static final String SOURCE = "source";

	private static final String GROUPS = "groups";

	private static final String GUEST = "guest";

	private static final String ENABLED = "eenabled";

	private static final String EMAIL = "email";

	private static final String PHONE = "phone";

	private static final String USERNAME = "username";

	private RefreshableListGrid list;

	private Layout detailsContainer = null;

	static final Canvas SELECT_USER = new HTMLPanel("&nbsp;" + I18N.message("selectuser"));

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
		refresh.addClickHandler(event -> {
			list.refresh(new UsersDS(null, true, false));
			details = SELECT_USER;
			detailsContainer.removeMembers(detailsContainer.getMembers());
			detailsContainer.setMembers(details);
		});
		toolStrip.addSeparator();

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("adduser"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> {
			list.deselectAllRecords();
			showUserDetails(new GUIUser());
		});
		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(event -> GridUtil.exportCSV(list, true));
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		toolStrip.addSeparator();

		ToolStripButton print = new ToolStripButton();
		print.setTitle(I18N.message("print"));
		toolStrip.addButton(print);
		print.addClickHandler(event -> GridUtil.print(list));
		toolStrip.addFill();

		final Layout listing = new VLayout();

		detailsContainer = new VLayout();

		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel as placeholder
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();
		id.setCellFormatter(new UserCellFormatter());

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		username.setCanFilter(true);
		username.setCellFormatter(new UserCellFormatter());

		ListGridField name = new ListGridField("name", I18N.message("lastname"), 100);
		name.setCanFilter(true);
		name.setCellFormatter(new UserCellFormatter());

		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"), 100);
		firstName.setCanFilter(true);
		firstName.setCellFormatter(new UserCellFormatter());

		ListGridField phone = new ListGridField(PHONE, I18N.message(PHONE), 90);
		phone.setCanFilter(true);
		phone.setCellFormatter(new UserCellFormatter());

		ListGridField cell = new ListGridField("cell", I18N.message("cell"), 90);
		cell.setCanFilter(true);
		cell.setCellFormatter(new UserCellFormatter());

		ListGridField city = new ListGridField("city", I18N.message("city"), 90);
		city.setCanFilter(true);
		city.setHidden(true);
		city.setCellFormatter(new UserCellFormatter());

		ListGridField company = new ListGridField(COMPANY, I18N.message(COMPANY), 100);
		company.setCanFilter(true);
		company.setHidden(true);
		company.setCellFormatter(new UserCellFormatter());

		ListGridField building = new ListGridField(BUILDING, I18N.message(BUILDING), 100);
		building.setCanFilter(true);
		building.setHidden(true);
		building.setCellFormatter(new UserCellFormatter());

		ListGridField department = new ListGridField(DEPARTMENT, I18N.message(DEPARTMENT), 100);
		department.setCanFilter(true);
		department.setHidden(true);
		department.setCellFormatter(new UserCellFormatter());

		ListGridField organizationalUnit = new ListGridField("organizationalUnit", I18N.message("organizationalunit"),
				100);
		organizationalUnit.setCanFilter(true);
		organizationalUnit.setHidden(true);
		organizationalUnit.setCellFormatter(new UserCellFormatter());

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL), 200);
		email.setCanFilter(true);
		email.setCellFormatter(new UserCellFormatter());

		DateListGridField expire = new DateListGridField("expire", "expireson",
				DateListGridField.DateCellFormatter.FORMAT_SHORT);
		expire.setCellFormatter(new UserDateCellFormatter());

		DateListGridField lastLogin = new DateListGridField("lastLogin", "lastlogin",
				DateListGridField.DateCellFormatter.FORMAT_DEFAULT);
		lastLogin.setAutoFitWidth(true);
		lastLogin.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		lastLogin.setCellFormatter(new UserDateCellFormatter(false));

		DateListGridField creation = new DateListGridField("creation", "createdon",
				DateListGridField.DateCellFormatter.FORMAT_DEFAULT);
		creation.setAutoFitWidth(true);
		creation.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		creation.setCellFormatter(new UserDateCellFormatter(false));
		creation.setHidden(true);

		ListGridField enabled = new EnabledListGridField();

		ListGridField guest = new ListGridField(GUEST, I18N.message(GUEST), 55);
		guest.setCanFilter(true);
		guest.setHidden(true);

		ListGridField groups = new ListGridField(GROUPS, I18N.message(GROUPS), 200);
		groups.setCanFilter(true);
		groups.setCellFormatter(new UserCellFormatter());

		ListGridField timeZone = new ListGridField("timeZone", I18N.message("timezone"), 120);
		timeZone.setHidden(true);
		timeZone.setAlign(Alignment.CENTER);
		timeZone.setCanFilter(true);

		UserListGridField avatar = new UserListGridField(true);

		ListGridField source = new ListGridField(SOURCE, I18N.message(SOURCE), 60);
		source.setCanFilter(true);
		source.setHidden(true);
		source.setAlign(Alignment.CENTER);

		ListGridField secondFactor = new ListGridField(SFA, I18N.message(SFA), 80);
		secondFactor.setCanFilter(true);
		secondFactor.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new UsersDS(null, true, false));
		list.setFields(id, enabled, avatar, username, firstName, name, email, creation, lastLogin, expire, company,
				department, building, organizationalUnit, city, phone, cell, groups, guest, timeZone, source,
				secondFactor);

		listing.addMember(infoPanel);
		listing.addMember(list);

		detailsContainer.setAlign(Alignment.CENTER);
		details = SELECT_USER;
		detailsContainer.addMember(details);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				onSelectUser(rec.getAttributeAsLong("id"));
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showusers", Integer.toString(list.getTotalRows()))));

		body.addMembers(toolStrip, listing, detailsContainer);
	}

	private void onSelectUser(long userId) {
		SecurityService.Instance.get().getUser(userId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIUser user) {
				showUserDetails(user);
			}
		});
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param user the suer to update
	 */
	public void updateRecord(GUIUser user) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, user.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", user.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("avatar", user.getId());
		rec.setAttribute(USERNAME, user.getUsername());
		rec.setAttribute("name", user.getName());
		rec.setAttribute("firstName", user.getFirstName());
		rec.setAttribute(EMAIL, user.getEmail());
		rec.setAttribute("cell", user.getCell());
		rec.setAttribute(PHONE, user.getPhone());
		rec.setAttribute("expire", user.getExpire());
		rec.setAttribute(ENABLED, user.isEnabled());
		rec.setAttribute(GUEST, user.isReadOnly());
		rec.setAttribute(SOURCE, user.getSource() == GUIUser.TYPE_DEFAULT ? "DEFAULT" : user.getSource());
		rec.setAttribute("city", user.getCity());
		rec.setAttribute(BUILDING, user.getBuilding());
		rec.setAttribute("organizationalUnit", user.getOrganizationalUnit());
		rec.setAttribute(DEPARTMENT, user.getDepartment());
		rec.setAttribute(COMPANY, user.getCompany());
		rec.setAttribute(SFA, user.getSecondFactor() != null ? user.getSecondFactor() : "");
		rec.setAttribute(GROUPS, user.getGroups().stream().filter(g -> g.getType() == GUIGroup.TYPE_DEFAULT)
				.map(g -> g.getName()).collect(Collectors.joining(",")));

		list.refreshRow(list.getRecordIndex(rec));
		list.redraw();
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

		final ListGridRecord[] selectedUsers = list.getSelectedRecords();
		final long selectedUserId = Long.parseLong(selectedUsers[0].getAttributeAsString("id"));

		MenuItem delete = prepareDeleteMenuItem(selectedUserId);

		MenuItem password = preparePasswordMenuItem(selectedUsers);

		MenuItem replicate = prepareReplicateMenuItem(selectedUsers);

		MenuItem twoTactorsAuth = prepareTwoFactorsAuth(selectedUsers);

		MenuItem enableUser = prepareEnableUserMenuItem();

		MenuItem disableUser = prepareDisableUserMenuItem();

		List<MenuItem> items = new ArrayList<>();
		if (!Session.get().isAdmin()) {
			if (Boolean.TRUE.equals(list.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
				items.add(disableUser);
			else
				items.add(enableUser);
		}
		items.add(password);

		if (!ADMIN.equals(list.getSelectedRecord().getAttributeAsString(USERNAME))) {
			if (Boolean.TRUE.equals(list.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
				items.add(disableUser);
			else
				items.add(enableUser);
		}

		if (Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION))
			items.add(twoTactorsAuth);

		items.add(replicate);
		items.add(new MenuItemSeparator());
		items.add(delete);

		contextMenu.setItems(items.toArray(new MenuItem[0]));

		password.setEnabled(selectedUsers.length == 1 && !Session.get().isDemo()
				&& "DEFAULT".equals(selectedUsers[0].getAttributeAsString(SOURCE)));
		twoTactorsAuth.setEnabled(selectedUsers.length == 1 && !Session.get().isDemo());
		delete.setEnabled(selectedUsers.length == 1 && !Session.get().isDemo());
		enableUser.setEnabled(selectedUsers.length == 1);
		disableUser.setEnabled(selectedUsers.length == 1);
		replicate.setEnabled(!Session.get().isDemo());

		if (ADMIN.equals(selectedUsers[0].getAttributeAsString(USERNAME))) {
			delete.setEnabled(false);
			if (!Session.get().getUser().getUsername().equalsIgnoreCase(ADMIN)) {
				password.setEnabled(false);
			}
		}

		contextMenu.showContextMenu();
	}

	private MenuItem prepareDisableUserMenuItem() {
		MenuItem disableUser = new MenuItem();
		disableUser.setTitle(I18N.message("disable"));
		disableUser.addClickHandler(event -> SecurityService.Instance.get()
				.changeStatus(list.getSelectedRecord().getAttributeAsLong("id"), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
						onSelectUser(list.getSelectedRecord().getAttributeAsLong("id"));
					}
				}));
		return disableUser;
	}

	private MenuItem prepareEnableUserMenuItem() {
		MenuItem enableUser = new MenuItem();
		enableUser.setTitle(I18N.message("enable"));
		enableUser.addClickHandler(event -> SecurityService.Instance.get()
				.changeStatus(list.getSelectedRecord().getAttributeAsLong("id"), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
						onSelectUser(list.getSelectedRecord().getAttributeAsLong("id"));
					}
				}));
		return enableUser;
	}

	private MenuItem prepareTwoFactorsAuth(final ListGridRecord[] selectedUsers) {
		MenuItem twoTactorsAuth = new MenuItem();
		twoTactorsAuth.setTitle(I18N.message("twofactorsauth"));
		twoTactorsAuth.addClickHandler(event -> SecurityService.Instance.get()
				.getUser(selectedUsers[0].getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIUser user) {
						TwoFactorsAuthenticationDialog dialog = new TwoFactorsAuthenticationDialog(user, true);
						dialog.show();
					}
				}));
		return twoTactorsAuth;
	}

	private MenuItem prepareReplicateMenuItem(final ListGridRecord[] selectedUsers) {
		MenuItem replicate = new MenuItem();
		replicate.setTitle(I18N.message("replicatesettings"));
		replicate.addClickHandler(event -> {
			List<Long> selectedIds = new ArrayList<>();
			for (ListGridRecord rec : selectedUsers)
				selectedIds.add(rec.getAttributeAsLong("id"));
			ReplicateUserSettings dialog = new ReplicateUserSettings(selectedIds, UsersPanel.this);
			dialog.show();
		});
		return replicate;
	}

	private MenuItem preparePasswordMenuItem(final ListGridRecord[] selectedUsers) {
		MenuItem password = new MenuItem();
		password.setTitle(I18N.message("changepassword"));
		password.addClickHandler(
				event -> new SetPassword(Long.parseLong(selectedUsers[0].getAttributeAsString("id"))).show());
		return password;
	}

	private MenuItem prepareDeleteMenuItem(final long selectedUserId) {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), yes -> {
			if (Boolean.TRUE.equals(yes)) {
				SecurityService.Instance.get().deleteUser(selectedUserId, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						details = SELECT_USER;
						detailsContainer.setMembers(details);
					}
				});
			}
		}));
		return delete;
	}

	void refresh() {
		list.refresh(new UsersDS(null, true, false));
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