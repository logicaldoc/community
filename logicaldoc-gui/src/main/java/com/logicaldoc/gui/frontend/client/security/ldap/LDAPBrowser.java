package com.logicaldoc.gui.frontend.client.security.ldap;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.LDAPService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel is used to perform user searches in the LDAP repositories
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class LDAPBrowser extends VLayout {

	private static final String EMAIL = "email";

	private static final String USERNAME = "username";

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private VLayout ldapusers = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid users;

	private InfoPanel infoPanel;

	private ButtonItem searchButton;

	private GUILDAPServer server;

	public LDAPBrowser(GUILDAPServer server) {
		setWidth100();
		this.server = server;
	}

	@Override
	public void onDraw() {
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setAlign(Alignment.LEFT);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setWrapItemTitles(false);

		// Username
		TextItem username = ItemFactory.newTextItem(USERNAME, null);

		searchButton = new ButtonItem();
		searchButton.setTitle(I18N.message("search"));
		searchButton.setAutoFit(true);
		searchButton.setEndRow(true);
		searchButton.setColSpan(2);
		searchButton.addClickHandler((ClickEvent event) -> onSearch());

		form.setItems(username, searchButton);

		search.setMembersMargin(5);
		search.setMembers(form);
		search.setHeight(50);
		search.setShowResizeBar(false);
		search.setWidth100();
		search.setMargin(5);

		ListGridField usernameField = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		usernameField.setCanFilter(true);

		ListGridField nameField = new ListGridField("name", I18N.message("name"), 150);
		usernameField.setCanFilter(true);

		ListGridField emailField = new ListGridField(EMAIL, I18N.message(EMAIL), 150);
		emailField.setCanFilter(true);

		ListGridField dnField = new ListGridField("dn", "DN");
		dnField.setWidth("*");
		dnField.setCanFilter(true);

		users = new ListGrid();
		users.setEmptyMessage(I18N.message("notitemstoshow"));
		users.setWidth100();
		users.setHeight100();
		users.setFields(usernameField, nameField, emailField, dnField);
		users.setSelectionType(SelectionStyle.MULTIPLE);
		users.setShowRecordComponents(true);
		users.setShowRecordComponentsByCell(true);
		users.setCanFreezeFields(true);
		users.setFilterOnKeypress(true);
		users.setAutoFetchData(true);
		users.setShowFilterEditor(true);

		results.addMember(users);

		// Prepare a panel containing a title and the users list
		infoPanel = new InfoPanel("");

		ldapusers.setMembers(search, infoPanel, results);

		addMember(ldapusers);

		users.addCellContextClickHandler((CellContextClickEvent event) -> {
			showContextMenu();
			event.cancel();
		});
	}

	public SelectItem[] getEventTypes() {
		List<SelectItem> items = new ArrayList<>();

		return items.toArray(new SelectItem[0]);
	}

	@SuppressWarnings("unchecked")
	private void onSearch() {
		users.setData();

		final Map<String, Object> values = vm.getValues();

		if (Boolean.TRUE.equals(vm.validate())) {
			String username = (String) values.get(USERNAME);

			if (username == null || "".equals(username.trim()))
				username = null;
			else
				username = "*" + username + "*";

			searchButton.setDisabled(true);

			LD.contactingServer();
			LDAPService.Instance.get().listUsers(username, server.getId(), new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					searchButton.setDisabled(false);
					super.onFailure(caught);
				}

				@Override
				public void onSuccess(List<GUIUser> result) {
					searchButton.setDisabled(false);
					LD.clearPrompt();
					List<ListGridRecord> records = new ArrayList<>();
					for (GUIUser user : result) {
						ListGridRecord rec = new ListGridRecord();
						rec.setAttribute("name", user.getFullName());
						rec.setAttribute("dn", user.getAddress());
						rec.setAttribute(USERNAME, user.getUsername());
						rec.setAttribute(EMAIL, user.getEmail());
						records.add(rec);
					}
					users.setData(records.toArray(new ListGridRecord[0]));
					infoPanel.setMessage(I18N.message("showelements", Integer.toString(users.getTotalRows())));
				}
			});
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		ListGridRecord[] selection = users.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		List<String> usernames = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			usernames.add(selection[i].getAttributeAsString(USERNAME));

		MenuItem importItem = new MenuItem();
		importItem.setTitle(I18N.message("iimport"));
		importItem.addClickHandler(click -> {
			LD.contactingServer();
			users.deselectAllRecords();
			LDAPService.Instance.get().importUsers(usernames, server.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(List<GUIValue> report) {
					LD.clearPrompt();
					String message = I18N.message("importreport", report.get(0).getValue(), report.get(1).getValue(),
							report.get(2).getValue());
					if ("0".equals(report.get(2).getValue()))
						GuiLog.info(I18N.message("importcompleted"), message);
					else
						GuiLog.error(I18N.message("importerrors"), message, null);
					SC.warn(message);
				}
			});
		});

		contextMenu.setItems(importItem);
		contextMenu.showContextMenu();
	}

	public void setServer(GUILDAPServer server) {
		this.server = server;
	}
}