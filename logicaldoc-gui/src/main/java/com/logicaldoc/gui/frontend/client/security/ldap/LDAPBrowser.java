package com.logicaldoc.gui.frontend.client.security.ldap;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
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
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel is used to perform user searches in the LDAP repositories
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class LDAPBrowser extends VLayout {

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
		TextItem username = ItemFactory.newTextItem("username", "username", null);

		searchButton = new ButtonItem();
		searchButton.setTitle(I18N.message("search"));
		searchButton.setAutoFit(true);
		searchButton.setEndRow(true);
		searchButton.setColSpan(2);
		searchButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSearch();
			}
		});

		form.setItems(username, searchButton);

		search.setMembersMargin(5);
		search.setMembers(form);
		search.setHeight(50);
		search.setShowResizeBar(false);
		search.setWidth100();
		search.setMargin(5);

		ListGridField usernameField = new ListGridField("username", I18N.message("username"), 100);
		usernameField.setCanFilter(true);

		ListGridField nameField = new ListGridField("name", I18N.message("name"), 150);
		usernameField.setCanFilter(true);

		ListGridField emailField = new ListGridField("email", I18N.message("email"), 150);
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

		users.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});
	}

	public SelectItem[] getEventTypes() {
		List<SelectItem> items = new ArrayList<SelectItem>();

		return items.toArray(new SelectItem[0]);
	}

	@SuppressWarnings("unchecked")
	private void onSearch() {
		users.setData(new ListGridRecord[0]);

		final Map<String, Object> values = (Map<String, Object>) vm.getValues();

		if (vm.validate()) {
			String username = (String) values.get("username");

			if (username == null || "".equals(username.trim()))
				username = null;
			else
				username = "*" + username + "*";

			searchButton.setDisabled(true);

			LD.contactingServer();
			LDAPService.Instance.get().listUsers(username, server.getId(), new AsyncCallback<GUIUser[]>() {

				@Override
				public void onFailure(Throwable caught) {
					searchButton.setDisabled(false);
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIUser[] result) {
					searchButton.setDisabled(false);
					LD.clearPrompt();
					if (result != null && result.length > 0) {
						ListGridRecord[] records = new ListGridRecord[result.length];
						for (int i = 0; i < result.length; i++) {
							ListGridRecord record = new ListGridRecord();
							record.setAttribute("name", result[i].getFullName());
							record.setAttribute("dn", result[i].getAddress());
							record.setAttribute("username", result[i].getUsername());
							record.setAttribute("email", result[i].getEmail());
							records[i] = record;
						}
						users.setData(records);
					}
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
		final String[] usernames = new String[selection.length];
		for (int i = 0; i < selection.length; i++)
			usernames[i] = selection[i].getAttributeAsString("username");

		MenuItem _import = new MenuItem();
		_import.setTitle(I18N.message("iimport"));
		_import.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.contactingServer();
				users.deselectAllRecords();
				LDAPService.Instance.get().importUsers(usernames, server.getId(), new AsyncCallback<GUIValue[]>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						LD.clearPrompt();
					}

					@Override
					public void onSuccess(GUIValue[] report) {
						LD.clearPrompt();
						String message = I18N.message("importreport",
								new String[] { report[0].getValue(), report[1].getValue(), report[2].getValue() });
						if ("0".equals(report[2].getValue()))
							GuiLog.info(I18N.message("importcompleted"), message);
						else
							GuiLog.error(I18N.message("importerrors"), message, null);
						SC.warn(message);
					}
				});
			}
		});

		contextMenu.setItems(_import);
		contextMenu.showContextMenu();
	}

	public void setServer(GUILDAPServer server) {
		this.server = server;
	}
}