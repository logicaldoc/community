package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.LdapService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
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
public class LdapBrowser extends VLayout {

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private VLayout ldapusers = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid users;

	private InfoPanel infoPanel;

	private ButtonItem searchButton;

	public LdapBrowser() {
		setWidth100();
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

		search.setMembersMargin(10);
		search.setMembers(form);
		search.setHeight("20%");
		search.setShowResizeBar(true);
		search.setWidth100();
		search.setMargin(10);

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

	/**
	 * Gets the option items for Messageshistory events types
	 */
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

			ContactingServer.get().show();
			LdapService.Instance.get().listUsers(username, new AsyncCallback<GUIUser[]>() {

				@Override
				public void onFailure(Throwable caught) {
					searchButton.setDisabled(false);
					ContactingServer.get().hide();
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIUser[] result) {
					searchButton.setDisabled(false);
					ContactingServer.get().hide();
					if (result != null && result.length > 0) {
						ListGridRecord[] records = new ListGridRecord[result.length];
						for (int i = 0; i < result.length; i++) {
							ListGridRecord record = new ListGridRecord();
							record.setAttribute("name", result[i].getFullName());
							record.setAttribute("dn", result[i].getAddress());
							record.setAttribute("username", result[i].getUserName());
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
		for (int i = 0; i < selection.length; i++) {
			usernames[i] = selection[i].getAttributeAsString("username");
		}

		MenuItem _import = new MenuItem();
		_import.setTitle(I18N.message("iimport"));
		_import.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LdapImportDialog dialog = new LdapImportDialog(usernames);
				dialog.show();
				users.deselectAllRecords();
			}
		});

		contextMenu.setItems(_import);
		contextMenu.showContextMenu();
	}
}