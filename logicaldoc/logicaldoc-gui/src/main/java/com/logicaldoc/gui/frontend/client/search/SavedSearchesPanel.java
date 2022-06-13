package com.logicaldoc.gui.frontend.client.search;

import java.util.Arrays;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.data.SavedSearchesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.ValuesCallback;
import com.logicaldoc.gui.common.client.widgets.GroupSelectorCombo;
import com.logicaldoc.gui.common.client.widgets.UserSelectorCombo;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the saved searches of the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SavedSearchesPanel extends VLayout {

	private ListGrid list;

	private static SavedSearchesPanel instance;

	public static SavedSearchesPanel get() {
		if (instance == null)
			instance = new SavedSearchesPanel();
		return instance;
	}

	private SavedSearchesPanel() {
	}

	@Override
	public void onDraw() {
		ListGridField name = new ListGridField("name", I18N.message("name"), 100);
		ListGridField type = new ListGridField("type", I18N.message("type"), 70);
		ListGridField description = new ListGridField("description", I18N.message("description"));

		list = new ListGrid();
		list.setWidth100();
		list.setHeight100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new SavedSearchesDS());
		list.setFields(name, type, description);
		addMember(list);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				ListGridRecord record = event.getRecord();
				SearchService.Instance.get().load(record.getAttributeAsString("name"),
						new AsyncCallback<GUISearchOptions>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUISearchOptions options) {
								Search.get().setOptions(options);
								Search.get().search();
							}
						});
			}
		});

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem execute = new MenuItem();
		execute.setTitle(I18N.message("execute"));
		execute.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = list.getSelectedRecord();
				SearchService.Instance.get().load(selection.getAttributeAsString("name"),
						new AsyncCallback<GUISearchOptions>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUISearchOptions options) {
								Search.get().setOptions(options);
								Search.get().search();
							}
						});
			}
		});

		MenuItem share = new MenuItem();
		share.setTitle(I18N.message("sharesearch"));
		share.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = list.getSelectedRecord();

				final UserSelectorCombo usersSelector = new UserSelectorCombo("users", "users", null, true, true);
				
				final GroupSelectorCombo groupsSelector = new GroupSelectorCombo("groups", "groups");
				
				LD.askForValues("sharesearch", null, Arrays.asList(new FormItem[] { usersSelector, groupsSelector }),
						350, new ValuesCallback() {
							@Override
							public void execute(String value) {

							}

							@Override
							public void execute(Map<String, Object> values) {
								LD.contactingServer();
								SearchService.Instance.get().shareSearch(selection.getAttributeAsString("name"),
										usersSelector.getUserIds(), groupsSelector.getGroupIds(), new AsyncCallback<Void>() {

											@Override
											public void onFailure(Throwable caught) {
												LD.clearPrompt();
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg0) {
												LD.clearPrompt();
											}
										});
							}
						});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				final String[] names = new String[selection.length];
				for (int i = 0; i < selection.length; i++) {
					names[i] = selection[i].getAttributeAsString("name");
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SearchService.Instance.get().delete(names, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
								}
							});
						}
					}
				});
			}
		});

		if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.SHARE_SEARCH))
			contextMenu.setItems(execute, share, delete);
		else
			contextMenu.setItems(execute, delete);
		contextMenu.showContextMenu();
	}

	public void addEntry(String name, String description, String type) {
		// Incredible!!! Without this line we have a duplicated save search
		// entry when the user saves the first search.
		System.out.println("");
		ListGridRecord record = new ListGridRecord();
		record.setAttribute("name", name);
		record.setAttribute("description", description);
		record.setAttribute("type", type);
		list.addData(record);
	}
}