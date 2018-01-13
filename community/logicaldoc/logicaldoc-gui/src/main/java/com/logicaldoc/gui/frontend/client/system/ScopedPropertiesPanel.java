package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.PropertiesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.ClusterService;
import com.logicaldoc.gui.frontend.client.services.ClusterServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
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
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of configuration parameters.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.5
 */
public class ScopedPropertiesPanel extends VLayout {

	private ListGrid list;

	private Layout listing = new VLayout();

	public ScopedPropertiesPanel() {
		setWidth100();

		listing.setAlign(Alignment.CENTER);
		listing.setHeight100();
		initListGrid();

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
				initListGrid();
			}
		});
		toolStrip.addFill();

		setMembers(toolStrip, listing);
	}

	private void initListGrid() {
		if (list != null) {
			listing.removeMember(list);
			list.destroy();
		}

		ListGridField name = new ListGridField("name", I18N.message("setting"), 210);
		name.setCanFilter(true);
		name.setCanSort(true);

		ListGridField value = new ListGridField("value", I18N.message("value"), 280);
		value.setCanFilter(true);
		value.setCanSort(true);

		ListGridField scope = new ListGridField("scope", I18N.message("scope"), 80);
		scope.setCanFilter(true);
		scope.setCanSort(true);
		scope.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return I18N.message(value.toString());
			}
		});

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanExpandRecords(false);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new PropertiesDS());
		list.setFields(scope, name, value);

		listing.addMember(list);

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

		ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final String[] ids = new String[selection.length];
		for (int i = 0; i < selection.length; i++) {
			ids[i] = selection[i].getAttribute("name");
		}

		MenuItem makeglobal = new MenuItem();
		makeglobal.setTitle(I18N.message("makeglobal"));
		makeglobal.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmmakeglobal"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ClusterService.Instance.get().makeGlobal(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									ListGridRecord[] selection = list.getSelectedRecords();
									for (int i = 0; i < ids.length; i++) {
										selection[i].setAttribute("scope", "global");
										list.refreshRow(list.getRecordIndex(selection[i]));
									}
								}
							});
						}
					}
				});
			}
		});

		MenuItem makelocal = new MenuItem();
		makelocal.setTitle(I18N.message("makelocal"));
		makelocal.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmmakelocal"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ClusterService.Instance.get().makeLocal(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									ListGridRecord[] selection = list.getSelectedRecords();
									for (int i = 0; i < ids.length; i++) {
										selection[i].setAttribute("scope", "local");
										list.refreshRow(list.getRecordIndex(selection[i]));
									}
								}
							});
						}
					}
				});
			}
		});

		contextMenu.setItems(makeglobal, makelocal);
		contextMenu.showContextMenu();
	}
}