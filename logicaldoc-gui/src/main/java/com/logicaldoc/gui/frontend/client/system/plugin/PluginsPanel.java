package com.logicaldoc.gui.frontend.client.system.plugin;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Displays a list of plugins available for the application.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class PluginsPanel extends VLayout {

	private ListGrid list;

	public PluginsPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton install = new ToolStripButton(I18N.message("install"));
		install.addClickHandler(event -> {
			new PluginUploader(PluginsPanel.this).show();
			event.cancel();
		});

		toolStrip.addButton(install);
		toolStrip.addFill();

		ListGridField name = new ListGridField("name", I18N.message("name"), 250);
		name.setCanEdit(false);
		name.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value.toString().startsWith("logicaldoc-"))
				return value.toString().substring(value.toString().indexOf('-') + 1);
			else
				return value.toString();
		});

		ListGridField version = new VersionListGridField();
		version.setCanEdit(false);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setShowFilterEditor(false);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(name, version);

		addMember(toolStrip);
		addMember(list);

		if (Session.get().isAdmin() && Session.get().isDefaultTenant())
			list.addCellContextClickHandler(event -> {
				showContextMenu();
				event.cancel();
			});

		refresh();
	}

	void refresh() {
		SystemService.Instance.get().getPlugins(new AsyncCallback<GUIValue[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIValue[] plugins) {
				ListGridRecord[] records = new ListGridRecord[plugins.length];
				for (int i = 0; i < plugins.length; i++) {
					records[i] = new ListGridRecord();
					records[i].setAttribute("name", plugins[i].getCode());
					records[i].setAttribute("version", plugins[i].getValue());
				}
				list.setRecords(records);
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem initialize = new MenuItem();
		initialize.setTitle(I18N.message("initialize"));
		initialize.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirminitializeplugin"), value -> {
					if (Boolean.TRUE.equals(value)) {
						LD.contactingServer();
						SystemService.Instance.get().initializePlugin(
								list.getSelectedRecord().getAttributeAsString("name"), new AsyncCallback<Void>() {
									@Override
									public void onFailure(Throwable caught) {
										LD.clearPrompt();
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void result) {
										LD.clearPrompt();
										GuiLog.info(I18N.message("plugininitialized"), null);
									}
								});
					}
				}));

		MenuItem uninstall = new MenuItem();
		uninstall.setTitle(I18N.message("uninstall"));
		uninstall.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmuninstallplugin"), value -> {
					if (Boolean.TRUE.equals(value)) {
						LD.contactingServer();
						SystemService.Instance.get().uninstallPlugin(
								list.getSelectedRecord().getAttributeAsString("name"), new AsyncCallback<Void>() {
									@Override
									public void onFailure(Throwable caught) {
										LD.clearPrompt();
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void result) {
										LD.clearPrompt();
										GuiLog.info(I18N.message("pluginuninstalled"), null);
									}
								});
					}
				}));

		contextMenu.setItems(initialize, uninstall);
		contextMenu.showContextMenu();
	}
}