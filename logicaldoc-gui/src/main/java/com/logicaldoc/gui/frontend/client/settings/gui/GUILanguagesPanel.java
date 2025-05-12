package com.logicaldoc.gui.frontend.client.settings.gui;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.data.LanguagesDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Displays a list of languages available for the GUI, allowing for
 * enable/disable single languages.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUILanguagesPanel extends VLayout {

	private static final String EENABLED = "eenabled";

	private ListGrid list;

	public GUILanguagesPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ListGridField enabled = new EnabledListGridField();

		ListGridField code = new ListGridField("code", I18N.message("code"), 80);
		code.setCanEdit(false);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanEdit(false);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setDataSource(new LanguagesDS(true));
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(enabled, code, name);

		addMember(list);

		if (Feature.enabled(Feature.GUI_LANGUAGES))
			list.addCellContextClickHandler(event -> {
				showContextMenu();
				event.cancel();
			});
	}

	private void showContextMenu() {
		final ListGridRecord rec = list.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(click -> SystemService.Instance.get()
				.setGUILanguageStatus(rec.getAttributeAsString("code"), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, true);
						list.refreshRow(list.getRecordIndex(rec));
						GuiLog.info(I18N.message("settingsaffectnewsessions"), null);
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(click -> SystemService.Instance.get()
				.setGUILanguageStatus(rec.getAttributeAsString("code"), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, false);
						list.refreshRow(list.getRecordIndex(rec));
						GuiLog.info(I18N.message("settingsaffectnewsessions"), null);
					}
				}));

		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean(EENABLED)))
			contextMenu.setItems(disable);
		else
			contextMenu.setItems(enable);
		contextMenu.showContextMenu();
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
