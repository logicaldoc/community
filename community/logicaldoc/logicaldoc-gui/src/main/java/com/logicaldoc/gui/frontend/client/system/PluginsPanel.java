package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays a list of plugins available for the application.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class PluginsPanel extends VLayout {

	public PluginsPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ListGridField name = new ListGridField("name", I18N.message("name"), 250);
		name.setCanEdit(false);

		ListGridField version = new VersionListGridField();
		version.setCanEdit(false);

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setShowFilterEditor(false);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(name, version);

		addMember(list);

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
}
