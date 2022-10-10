package com.logicaldoc.gui.frontend.client.impex.converters;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.ExtensionAliasesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;

/**
 * This is the dialog used to quickly associate a converter to formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ExtensionAliasesDialog extends Window {

	private ListGrid extensionsGrid;

	public ExtensionAliasesDialog() {

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("extensionaliases"));
		setWidth(320);
		setHeight(360);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		refresh();
	}

	private void refresh() {
		ListGridField extension = new ListGridField("extension", I18N.message("extension"), 60);
		extension.setCanEdit(false);
		ListGridField aliases = new ListGridField("aliases", I18N.message("aliases"));
		aliases.setCanEdit(true);
		aliases.setWidth("*");

		extensionsGrid = new ListGrid();
		extensionsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		extensionsGrid.setShowFilterEditor(true);
		extensionsGrid.setFilterOnKeypress(true);
		extensionsGrid.setAutoFetchData(true);
		extensionsGrid.setEditByCell(true);
		extensionsGrid.setSelectionType(SelectionStyle.SIMPLE);
		extensionsGrid.setWidth100();
		extensionsGrid.setHeight("*");
		extensionsGrid.setDataSource(new ExtensionAliasesDS());
		extensionsGrid.setFields(extension, aliases);

		extensionsGrid.addEditCompleteHandler(new EditCompleteHandler() {
			@Override
			public void onEditComplete(EditCompleteEvent event) {
				ListGridRecord record = extensionsGrid.getRecord(event.getRowNum());

				String extension = record.getAttributeAsString("extension");
				String aliases = (String) event.getNewValues().get("aliases");
				aliases = aliases.trim().toLowerCase().replace(" ", "");

				SettingService.Instance.get().saveExtensionAliases(extension, aliases, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						GuiLog.info(I18N.message("settingssaved"), null);
					}
				});
			}
		});

		addItem(extensionsGrid);
	}
}