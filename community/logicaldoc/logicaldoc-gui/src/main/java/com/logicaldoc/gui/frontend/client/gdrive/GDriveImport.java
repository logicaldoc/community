package com.logicaldoc.gui.frontend.client.gdrive;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.GDriveService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show documents in Google Drive that can be
 * imported into LogicalDOC
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class GDriveImport extends Window {

	private VLayout layout = null;

	public GDriveImport() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("importfromgdrive"));

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth(600);
		setHeight(350);
		centerInPage();

		layout = new VLayout();
		layout.setMargin(2);
		layout.setWidth100();
		layout.setHeight100();

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		ListGridField resourceId = new ListGridField("resourceId", "id", 200);
		resourceId.setHidden(true);

		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		fileName.setCanFilter(true);
		fileName.setWidth("*");

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.RIGHT);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);
		size.setHidden(true);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setCanFilter(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 90);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);
		version.setHidden(true);

		ListGridField editor = new ListGridField("editor", I18N.message("editedby"), 90);
		editor.setAlign(Alignment.CENTER);
		editor.setCanFilter(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");

		final ListGrid grid = new ListGrid();
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);
		grid.setCanFreezeFields(true);
		grid.setAutoFetchData(true);
		grid.setFilterOnKeypress(true);
		grid.setWrapCells(false);
		grid.setSelectionType(SelectionStyle.MULTIPLE);
		grid.setFields(resourceId, icon, fileName, size, version, editor, lastModified);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.setAlign(Alignment.LEFT);

		toolStrip.addFormItem(new SearchBox() {
			@Override
			protected void onSearch() {
				ContactingServer.get().show();
				GDriveService.Instance.get().search(this.getValueAsString(), new AsyncCallback<GUIDocument[]>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument[] hits) {
						ContactingServer.get().hide();
						ListGridRecord[] records = new ListGridRecord[hits.length];
						int i = 0;
						for (GUIDocument hit : hits) {
							ListGridRecord record = new ListGridRecord();
							record.setAttribute("resourceId", hit.getExtResId());
							record.setAttribute("icon", hit.getIcon());
							record.setAttribute("filename", hit.getFileName());
							record.setAttribute("version", hit.getVersion());
							record.setAttribute("size", hit.getFileSize());
							record.setAttribute("editor", hit.getPublisher());
							record.setAttribute("lastModified", hit.getLastModified());
							records[i++] = record;
						}
						grid.setData(records);
					}
				});
			}
		});
		toolStrip.addSeparator();

		ToolStripButton importSelection = new ToolStripButton();
		importSelection.setTitle(I18N.message("iimport"));
		toolStrip.addButton(importSelection);
		importSelection.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord[] selection = grid.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;

				String[] resIds = new String[selection.length];
				for (int i = 0; i < resIds.length; i++)
					resIds[i] = selection[i].getAttributeAsString("resourceId");

				ContactingServer.get().show();
				GDriveService.Instance.get().importDocuments(resIds, Session.get().getCurrentFolder().getId(), null,
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								ContactingServer.get().hide();
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								ContactingServer.get().hide();
								DocumentsPanel.get().refresh();
							}
						});
			}
		});
		toolStrip.addFill();

		layout.setMembers(toolStrip, grid);
		addItem(layout);
	}
}