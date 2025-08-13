package com.logicaldoc.gui.frontend.client.google.drive;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.TypeIconGridField;
import com.logicaldoc.gui.common.client.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.google.GoogleAsyncCallback;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
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
public class DriveImport extends Window {

	private static final String FILENAME = "filename";

	private static final String RESOURCE_ID = "resourceId";

	private VLayout layout = null;

	public DriveImport() {
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

		addCloseClickHandler(close -> destroy());

		ListGridField resourceId = new ListGridField(RESOURCE_ID, "id", 200);
		resourceId.setHidden(true);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setCanFilter(true);
		fileName.setWidth("*");

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setCanFilter(false);
		size.setHidden(true);

		ListGridField icon = new TypeIconGridField();
		icon.setHidden(true);

		ListGridField version = new VersionListGridField();
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
				LD.contactingServer();
				GoogleService.Instance.get().search(this.getValueAsString(), new GoogleAsyncCallback<>() {
					@Override
					public void onSuccess(List<GUIDocument> hits) {
						LD.clearPrompt();
						List<ListGridRecord> records = new ArrayList<>();
						for (GUIDocument hit : hits) {
							ListGridRecord rec = new ListGridRecord();
							rec.setAttribute(RESOURCE_ID, hit.getExtResId());
							rec.setAttribute("icon", hit.getIcon());
							rec.setAttribute(FILENAME, hit.getFileName());
							rec.setAttribute("version", hit.getVersion());
							rec.setAttribute("size", hit.getFileSize());
							rec.setAttribute("editor", hit.getPublisher());
							rec.setAttribute("lastModified", hit.getLastModified());
							records.add(rec);
						}
						grid.setData(records.toArray(new ListGridRecord[0]));
					}
				});
			}
		});
		toolStrip.addSeparator();

		ToolStripButton importSelection = new ToolStripButton();
		importSelection.setTitle(I18N.message("iimport"));
		toolStrip.addButton(importSelection);
		importSelection.addClickHandler(click -> {
			ListGridRecord[] selection = grid.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;

			List<String> resIds = new ArrayList<>();
			for (int i = 0; i < selection.length; i++)
				resIds.add(selection[i].getAttributeAsString(RESOURCE_ID));

			LD.contactingServer();
			GoogleService.Instance.get().importDocuments(resIds, FolderController.get().getCurrentFolder().getId(),
					null, new GoogleAsyncCallback<>() {
						@Override
						public void onSuccess(Void ret) {
							LD.clearPrompt();
							DocumentsPanel.get().refresh();
						}
					});
		});
		toolStrip.addFill();

		layout.setMembers(toolStrip, grid);
		addItem(layout);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof DriveImport)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}