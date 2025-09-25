package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Panel that shows the histories of a specific workflow instance
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class WorkflowHistoriesPanel extends VLayout {

	private static final String DOCUMENT_ID = "documentId";

	private RefreshableListGrid historiesGrid = new RefreshableListGrid();

	private Long wfInstanceId;

	private Long wfTemplateId;

	private boolean showComment = true;

	public WorkflowHistoriesPanel(Long wfInstanceId, Long wfTemplateId, boolean showComment) {
		this.wfInstanceId = wfInstanceId;
		this.wfTemplateId = wfTemplateId;
		this.showComment = showComment;
	}

	public ListGrid getHistoriesGrid() {
		return historiesGrid;
	}

	public Long getWfTemplateId() {
		return wfTemplateId;
	}

	public void setWfTemplateId(Long wfTemplateId) {
		this.wfTemplateId = wfTemplateId;
	}

	public Long getWfInstanceId() {
		return wfInstanceId;
	}

	public void setWfInstanceId(Long wfInstanceId) {
		this.wfInstanceId = wfInstanceId;
	}

	@Override
	public void onDraw() {
		ListGridField historyId = new IdListGridField();

		ListGridField historyEvent = new ListGridField("event", I18N.message("event"), 200);
		ListGridField historyName = new WorkflowTaskNameListGridField("name", "display", I18N.message("task"));

		ListGridField historyDate = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ListGridField historyUser = new UserListGridField("user", "userId", "user");
		ListGridField historyComment = new ListGridField("comment", I18N.message("comment"));
		historyComment.setMinWidth(200);
		historyComment.setWidth("*");
		historyComment.setHidden(!showComment);
		FileNameListGridField historyFilename = new FileNameListGridField();
		ListGridField documentId = new ListGridField(DOCUMENT_ID, I18N.message("docid"), 80);
		documentId.setHidden(true);
		ListGridField historySid = new ListGridField("sessionid", I18N.message("sid"), 240);
		historySid.setHidden(true);
		ListGridField key = new ListGridField("key", I18N.message("key"), 90);
		key.setHidden(true);
		ListGridField transition = new ListGridField("transition", I18N.message("transition"), 120);
		transition.setHidden(true);

		ListGridField templateId = new ListGridField("templateId", I18N.message("templateid"), 70);
		templateId.setHidden(true);
		ListGridField templateVersion = new VersionListGridField("templateVersion", "version");
		templateVersion.setHidden(true);

		historiesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		historiesGrid.setCanFreezeFields(true);
		historiesGrid.setAutoFetchData(true);
		historiesGrid.setShowHeader(true);
		historiesGrid.setCanSelectAll(false);
		historiesGrid.setSelectionType(SelectionStyle.SINGLE);
		historiesGrid.setWidth100();
		historiesGrid.sort("date", SortDirection.ASCENDING);
		historiesGrid.setBorder("1px solid #E1E1E1");

		if (wfInstanceId != null && wfTemplateId != null)
			historiesGrid.setDataSource(new WorkflowHistoriesDS(wfInstanceId, wfTemplateId, null, null, null));
		if (Menu.enabled(Menu.SESSIONS))
			historiesGrid.setFields(historyId, templateVersion, templateId, historyEvent, historyName, historyDate,
					historyUser, historyComment, historyFilename, transition, documentId, historySid, key);
		else
			historiesGrid.setFields(historyId, templateVersion, templateId, historyEvent, historyName, historyDate,
					historyUser, historyComment, historyFilename, transition, documentId, key);
		historiesGrid.addCellContextClickHandler(event -> {
			event.cancel();
			showHistoryContextMenu();
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(event -> GridUtil.exportCSV(historiesGrid, true));

		Button print = new Button(I18N.message("print"));
		print.setAutoFit(true);
		buttons.addMember(print);
		print.addClickHandler(event -> GridUtil.print(historiesGrid));

		setMembers(historiesGrid, buttons);
	}

	public void refresh() {
		if (historiesGrid.getDataSource() != null) {
			historiesGrid.getDataSource().destroy();
			historiesGrid.invalidateCache();
		}

		if (wfInstanceId != null && wfTemplateId != null) {
			historiesGrid.refresh(new WorkflowHistoriesDS(wfInstanceId, wfTemplateId, null, null, null));
			historiesGrid.fetchData();
		}
	}

	private void showHistoryContextMenu() {
		final ListGridRecord selection = historiesGrid.getSelectedRecord();
		if (selection.getAttributeAsString(DOCUMENT_ID) == null
				|| selection.getAttributeAsString(DOCUMENT_ID).isEmpty())
			return;

		DocumentService.Instance.get().getById(Long.parseLong(selection.getAttributeAsString(DOCUMENT_ID)),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(final GUIDocument doc) {
						if (doc == null)
							return;

						final com.smartgwt.client.widgets.menu.Menu contextMenu = new com.smartgwt.client.widgets.menu.Menu();

						final MenuItem preview = new MenuItem();
						preview.setTitle(I18N.message("preview"));
						preview.setEnabled(com.logicaldoc.gui.common.client.Menu
								.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
						preview.addClickHandler(event -> new PreviewPopup(doc).show());

						final MenuItem download = new MenuItem();
						download.setTitle(I18N.message("download"));
						download.setEnabled(doc.getFolder().isDownload());
						download.addClickHandler(event -> {
							if (doc.getFolder().isDownload())
								DocUtil.download(doc.getId(), null);
						});

						final MenuItem open = new MenuItem();
						open.setTitle(I18N.message("openinfolder"));
						open.addClickHandler(event -> {
							destroy();
							DocumentsPanel.get().openInFolder(doc.getFolder().getId(), doc.getId());
						});

						contextMenu.setItems(preview, download, open);
						contextMenu.showContextMenu();
					}
				});
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