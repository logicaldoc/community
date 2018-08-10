package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.PreviewPopup;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Panel that shows the histories of a specific workflow instance
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class WorkflowHistoriesPanel extends VLayout {

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
		ListGridField historyId = new ListGridField("id", I18N.message("id"), 60);
		historyId.setHidden(true);

		ListGridField historyEvent = new ListGridField("event", I18N.message("event"), 200);
		ListGridField historyName = new ListGridField("name", I18N.message("task"), 200);
		historyName.setHidden(true);

		ListGridField historyDate = new ListGridField("date", I18N.message("date"), 120);
		historyDate.setAlign(Alignment.CENTER);
		historyDate.setType(ListGridFieldType.DATE);
		historyDate.setCellFormatter(new DateCellFormatter(false));
		historyDate.setCanFilter(false);
		ListGridField historyUser = new ListGridField("user", I18N.message("user"), 120);
		ListGridField historyComment = new ListGridField("comment", I18N.message("comment"));
		historyComment.setWidth("*");
		historyComment.setHidden(!showComment);
		ListGridField historyFilename = new ListGridField("filename", I18N.message("document"), 180);
		ListGridField documentId = new ListGridField("documentId", I18N.message("docid"), 80);
		documentId.setHidden(true);
		ListGridField historySid = new ListGridField("sessionid", I18N.message("sid"), 240);
		historySid.setHidden(true);
		ListGridField transition = new ListGridField("transition", I18N.message("transition"), 120);
		transition.setHidden(true);

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
			historiesGrid.setDataSource(new WorkflowHistoriesDS(wfInstanceId, wfTemplateId, null, null));
		if (Menu.enabled(Menu.SESSIONS))
			historiesGrid.setFields(historyId, historyEvent, historyName, historyDate, historyUser, historyComment,
					historyFilename, transition, documentId, historySid);
		else
			historiesGrid.setFields(historyId, historyEvent, historyName, historyDate, historyUser, historyComment,
					historyFilename, transition, documentId);
		historiesGrid.addCellContextClickHandler(new CellContextClickHandler() {

			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				event.cancel();
				showHistoryContextMenu();
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(historiesGrid, true);
			}
		});

		Button print = new Button(I18N.message("print"));
		print.setAutoFit(true);
		buttons.addMember(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { historiesGrid });
			}
		});

		setMembers(historiesGrid, buttons);
	}

	public void refresh() {
		if (historiesGrid.getDataSource() != null) {
			historiesGrid.getDataSource().destroy();
			historiesGrid.invalidateCache();
		}

		if (wfInstanceId != null && wfTemplateId != null) {
			historiesGrid.refresh(new WorkflowHistoriesDS(wfInstanceId, wfTemplateId, null, null));
			historiesGrid.fetchData();
		}
	}

	private void showHistoryContextMenu() {
		final ListGridRecord selection = historiesGrid.getSelectedRecord();
		if (selection.getAttributeAsString("documentId") == null
				|| selection.getAttributeAsString("documentId").isEmpty())
			return;

		DocumentService.Instance.get().getById(Long.parseLong(selection.getAttributeAsString("documentId")),
				new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(final GUIDocument doc) {
						if (doc == null)
							return;

						final com.smartgwt.client.widgets.menu.Menu contextMenu = new com.smartgwt.client.widgets.menu.Menu();

						final MenuItem preview = new MenuItem();
						preview.setTitle(I18N.message("preview"));
						preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
							public void onClick(MenuItemClickEvent event) {
								PreviewPopup iv = new PreviewPopup(doc);
								iv.show();
							}
						});

						final MenuItem download = new MenuItem();
						download.setTitle(I18N.message("download"));
						download.setEnabled(doc.getFolder().isDownload());
						download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
							public void onClick(MenuItemClickEvent event) {
								if (doc.getFolder().isDownload())
									DocUtil.download(doc.getId(), null);
							}
						});

						final MenuItem open = new MenuItem();
						open.setTitle(I18N.message("openinfolder"));
						open.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
							public void onClick(MenuItemClickEvent event) {
								destroy();
								DocumentsPanel.get().openInFolder(doc.getFolder().getId(), doc.getId());
							}
						});

						contextMenu.setItems(preview, download, open);
						contextMenu.showContextMenu();
					}
				});
	}
}