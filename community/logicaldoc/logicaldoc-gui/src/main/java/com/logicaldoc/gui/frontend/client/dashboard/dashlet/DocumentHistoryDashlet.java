package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Dashlet specialized in listing document's history records
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentHistoryDashlet extends Dashlet {

	private DocumentHistoryDS dataSource;

	protected RefreshableListGrid list;

	protected String event;

	public DocumentHistoryDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);
		if (guiDashlet.getName().equals("checkout"))
			event = Constants.EVENT_CHECKEDOUT;
		else if (guiDashlet.getName().equals("checkin"))
			event = Constants.EVENT_CHECKEDIN;
		else if (guiDashlet.getName().equals("locked"))
			event = Constants.EVENT_LOCKED;
		else if (guiDashlet.getName().equals("download"))
			event = Constants.EVENT_DOWNLOADED;
		else if (guiDashlet.getName().equals("change"))
			event = Constants.EVENT_CHANGED;
		init();
	}

	private void init() {
		ListGridField version = new ListGridField("version", I18N.message("version"), 70);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		list = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (record.getAttributeAsBoolean("new")) {
					return "font-weight: bold;";
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}
		};

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (event != null)
					event.cancel();
				Record record = event.getRecord();
				DocumentService.Instance.get().getById(Long.parseLong(record.getAttributeAsString("docId")),
						new AsyncCallback<GUIDocument>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument document) {
								Menu contextMenu = prepareContextMenu(document);
								contextMenu.showContextMenu();
							}
						});
			}
		});

		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(false);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.NONE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(getDataSource());
		list.setFields(icon, fileName, version, date);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("docId")));
			}
		});

		HeaderControl markAsRead = new HeaderControl(HeaderControl.TRASH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent e) {
				ContactingServer.get().show();
				DocumentService.Instance.get().markHistoryAsRead(event, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						ContactingServer.get().hide();
						RecordList l = list.getRecordList();
						for (int i = 0; i < list.getTotalRows(); i++) {
							l.get(i).setAttribute("new", false);
						}
						list.redraw();
						setTitle(I18N.message(guiDashlet.getTitle(), Integer.toString(list.getTotalRows())));
					}
				});
			}
		});
		
		setHeaderControls(HeaderControls.HEADER_LABEL, markAsRead, refreshControl, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);

		// Count the total of events and the total of unchecked events
		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent e) {
				Record[] records = list.getRecordList().toArray();
				int unread = 0;
				for (Record record : records) {
					if (record.getAttributeAsBoolean("new"))
						unread++;
				}

				int total = list.getTotalRows();
				String title = I18N.message(event + "docs", Integer.toString(total));
				if (unread > 0)
					title = "<b>" + title + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + I18N.message("newitems")
							+ ": " + unread + "</b>";

				String icn = "file";
				if (event.equals(Constants.EVENT_CHECKEDOUT))
					icn = "edit";
				else if (event.equals(Constants.EVENT_LOCKED))
					icn = "lock-alt";
				else if (event.equals(Constants.EVENT_DOWNLOADED))
					icn = "download";
				else if (event.equals(Constants.EVENT_CHECKEDIN))
					icn = "check-square";
				else if (event.equals(Constants.EVENT_CHANGED))
					icn = "edit";

				setTitle(AwesomeFactory.getIconHtml(icn, title));

				if (Constants.EVENT_LOCKED.equals(event))
					Session.get().getUser().setLockedDocs(total);
				else if (Constants.EVENT_CHECKEDOUT.equals(event))
					Session.get().getUser().setCheckedOutDocs(total);
			}
		});

		addItem(list);
	}

	private DataSource getDataSource() {
		return new DocumentHistoryDS(getDataSourceUrl());
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}

	@Override
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = super.prepareContextMenu(document);
		if (event.equals(Constants.EVENT_CHECKEDOUT) || event.equals(Constants.EVENT_LOCKED)) {
			MenuItem unlock = new MenuItem();
			unlock.setTitle(I18N.message("unlock"));
			unlock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
					DocumentService.Instance.get().unlock(new long[] { document.getId() }, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							Session.get().getUser().setLockedDocs(Session.get().getUser().getLockedDocs() - 1);
							list.removeSelectedData();
							list.refresh(getDataSource());
						}
					});
				}
			});
			contextMenu.addItem(unlock);
		}
		return contextMenu;
	}

	@Override
	protected void refresh() {
		list.refresh(getDataSource());
	}
}