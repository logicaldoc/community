package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.HeaderControl.HeaderIcon;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

public class StatusDashlet extends Dashlet {

	private DocumentsDS dataSource;

	protected ListGrid list;

	protected String eventCode;

	public StatusDashlet(int id, final String eventCode) {
		super(id);

		this.eventCode = eventCode;

		String icn = "page_white.png";
		setTitle(I18N.message(eventCode + "docs"));
		if (eventCode.equals(Constants.EVENT_CHECKEDOUT))
			icn = "page_edit.png";
		else if (eventCode.equals(Constants.EVENT_LOCKED))
			icn = "page_white_lock.png";

		HeaderControl refresh = new HeaderControl(HeaderControl.REFRESH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		HeaderIcon portletIcon = ItemFactory.newHeaderIcon(icn);
		HeaderControl hcicon = new HeaderControl(portletIcon);
		hcicon.setSize(16);
		setHeaderControls(hcicon, HeaderControls.HEADER_LABEL, refresh, HeaderControls.MAXIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);

		refresh();
	}

	private void refresh() {
		if (list != null)
			removeItem(list);

		int max = 20;
		int status = Constants.DOC_UNLOCKED;

		if (eventCode.equals(Constants.EVENT_CHECKEDOUT))
			status = Constants.DOC_CHECKED_OUT;
		else if (eventCode.equals(Constants.EVENT_LOCKED))
			status = Constants.DOC_LOCKED;

		ListGridField version = new ListGridField("version", I18N.message("version"), 70);
		ListGridField lastModified = new ListGridField("lastModified", I18N.message("date"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(false);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.NONE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(new DocumentsDS(status, max));
		list.setFields(icon, fileName, version, lastModified);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (event != null)
					event.cancel();
				Record record = event.getRecord();
				DocumentService.Instance.get().getById(Long.parseLong(record.getAttributeAsString("id")),
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

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("id")));
			}
		});

		// Count the total of events and the total of unchecked events
		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				Record[] records = list.getRecordList().toArray();
				int unread = 0;
				for (Record record : records) {
					if (record.getAttributeAsBoolean("new"))
						unread++;
				}

				int total = list.getTotalRows();
				String title = I18N.message(eventCode + "docs", Integer.toString(total));
				if (unread > 0)
					title = "<b>" + title + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + I18N.message("newitems")
							+ ": " + unread + "</b>";
				setTitle(title);

				if (Constants.EVENT_LOCKED.equals(eventCode))
					Session.get().getUser().setLockedDocs(total);
				else if (Constants.EVENT_CHECKEDOUT.equals(eventCode))
					Session.get().getUser().setCheckedOutDocs(total);
			}
		});

		addItem(list);
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
		if (eventCode.equals(Constants.EVENT_CHECKEDOUT) || eventCode.equals(Constants.EVENT_LOCKED)) {
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
							refresh();
						}
					});
				}
			});
			contextMenu.addItem(unlock);
		}
		return contextMenu;
	}
}
