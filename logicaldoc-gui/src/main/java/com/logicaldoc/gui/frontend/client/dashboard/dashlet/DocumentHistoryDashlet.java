package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.HeaderControls;
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

	protected DocumentsListGrid list;

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
		else if ("lastaccessed".equals(guiDashlet.getName()))
			setTitle(AwesomeFactory.getIconHtml("eye", I18N.message(guiDashlet.getTitle())));
		init();
	}

	private void init() {
		list = new DocumentsListGrid(guiDashlet.getExtendedAttributes()) {

			@Override
			protected void prepareFieldsMap() {
				super.prepareFieldsMap();

				ColoredListGridField docid = new ColoredListGridField("docid", "docid");
				docid.setHidden(true);
				fieldsMap.put(docid.getName(), docid);

				DateListGridField date = new DateListGridField("date", "date");
				date.setHidden(true);
				fieldsMap.put(date.getName(), date);

				ListGridField evnt = new ColoredListGridField("event", "event");
				evnt.setHidden(true);
				fieldsMap.put(evnt.getName(), evnt);

				ListGridField path = new ColoredListGridField("path", "path");
				path.setHidden(true);
				fieldsMap.put(path.getName(), path);

				ListGridField reason = new ColoredListGridField("reason", "reason");
				reason.setHidden(true);
				fieldsMap.put(reason.getName(), reason);

				ListGridField user = new UserListGridField("user", "userid", "user",
						Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
				user.setCanFilter(true);
				user.setHidden(true);
				user.setCanSort(true);
				fieldsMap.put(user.getName(), user);

				ListGridField _new = new ListGridField("new", "new");
				_new.setHidden(true);
				fieldsMap.put(_new.getName(), _new);
			}

			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if ("true".equals(record.getAttributeAsString("new")) && event != null) {
					return "font-weight: bold;";
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}
		};

		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setCanGroupBy(false);
		list.setCanReorderFields(false);
		list.setCanFreezeFields(false);
		list.setSelectionType(SelectionStyle.NONE);
		list.setHeight100();
		list.setBorder("0px");

		Map<String, ListGridField> fieldsMap = list.getFieldsMap();
		fieldsMap.get("statusIcons").setHidden(true);

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
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument document) {
								Menu contextMenu = prepareContextMenu(document);
								contextMenu.showContextMenu();
							}
						});
			}
		});

		list.setDataSource(getDataSource());

		final List<ListGridField> fields = new ArrayList<ListGridField>();

		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		for (String col : getColumnsList()) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}

		if (!fields.contains(fieldsMap.get("date"))) {
			fieldsMap.get("date").setHidden(true);
			fields.add(fieldsMap.get("date"));
		}
		if (!fields.contains(fieldsMap.get("event"))) {
			fieldsMap.get("event").setHidden(true);
			fields.add(fieldsMap.get("event"));
		}
		if (!fields.contains(fieldsMap.get("path"))) {
			fieldsMap.get("path").setHidden(true);
			fields.add(fieldsMap.get("path"));
		}
		if (!fields.contains(fieldsMap.get("reason"))) {
			fieldsMap.get("reason").setHidden(true);
			fields.add(fieldsMap.get("reason"));
		}
		if (!fields.contains(fieldsMap.get("filename"))) {
			fieldsMap.get("filename").setHidden(true);
			fields.add(fieldsMap.get("filename"));
		}
		if (!fields.contains(fieldsMap.get("lastModified"))) {
			fieldsMap.get("lastModified").setHidden(true);
			fields.add(fieldsMap.get("lastModified"));
		}
		if (!fields.contains(fieldsMap.get("date"))) {
			fieldsMap.get("date").setHidden(true);
			fields.add(fieldsMap.get("date"));
		}
		if (!fields.contains(fieldsMap.get("type"))) {
			fieldsMap.get("type").setHidden(true);
			fields.add(fieldsMap.get("type"));
		}
		if (!fields.contains(fieldsMap.get("size"))) {
			fieldsMap.get("size").setHidden(true);
			fields.add(fieldsMap.get("size"));
		}
		if (!fields.contains(fieldsMap.get("pages"))) {
			fieldsMap.get("pages").setHidden(true);
			fields.add(fieldsMap.get("pages"));
		}
		if (!fields.contains(fieldsMap.get("fileVersion"))) {
			fieldsMap.get("fileVersion").setHidden(true);
			fields.add(fieldsMap.get("fileVersion"));
		}
		if (!fields.contains(fieldsMap.get("version"))) {
			fieldsMap.get("version").setHidden(true);
			fields.add(fieldsMap.get("version"));
		}
		if (!fields.contains(fieldsMap.get("publisher"))) {
			fieldsMap.get("publisher").setHidden(true);
			fields.add(fieldsMap.get("publisher"));
		}
		if (!fields.contains(fieldsMap.get("published"))) {
			fieldsMap.get("published").setHidden(true);
			fields.add(fieldsMap.get("published"));
		}
		if (!fields.contains(fieldsMap.get("creator"))) {
			fieldsMap.get("creator").setHidden(true);
			fields.add(fieldsMap.get("creator"));
		}
		if (!fields.contains(fieldsMap.get("created"))) {
			fieldsMap.get("created").setHidden(true);
			fields.add(fieldsMap.get("created"));
		}
		if (!fields.contains(fieldsMap.get("customId"))) {
			fieldsMap.get("customId").setHidden(true);
			fields.add(fieldsMap.get("customId"));
		}
		if (!fields.contains(fieldsMap.get("rating"))) {
			fieldsMap.get("rating").setHidden(true);
			fields.add(fieldsMap.get("rating"));
		}
		if (!fields.contains(fieldsMap.get("comment"))) {
			fieldsMap.get("comment").setHidden(true);
			fields.add(fieldsMap.get("comment"));
		}
		if (!fields.contains(fieldsMap.get("workflowStatus"))) {
			fieldsMap.get("workflowStatus").setHidden(true);
			fields.add(fieldsMap.get("workflowStatus"));
		}
		if (!fields.contains(fieldsMap.get("template"))) {
			fieldsMap.get("template").setHidden(true);
			fields.add(fieldsMap.get("template"));
		}
		if (!fields.contains(fieldsMap.get("startPublishing"))) {
			fieldsMap.get("startPublishing").setHidden(true);
			fields.add(fieldsMap.get("startPublishing"));
		}
		if (!fields.contains(fieldsMap.get("stopPublishing"))) {
			fieldsMap.get("stopPublishing").setHidden(true);
			fields.add(fieldsMap.get("stopPublishing"));
		}
		if (!fields.contains(fieldsMap.get("language"))) {
			fieldsMap.get("language").setHidden(true);
			fields.add(fieldsMap.get("language"));
		}

		list.setFields(fields.toArray(new ListGridField[0]));

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("docId")));
			}
		});

		// Count the total of events and the total of unchecked events
		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent e) {
				String title = I18N.message(guiDashlet.getTitle());
				String icn = "file";
				if ("lastaccessed".equals(guiDashlet.getName()))
					icn = "eye";

				if (event != null) {
					Record[] records = list.getRecordList().toArray();
					int unread = 0;
					for (Record record : records) {
						if ("true".equals(record.getAttributeAsString("new")))
							unread++;
					}

					int total = list.getTotalRows();
					title = I18N.message(event + "docs", Integer.toString(total));
					if (unread > 0)
						title = "<b>" + title + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" + I18N.message("newitems")
								+ ": " + unread + "</b>";

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

					if (Constants.EVENT_LOCKED.equals(event))
						Session.get().getUser().setLockedDocs(total);
					else if (Constants.EVENT_CHECKEDOUT.equals(event))
						Session.get().getUser().setCheckedOutDocs(total);
				}

				setTitle(AwesomeFactory.getIconHtml(icn, title));
			}
		});

		HeaderControl markAsRead = new HeaderControl(HeaderControl.TRASH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent e) {
				LD.contactingServer();
				DocumentService.Instance.get().markHistoryAsRead(event, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						LD.clearPrompt();
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
		markAsRead.setTooltip(I18N.message("maskallasread"));

		HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});
		exportControl.setTooltip(I18N.message("export"));

		HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});
		printControl.setTooltip(I18N.message("print"));

		if (event != null)
			setHeaderControls(HeaderControls.HEADER_LABEL, markAsRead, refreshControl, exportControl, printControl,
					HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
		else
			setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, exportControl, printControl,
					HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
		addItem(list);
	}

	private DocumentsDS getDataSource() {
		return new DocumentsDS(getDataSourceUrl(), I18N.getLocale(), guiDashlet.getExtendedAttributes());
	}

	private List<String> getColumnsList() {
		List<String> set = guiDashlet.getColumnsList();
		if (set == null || set.isEmpty()) {
			set = new ArrayList<String>();
			set.add("filename");
			set.add("version");
			set.add("date");
		}
		return set;
	}

	@Override
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = super.prepareContextMenu(document);
		if (event != null && (event.equals(Constants.EVENT_CHECKEDOUT) || event.equals(Constants.EVENT_LOCKED))) {
			MenuItem unlock = new MenuItem();
			unlock.setTitle(I18N.message("unlock"));
			unlock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
					DocumentService.Instance.get().unlock(new long[] { document.getId() }, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
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