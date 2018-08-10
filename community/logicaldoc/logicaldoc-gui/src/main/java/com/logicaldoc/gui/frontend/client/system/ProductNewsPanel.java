package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.FeedMessagesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of feed messages and allows the selection.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class ProductNewsPanel extends AdminPanel {

	private ListGrid list;

	public ProductNewsPanel() {
		super("task.name.ProductNews");
	}

	@Override
	public void onDraw() {
		// Initialize the listing panel as placeholder
		final Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight100();
		initListGrid();

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		ToolStripButton markAsRead = new ToolStripButton();
		markAsRead.setTitle(I18N.message("maskallasread"));
		toolStrip.addButton(markAsRead);
		markAsRead.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				final ListGridRecord[] records = list.getRecords();
				if (records == null || records.length == 0)
					return;
				final long[] ids = new long[records.length];
				for (int i = 0; i < records.length; i++) {
					ids[i] = Long.parseLong(records[i].getAttribute("id"));
				}

				SystemService.Instance.get().markFeedMsgAsRead(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : records) {
							record.setAttribute("read", "1");
							list.refreshRow(list.getRecordIndex(record));
						}
					}
				});
			}
		});
		ToolStripButton markAsNotRead = new ToolStripButton();
		markAsNotRead.setTitle(I18N.message("maskallasnotread"));
		toolStrip.addButton(markAsNotRead);
		markAsNotRead.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				final ListGridRecord[] records = list.getRecords();
				if (records == null || records.length == 0)
					return;
				final long[] ids = new long[records.length];
				for (int i = 0; i < records.length; i++) {
					ids[i] = Long.parseLong(records[i].getAttribute("id"));
				}

				SystemService.Instance.get().markFeedMsgAsNotRead(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : records) {
							record.setAttribute("read", "0");
							list.refreshRow(list.getRecordIndex(record));
						}
					}
				});
			}
		});
		toolStrip.addFill();

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField title = new ListGridField("title", I18N.message("message"), 400);
		title.setCanFilter(true);

		ListGridField description = new ListGridField("description", I18N.message("description"), 250);
		description.setHidden(true);

		ListGridField pubDate = new ListGridField("pubDate", I18N.message("date"), 110);
		pubDate.setAlign(Alignment.CENTER);
		pubDate.setType(ListGridFieldType.DATE);
		pubDate.setCellFormatter(new DateCellFormatter(false));
		pubDate.setCanFilter(false);

		list = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (getFieldName(colNum).equals("title")) {
					if ("0".equals(record.getAttributeAsString("read"))) {
						return "font-weight:bold;";
					} else {
						return super.getCellCSSText(record, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanExpandRecords(true);
		list.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		list.setDetailField("description");
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(false);
		list.setDataSource(new FeedMessagesDS());
		list.setFields(id, title, description, pubDate);

		listing.addMember(list);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				Window.open(list.getSelectedRecord().getAttribute("link"), "_blank",
						"location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes");
			}
		});

		body.setMembers(toolStrip, listing);
	}

	private void initListGrid() {

	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++) {
			ids[i] = Long.parseLong(selection[i].getAttribute("id"));
		}

		MenuItem maskAsRead = new MenuItem();
		maskAsRead.setTitle(I18N.message("maskasread"));
		maskAsRead.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().markFeedMsgAsRead(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : selection) {
							record.setAttribute("read", "1");
							list.refreshRow(list.getRecordIndex(record));
						}
					}
				});
			}
		});

		MenuItem maskAsNotRead = new MenuItem();
		maskAsNotRead.setTitle(I18N.message("maskasnotread"));
		maskAsNotRead.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().markFeedMsgAsNotRead(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : selection) {
							record.setAttribute("read", "0");
							list.refreshRow(list.getRecordIndex(record));
						}
					}
				});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SystemService.Instance.get().deleteFeedMessages(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
								}
							});
						}
					}
				});
			}
		});

		MenuItem openLink = new MenuItem();
		openLink.setTitle(I18N.message("openlink"));
		openLink.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				Window.open(list.getSelectedRecord().getAttribute("link"), "_blank",
						"location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes");
			}
		});
		if (selection.length != 1)
			openLink.setEnabled(false);

		contextMenu.setItems(maskAsRead, maskAsNotRead, delete, openLink);
		contextMenu.showContextMenu();
	}
}
