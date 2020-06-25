package com.logicaldoc.gui.frontend.client.security.ldap;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.data.LDAPServersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.LDAPService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DropCompleteEvent;
import com.smartgwt.client.widgets.events.DropCompleteHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the LDAP servers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.2
 */
public class LDAPServersPanel extends AdminPanel {

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_SERVER;

	final static Canvas SELECT_SERVER = new HTMLPanel("&nbsp;" + I18N.message("selectserver"));

	public LDAPServersPanel() {
		super("extauth");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField url = new ListGridField("url", I18N.message("server"), 300);
		url.setCanFilter(true);
		url.setWidth("*");

		ListGridField enabled = new ListGridField("eenabled", " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, url);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanReorderRecords(true);
		list.setShowRowNumbers(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new LDAPServersDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		ToolStripButton addServer = new ToolStripButton();
		addServer.setTitle(I18N.message("addserver"));
		addServer.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				GUILDAPServer server = new GUILDAPServer();
				server.setLanguage(Session.get().getUser().getLanguage());
				showServerDetails(server);
			}
		});
		if (Feature.visible(Feature.LDAP))
			toolStrip.addButton(addServer);

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		VLayout listingLayout = new VLayout();
		listingLayout.setMembers(toolStrip, listing);
		listingLayout.setWidth(360);
		listingLayout.setShowResizeBar(true);

		HLayout hBody = new HLayout();
		hBody.setMembers(listingLayout, detailsContainer);

		body.setMembers(hBody);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record record = list.getSelectedRecord();
				if (record != null)
					LDAPService.Instance.get().get(record.getAttributeAsLong("id"), new AsyncCallback<GUILDAPServer>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUILDAPServer server) {
							showServerDetails(server);
						}
					});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showservers", Integer.toString(list.getTotalRows())));
			}
		});

		list.addDropCompleteHandler(new DropCompleteHandler() {

			@Override
			public void onDropComplete(DropCompleteEvent event) {
				List<Long> ids = new ArrayList<Long>();
				if (list.getRecords() != null && list.getRecords().length > 0) {
					for (ListGridRecord record : list.getRecords())
						ids.add(record.getAttributeAsLong("id"));
					LDAPService.Instance.get().reorder(ids.toArray(new Long[0]), new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							
						}
					});
				}
			}
		});
	}

	public void refresh() {
		list.refresh(new LDAPServersDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_SERVER;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							LDAPService.Instance.get().delete(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									refresh();
								}
							});
						}
					}
				});
			}
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public void showServerDetails(GUILDAPServer server) {
		detailsContainer.removeMember(details);
		details = new LDAPServerDetailsPanel(this, server);
		detailsContainer.addMember(details);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param server the LDAP server to updates
	 */
	public void updateRecord(GUILDAPServer server) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, server.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", server.getId());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("url", server.getUrl());

		record.setAttribute("eenabled", server.isEnabled() ? "0" : "2");

		list.refreshRow(list.getRecordIndex(record));
	}
}