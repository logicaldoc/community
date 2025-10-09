package com.logicaldoc.gui.frontend.client.security.ldap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.data.LDAPServersDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.formatters.EnabledCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.LDAPService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the LDAP servers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.2
 */
public class LDAPServersPanel extends AdminPanel {

	private static final String ENABLED = "eenabled";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_SERVER;

	static final Canvas SELECT_SERVER = new HTMLPanel("&nbsp;" + I18N.message("selectserver"));

	public LDAPServersPanel() {
		super("extauth");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();

		ListGridField id = new IdListGridField();
		id.setCellFormatter(new EnabledCellFormatter());

		ListGridField url = new ListGridField("url", I18N.message("server"), 300);
		url.setCanFilter(true);
		url.setWidth("*");
		url.setCellFormatter(new EnabledCellFormatter());

		ListGridField enabled = new EnabledListGridField();

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
		refresh.addClickHandler(event -> refresh());

		ToolStripButton addServer = new ToolStripButton();
		addServer.setTitle(I18N.message("addserver"));
		addServer.addClickHandler(event -> {
			list.deselectAllRecords();
			GUILDAPServer server = new GUILDAPServer();
			server.setLanguage(Session.get().getUser().getLanguage());
			showServerDetails(server);
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

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> onSelectionChanged());

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showservers", Integer.toString(list.getTotalRows()))));

		list.addDropCompleteHandler(event -> {
			if (list.getRecords() != null && list.getRecords().length > 0) {
				LDAPService.Instance.get().reorder(GridUtil.getIds(list.getRecords()), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void arg) {
						// Nothing to do
					}
				});
			}
		});
	}

	private void onSelectionChanged() {
		Record rec = list.getSelectedRecord();
		if (rec != null)
			LDAPService.Instance.get().get(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUILDAPServer server) {
					showServerDetails(server);
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

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						LDAPService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(Void result) {
								refresh();
							}
						});
					}
				}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(Boolean.FALSE.equals(rec.getAttributeAsBoolean(ENABLED)));
		enable.addClickHandler(event -> LDAPService.Instance.get().get(id, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUILDAPServer server) {
				server.setEnabled(true);
				LDAPService.Instance.get().save(server, new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(GUILDAPServer v) {
						updateRecord(v);
						onSelectionChanged();
					}
				});
			}
		}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(Boolean.TRUE.equals(rec.getAttributeAsBoolean(ENABLED)));
		disable.addClickHandler(event -> LDAPService.Instance.get().get(id, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUILDAPServer server) {
				server.setEnabled(false);
				LDAPService.Instance.get().save(server, new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(GUILDAPServer v) {
						updateRecord(v);
						onSelectionChanged();
					}
				});
			}
		}));

		contextMenu.setItems(enable, disable, new MenuItemSeparator(), delete);
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
	 * Updates the selected rec with new data
	 * 
	 * @param server the LDAP server to updates
	 */
	public void updateRecord(GUILDAPServer server) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, server.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", server.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("url", server.getUrl());

		rec.setAttribute(ENABLED, server.isEnabled());

		list.refreshRow(list.getRecordIndex(rec));
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