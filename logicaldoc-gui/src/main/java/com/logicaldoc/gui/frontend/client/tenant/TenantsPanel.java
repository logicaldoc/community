package com.logicaldoc.gui.frontend.client.tenant;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.data.TenantsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of tenants and a details area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class TenantsPanel extends AdminPanel {

	private static final String ENABLED_ICON = "enabledIcon";

	private static final String ADDRESS = "address";

	private static final String EMAIL = "email";

	private static final String COUNTRY = "country";

	private ListGrid list;

	private Layout detailsContainer = new VLayout();

	static final Canvas SELECT_TENANT = new HTMLPanel("&nbsp;" + I18N.message("selecttenant"));

	private Canvas details = SELECT_TENANT;

	public TenantsPanel() {
		super("tenants");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel as placeholder
		final Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("50%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 100);
		name.setCanFilter(true);

		ListGridField displayName = new ListGridField("displayName", I18N.message("displayname"), 150);
		displayName.setCanFilter(true);

		ListGridField telephone = new ListGridField("telephone", I18N.message("phone"), 90);
		telephone.setCanFilter(true);

		ListGridField country = new ListGridField(COUNTRY, I18N.message(COUNTRY), 90);
		country.setCanFilter(true);

		ListGridField city = new ListGridField("city", I18N.message("city"), 90);
		city.setCanFilter(true);

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL), 200);
		email.setCanFilter(true);

		ListGridField address = new ListGridField(ADDRESS, I18N.message(ADDRESS), 150);
		address.setCanFilter(true);

		ListGridField enabled = new ListGridField(ENABLED_ICON, " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".png");
		enabled.setCanFilter(false);

		ListGridField expire = new DateListGridField("expire", "expireson");

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new TenantsDS());
		list.setFields(id, enabled, name, displayName, expire, email, telephone, country, city, address);

		listing.addMember(infoPanel);
		listing.addMember(list);

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtenant"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> {
			list.deselectAllRecords();
			GUITenant tenant = new GUITenant();
			tenant.setBranding(Session.get().getInfo().getBranding());
			showTenantDetails(tenant);
		});
		toolStrip.addFill();

		body.setMembers(toolStrip, listing, detailsContainer);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				loadTenant(Long.parseLong(rec.getAttributeAsString("id")));
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showtenants", Integer.toString(list.getTotalRows()))));
	}

	public void loadTenant(long tenantId) {
		TenantService.Instance.get().load(tenantId, new AsyncCallback<GUITenant>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITenant tenant) {
				showTenantDetails(tenant);
			}
		});
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param tenant the tenant to update
	 */
	public void updateRecord(GUITenant tenant) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, tenant.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", tenant.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", tenant.getName());
		rec.setAttribute("displayName", tenant.getDisplayName());
		rec.setAttribute(EMAIL, tenant.getEmail());
		rec.setAttribute("telephone", tenant.getTelephone());
		rec.setAttribute(ADDRESS, tenant.getStreet());
		rec.setAttribute(COUNTRY, tenant.getCountry());
		rec.setAttribute("city", tenant.getCity());
		rec.setAttribute("postalCode", tenant.getPostalCode());
		rec.setAttribute("state", tenant.getState());
		rec.setAttribute("expire", tenant.getExpire());

		if (tenant.isEnabled()) {
			rec.setAttribute(ENABLED_ICON, "bullet_green");
			rec.setAttribute("eenabled", false);
		} else {
			rec.setAttribute(ENABLED_ICON, "bullet_red");
			rec.setAttribute("eenabled", false);
		}

		list.refreshRow(list.getRecordIndex(rec));
	}

	public void showTenantDetails(GUITenant tenant) {
		if (!(details instanceof TenantDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new TenantDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((TenantDetailsPanel) details).setTenant(tenant);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> {
			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
				if (Boolean.TRUE.equals(value)) {
					TenantService.Instance.get().delete(id, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							list.removeSelectedData();
							list.deselectAllRecords();
							details = SELECT_TENANT;
							detailsContainer.setMembers(details);
						}
					});
				}
			});
		});

		MenuItem password = new MenuItem();
		password.setTitle(I18N.message("changepassword"));
		password.addClickHandler(event -> new SetAdminPassword(rec.getAttributeAsString("name")).show());
		password.setEnabled(!Session.get().isDemo());

		if (id == Constants.TENANT_DEFAULTID) {
			delete.setEnabled(false);
			password.setEnabled(false);
		}

		contextMenu.setItems(password, delete);
		contextMenu.showContextMenu();
	}
}