package com.logicaldoc.gui.frontend.client.tenant;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.data.TenantsDS;
import com.logicaldoc.gui.common.client.grid.EnabledDateListGridField;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.formatters.EnabledCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
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

	private static final String ENABLED = "eenabled";

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

		ListGridField id = new IdListGridField();
		id.setCellFormatter(new EnabledCellFormatter());

		ListGridField name = new ListGridField("name", I18N.message("name"), 100);
		name.setCanFilter(true);
		name.setCellFormatter(new EnabledCellFormatter());

		ListGridField displayName = new ListGridField("displayName", I18N.message("displayname"), 150);
		displayName.setCanFilter(true);
		displayName.setCellFormatter(new EnabledCellFormatter());

		ListGridField telephone = new ListGridField("telephone", I18N.message("phone"), 90);
		telephone.setCanFilter(true);
		telephone.setCellFormatter(new EnabledCellFormatter());

		ListGridField country = new ListGridField(COUNTRY, I18N.message(COUNTRY), 90);
		country.setCanFilter(true);
		country.setCellFormatter(new EnabledCellFormatter());

		ListGridField city = new ListGridField("city", I18N.message("city"), 90);
		city.setCanFilter(true);
		city.setCellFormatter(new EnabledCellFormatter());

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL), 200);
		email.setCanFilter(true);
		email.setCellFormatter(new EnabledCellFormatter());

		ListGridField address = new ListGridField(ADDRESS, I18N.message(ADDRESS), 150);
		address.setCanFilter(true);
		address.setCellFormatter(new EnabledCellFormatter());

		ListGridField enabled = new EnabledListGridField();

		ListGridField expire = new EnabledDateListGridField("expire", "expireson");

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
		TenantService.Instance.get().load(tenantId, new DefaultAsyncCallback<>() {
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
		rec.setAttribute(ENABLED, tenant.isEnabled());

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
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				TenantService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						super.onSuccess(result);
						list.removeSelectedData();
						list.deselectAllRecords();
						details = SELECT_TENANT;
						detailsContainer.setMembers(details);
					}
				});
			}
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(!rec.getAttributeAsBoolean(ENABLED));
		enable.addClickHandler(event -> TenantService.Instance.get().load(id, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUITenant tenant) {
				tenant.setEnabled(true);
				TenantService.Instance.get().save(tenant, new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUITenant v) {
						super.onSuccess(v);
						updateRecord(v);
						showTenantDetails(v);
					}
				});
			}
		}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(rec.getAttributeAsBoolean(ENABLED));
		disable.addClickHandler(event -> TenantService.Instance.get().load(id, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUITenant tenant) {
				tenant.setEnabled(false);
				TenantService.Instance.get().save(tenant, new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUITenant v) {
						super.onSuccess(v);
						updateRecord(v);
						showTenantDetails(v);
					}
				});
			}
		}));

		MenuItem password = new MenuItem();
		password.setTitle(I18N.message("changepassword"));
		password.addClickHandler(event -> new SetAdminPassword(rec.getAttributeAsString("name")).show());
		password.setEnabled(!Session.get().isDemo());

		if (id == Constants.TENANT_DEFAULTID) {
			delete.setEnabled(false);
			password.setEnabled(false);
			enable.setEnabled(false);
			disable.setEnabled(false);
		}

		contextMenu.setItems(password, enable, disable, delete);
		contextMenu.showContextMenu();
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