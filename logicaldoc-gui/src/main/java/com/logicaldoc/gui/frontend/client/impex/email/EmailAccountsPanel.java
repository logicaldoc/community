package com.logicaldoc.gui.frontend.client.impex.email;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.data.EmailAccountsDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.IntegerListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.EmailAccountService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of import email accounts
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountsPanel extends AdminPanel {

	private static final String QUESTION = "question";

	private static final String ENABLED = "eenabled";

	private static final String EMAIL = "email";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_ACCOUNT;

	static final Canvas SELECT_ACCOUNT = new HTMLPanel("&nbsp;" + I18N.message("selectaccount"));

	public EmailAccountsPanel() {
		super("emailaccounts");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL), 300);
		email.setCanFilter(true);

		ListGridField enabled = new EnabledListGridField();

		IntegerListGridField emails = new IntegerListGridField("emails", I18N.message("importedemails"));
		emails.setAutoFitWidth(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, email, emails);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new EmailAccountsDS("default"));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler((ClickEvent event) -> {
			list.refresh(new EmailAccountsDS("default"));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_ACCOUNT;
			detailsContainer.setMembers(details);
		});

		ToolStripButton addAccount = new ToolStripButton();
		addAccount.setTitle(I18N.message("addaccount"));
		toolStrip.addButton(addAccount);
		addAccount.addClickHandler((ClickEvent event) -> {
			list.deselectAllRecords();
			GUIEmailAccount account = new GUIEmailAccount();
			showDetails(account);
		});

		list.addCellContextClickHandler((CellContextClickEvent event) -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler((SelectionEvent event) -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				EmailAccountService.Instance.get().get(Long.parseLong(rec.getAttributeAsString("id")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(GUIEmailAccount account) {
								showDetails(account);
							}
						});
		});

		list.addDataArrivedHandler((DataArrivedEvent event) -> infoPanel
				.setMessage(I18N.message("showaccounts", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> LD.ask(I18N.message(QUESTION),
				I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						EmailAccountService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								list.removeSelectedData();
								list.deselectAllRecords();
								showDetails(null);
							}
						});
					}
				}));

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler((MenuItemClickEvent event) -> EmailAccountService.Instance.get()
				.test(Long.parseLong(rec.getAttributeAsString("id")), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Boolean result) {
						if (result.booleanValue())
							SC.say(I18N.message("connectionestablished"));
						else
							SC.warn(I18N.message("connectionfailed"));
					}
				}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(Boolean.FALSE.equals(rec.getAttributeAsBoolean(ENABLED)));
		enable.addClickHandler(click -> EmailAccountService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(Boolean.TRUE.equals(rec.getAttributeAsBoolean(ENABLED)));
		disable.addClickHandler(click -> EmailAccountService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem resetCache = new MenuItem();
		resetCache.setTitle(I18N.message("resetcache"));
		resetCache
				.addClickHandler(click -> LD.ask(I18N.message(QUESTION), I18N.message("confirmresetcache"), choice -> {
					if (Boolean.TRUE.equals(choice)) {
						EmailAccountService.Instance.get().resetCache(id, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								GuiLog.info(I18N.message("cachedeleted"), null);
							}
						});
					}
				}));

		MenuItem resetCounter = new MenuItem();
		resetCounter.setTitle(I18N.message("resetcounter"));
		resetCounter.addClickHandler((MenuItemClickEvent event) -> LD.ask(I18N.message(QUESTION),
				I18N.message("confirmresetcounter"), choice -> {
					if (Boolean.TRUE.equals(choice)) {
						EmailAccountService.Instance.get().resetCounter(id, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								GuiLog.info(I18N.message("counterreseted"), null);
								rec.setAttribute("emails", "0");
								list.refreshRow(list.getRecordIndex(rec));
							}
						});
					}
				}));

		contextMenu.setItems(test, enable, disable, delete, resetCache, resetCounter);
		contextMenu.showContextMenu();
	}

	public void showDetails(GUIEmailAccount account) {
		if (!(details instanceof EmailAccountsPanel)) {
			detailsContainer.removeMember(details);
			details = new EmailAccountDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((EmailAccountDetailsPanel) details).setAccount(account);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param account the email account to update
	 */
	public void updateRecord(GUIEmailAccount account) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, account.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", account.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute(EMAIL, account.getMailAddress());
		rec.setAttribute(ENABLED, account.getEnabled() == 1);
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