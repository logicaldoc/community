package com.logicaldoc.gui.frontend.client.impex.email;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
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
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
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
		refresh.addClickHandler(click -> refresh());

		ToolStripButton addAccount = new ToolStripButton();
		addAccount.setTitle(I18N.message("addaccount"));
		toolStrip.addButton(addAccount);
		addAccount.addClickHandler(click -> {
			list.deselectAllRecords();
			GUIEmailAccount account = new GUIEmailAccount();
			showDetails(account);
		});

		if (Session.get().isDefaultTenant()) {
			toolStrip.addSeparator();

			ToolStripButton settings = new ToolStripButton();
			settings.setTitle(I18N.message("settings"));
			toolStrip.addButton(settings);
			settings.addClickHandler(click -> new EmailAccountSettings().show());
		}

		list.addCellContextClickHandler(click -> {
			showContextMenu();
			click.cancel();
		});

		list.addSelectionChangedHandler(changed -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				EmailAccountService.Instance.get().get(Long.parseLong(rec.getAttributeAsString("id")),
						new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(GUIEmailAccount account) {
								showDetails(account);
							}
						});
		});

		list.addDataArrivedHandler(
				arrived -> infoPanel.setMessage(I18N.message("showaccounts", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void refresh() {
		list.refresh(new EmailAccountsDS("default"));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_ACCOUNT;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> LD.ask(I18N.message(QUESTION), I18N.message("confirmdelete"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				EmailAccountService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showDetails(null);
					}
				});
			}
		}));

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler(click -> EmailAccountService.Instance.get()
				.test(Long.parseLong(rec.getAttributeAsString("id")), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Boolean result) {
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
					public void handleSuccess(Void result) {
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
					public void handleSuccess(Void result) {
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
							public void handleSuccess(Void result) {
								GuiLog.info(I18N.message("cachedeleted"), null);
							}
						});
					}
				}));

		MenuItem resetCounter = new MenuItem();
		resetCounter.setTitle(I18N.message("resetcounter"));
		resetCounter.addClickHandler(
				click -> LD.ask(I18N.message(QUESTION), I18N.message("confirmresetcounter"), choice -> {
					if (Boolean.TRUE.equals(choice)) {
						EmailAccountService.Instance.get().resetCounter(id, new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(Void result) {
								GuiLog.info(I18N.message("counterreseted"), null);
								rec.setAttribute("emails", "0");
								list.refreshRow(list.getRecordIndex(rec));
							}
						});
					}
				}));

		MenuItem clone = new MenuItem();
		clone.setTitle(I18N.message("clone"));
		clone.addClickHandler(click -> EmailAccountService.Instance.get()
				.clone(Long.parseLong(rec.getAttributeAsString("id")), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIEmailAccount result) {
						refresh();
					}
				}));

		contextMenu.setItems(test, enable, disable, clone, new MenuItemSeparator(), resetCache, resetCounter,
				new MenuItemSeparator(), delete);
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
		rec.setAttribute(ENABLED, account.isEnabled());
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