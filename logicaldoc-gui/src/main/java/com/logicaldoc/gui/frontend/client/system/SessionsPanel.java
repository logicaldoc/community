package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.SessionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Displays a list of user sessions, allowing the kill operation.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SessionsPanel extends VLayout {

	private static final String STATUS_LABEL = "statusLabel";

	private static final String STATUS = "status";

	private RefreshableListGrid sessionsGrid;

	private StaticTextItem activeSessions;

	public SessionsPanel() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(event -> sessionsGrid.refresh(new SessionsDS()));

		activeSessions = ItemFactory.newStaticTextItem("activesessions", "");

		toolStrip.addButton(refresh);
		toolStrip.addSeparator();
		toolStrip.addFormItem(activeSessions);
		toolStrip.addFill();
		addMember(toolStrip);
	}

	@Override
	public void onDraw() {
		prepareSessionsGrid();

		sessionsGrid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		sessionsGrid.addDoubleClickHandler(event -> {
			LD.askForValue(I18N.message("sid"), I18N.message("sid"),
					sessionsGrid.getSelectedRecord().getAttributeAsString("sid"), value -> {
						// Nothing to do
					});
			event.cancel();
		});

		sessionsGrid.addDataArrivedHandler(event -> {
			// Search the records with status=0 that are the active sessions
			Record[] records = sessionsGrid.getRecordList().findAll(STATUS, "0");
			if (records == null || records.length < 1)
				activeSessions.setValue("0");
			else
				activeSessions.setValue(Integer.toString(records.length));
		});

		addMember(sessionsGrid);
	}

	private void prepareSessionsGrid() {
		ListGridField sid = new ListGridField("sid", I18N.message("sid"), 250);

		ListGridField node = new ListGridField("node", I18N.message("node"), 250);
		node.setHidden(true);

		ListGridField username = new ListGridField("username", I18N.message("username"), 80);
		username.setCanFilter(true);

		ListGridField client = new ListGridField("client", I18N.message("client"), 200);
		client.setCanFilter(true);

		ListGridField tenant = new ListGridField("tenant", I18N.message("tenant"), 80);
		tenant.setCanFilter(true);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField renew = new DateListGridField("renew", "lastrenew");

		ListGridField statusLabel = new ListGridField(STATUS_LABEL, I18N.message(STATUS), 80);
		statusLabel.setCanFilter(false);

		sessionsGrid = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (getFieldName(colNum).equals("sid")) {
					if (Session.get().getSid() != null && Session.get().getSid().equals(rec.getAttribute("sid"))) {
						return "font-weight: bold;";
					} else {
						return super.getCellCSSText(rec, rowNum, colNum);
					}
				} else if (getFieldName(colNum).equals(STATUS_LABEL)) {
					if (!"0".equals(rec.getAttribute(STATUS))) {
						return "color: red;";
					} else {
						return super.getCellCSSText(rec, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(rec, rowNum, colNum);
				}
			}
		};
		sessionsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		sessionsGrid.setShowRecordComponents(true);
		sessionsGrid.setShowRecordComponentsByCell(true);
		sessionsGrid.setCanFreezeFields(true);
		sessionsGrid.setAutoFetchData(true);
		sessionsGrid.setSelectionType(SelectionStyle.SINGLE);
		sessionsGrid.setDataSource(new SessionsDS());

		sessionsGrid.setFields(sid, statusLabel, username, tenant, created, renew, node, client);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem killSession = new MenuItem();
		killSession.setTitle(I18N.message("kill"));
		killSession.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmkill"), yes -> {
			if (Boolean.TRUE.equals(yes)) {
				ListGridRecord rec = sessionsGrid.getSelectedRecord();
				SecurityService.Instance.get().kill(rec.getAttributeAsString("sid"), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						sessionsGrid.getSelectedRecord().setAttribute(STATUS_LABEL, "Closed");
						sessionsGrid.getSelectedRecord().setAttribute(STATUS, "1");
						sessionsGrid.refreshRow(sessionsGrid.getRecordIndex(sessionsGrid.getSelectedRecord()));
					}
				});
			}
		}));

		if (!"0".equals(sessionsGrid.getSelectedRecord().getAttributeAsString(STATUS))
				|| (Session.get().getSid() != null
						&& Session.get().getSid().equals(sessionsGrid.getSelectedRecord().getAttributeAsString("sid"))))
			killSession.setEnabled(false);

		if (!sessionsGrid.getSelectedRecord().getAttributeAsString("node")
				.equals(Session.get().getInfo().getInstallationId()))
			killSession.setEnabled(false);

		contextMenu.setItems(killSession);
		contextMenu.showContextMenu();
	}
}
