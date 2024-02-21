package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;
import com.logicaldoc.gui.common.client.data.RetentionPoliciesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.impex.folders.ImportFolderDetailsPanel;
import com.logicaldoc.gui.frontend.client.services.RetentionPoliciesService;
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
 * Panel showing the list of retention policies
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class RetentionPoliciesPanel extends AdminPanel {

	private static final String ACTION = "action";

	private static final String EENABLED = "eenabled";

	private static final String TEMPLATE = "template";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_POLICY;

	static final Canvas SELECT_POLICY = new HTMLPanel("&nbsp;" + I18N.message("selectpolicy"));

	public RetentionPoliciesPanel() {
		super("retentionpolicies");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");
		detailsContainer.clear();

		Layout listing = new VLayout();
		listing.clear();
		if (list != null)
			listing.removeMember(list);
		if (details instanceof ImportFolderDetailsPanel) {
			detailsContainer.removeMember(details);
			details = SELECT_POLICY;
		}

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("70%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);
		name.setCanFilter(false);

		ListGridField days = new ListGridField("days", I18N.message("ddays"), 60);
		days.setCanFilter(false);

		ListGridField dateOption = prepareDateOptionField();

		ListGridField template = new ListGridField(TEMPLATE, I18N.message(TEMPLATE), 150);
		template.setCanFilter(false);

		ListGridField action = prepareActionField();

		ListGridField enabled = new ListGridField(EENABLED, " ", 24);
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
		list.setFields(enabled, id, name, days, dateOption, template, action);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setCanSort(false);
		list.setFilterOnKeypress(true);
		list.setDataSource(new RetentionPoliciesDS());
		list.setShowRowNumbers(true);
		list.setCanReorderRecords(true);

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

		ToolStripButton addPolicy = new ToolStripButton();
		addPolicy.setTitle(I18N.message("addpolicy"));
		toolStrip.addButton(addPolicy);
		addPolicy.addClickHandler(addPolicyClick -> {
			list.deselectAllRecords();
			GUIRetentionPolicy policy = new GUIRetentionPolicy();
			showPolicyDetails(policy);
		});

		addListHandlers(infoPanel);

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void addListHandlers(final InfoPanel infoPanel) {
		list.addCellContextClickHandler(listClick -> {
			showContextMenu();
			listClick.cancel();
		});

		list.addSelectionChangedHandler(listChanged -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				RetentionPoliciesService.Instance.get().getPolicy(Long.parseLong(rec.getAttributeAsString("id")),
						new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIRetentionPolicy policy) {
								showPolicyDetails(policy);
							}
						});
		});

		list.addDataArrivedHandler(listArrived -> infoPanel
				.setMessage(I18N.message("showretpolicies", Integer.toString(list.getTotalRows()))));

		list.addDropCompleteHandler(listDropCompleted -> RetentionPoliciesService.Instance.get()
				.reorder(GridUtil.getIds(list.getRecords()), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						// Nothing to do
					}
				}));
	}

	private ListGridField prepareActionField() {
		ListGridField action = new ListGridField(ACTION, I18N.message(ACTION), 150);
		action.setCanFilter(false);
		action.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			int val = Integer.parseInt(value.toString());
			if (val == GUIRetentionPolicy.ACTION_ARCHIVE)
				return I18N.message("archive");
			else if (val == GUIRetentionPolicy.ACTION_DELETE)
				return I18N.message("ddelete");
			else
				return I18N.message("unpublish");
		});
		return action;
	}

	private ListGridField prepareDateOptionField() {
		ListGridField dateOption = new ListGridField("dateOption", I18N.message("dateoption"), 100);
		dateOption.setCanFilter(false);
		dateOption.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			int val = Integer.parseInt(value.toString());
			if (val == GUIRetentionPolicy.DATE_OPT_ARCHIVED)
				return I18N.message("archiveds");
			else if (val == GUIRetentionPolicy.DATE_OPT_CREATION)
				return I18N.message("created");
			else if (val == GUIRetentionPolicy.DATE_OPT_PUBLISHED)
				return I18N.message("published");
			else
				return I18N.message("stoppublishing");
		});
		return dateOption;
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm))
				RetentionPoliciesService.Instance.get().delete(id, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showPolicyDetails(null);
					}
				});
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> RetentionPoliciesService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "0");
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> RetentionPoliciesService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "2");
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		if ("0".equals(rec.getAttributeAsString(EENABLED)))
			contextMenu.setItems(disable, delete);
		else
			contextMenu.setItems(enable, delete);

		contextMenu.showContextMenu();
	}

	public void showPolicyDetails(GUIRetentionPolicy policy) {
		if (!(details instanceof ImportFolderDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new RetentionPolicyDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((RetentionPolicyDetailsPanel) details).setPolicy(policy);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param policy the policy to update
	 */
	public void updateRecord(GUIRetentionPolicy policy) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, policy.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", policy.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", policy.getName());
		rec.setAttribute("days", "" + policy.getRetentionDays());
		rec.setAttribute("dateOption", "" + policy.getDateOption());
		rec.setAttribute(TEMPLATE, policy.getTemplateName() != null ? policy.getTemplateName() : null);
		rec.setAttribute("position", "" + policy.getPosition());
		rec.setAttribute(ACTION, "" + policy.getAction());

		list.refreshRow(list.getRecordIndex(rec));
	}

	public void refresh() {
		list.refresh(new RetentionPoliciesDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_POLICY;
		detailsContainer.setMembers(details);
	}
}