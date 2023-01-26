package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;
import com.logicaldoc.gui.common.client.data.RetentionPoliciesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
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
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DropCompleteEvent;
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
 * Panel showing the list of retention policies
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class RetentionPoliciesPanel extends AdminPanel {

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_POLICY;

	final static Canvas SELECT_POLICY = new HTMLPanel("&nbsp;" + I18N.message("selectpolicy"));

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
		if (details != null && details instanceof ImportFolderDetailsPanel) {
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

		ListGridField template = new ListGridField("template", I18N.message("template"), 150);
		template.setCanFilter(false);

		ListGridField action = prepareActionField();

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
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		ToolStripButton addPolicy = new ToolStripButton();
		addPolicy.setTitle(I18N.message("addpolicy"));
		toolStrip.addButton(addPolicy);
		addPolicy.addClickHandler((ClickEvent addPolicyClick) -> {
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
		list.addCellContextClickHandler((CellContextClickEvent listClick) -> {
			showContextMenu();
			listClick.cancel();
		});

		list.addSelectionChangedHandler((SelectionEvent listChanged) -> {
			Record record = list.getSelectedRecord();
			if (record != null)
				RetentionPoliciesService.Instance.get().getPolicy(Long.parseLong(record.getAttributeAsString("id")),
						new AsyncCallback<GUIRetentionPolicy>() {

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

		list.addDataArrivedHandler((DataArrivedEvent listArrived) -> {
			infoPanel.setMessage(I18N.message("showretpolicies", Integer.toString(list.getTotalRows())));
		});

		list.addDropCompleteHandler((DropCompleteEvent listDropCompleted) -> {
			ListGridRecord[] records = list.getRecords();
			long[] ids = new long[records.length];
			for (int i = 0; i < ids.length; i++)
				ids[i] = Long.parseLong(records[i].getAttributeAsString("id"));
			RetentionPoliciesService.Instance.get().reorder(ids, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					// Nothing to do
				}
			});
		});
	}

	private ListGridField prepareActionField() {
		ListGridField action = new ListGridField("action", I18N.message("action"), 150);
		action.setCanFilter(false);
		action.setCellFormatter((Object value, ListGridRecord record, int rowNum, int colNum) -> {
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
		dateOption.setCellFormatter((Object value, ListGridRecord record, int rowNum, int colNum) -> {
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
							RetentionPoliciesService.Instance.get().delete(id, new AsyncCallback<Void>() {
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
						}
					}
				});
			}
		});

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				RetentionPoliciesService.Instance.get().changeStatus(Long.parseLong(record.getAttributeAsString("id")),
						true, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "0");
								list.refreshRow(list.getRecordIndex(record));
							}
						});
			}
		});

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				RetentionPoliciesService.Instance.get().changeStatus(Long.parseLong(record.getAttributeAsString("id")),
						false, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "2");
								list.refreshRow(list.getRecordIndex(record));
							}
						});
			}
		});

		if ("0".equals(record.getAttributeAsString("eenabled")))
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
	 * Updates the selected record with new data
	 * 
	 * @param policy the policy to update
	 */
	public void updateRecord(GUIRetentionPolicy policy) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, policy.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", policy.getId());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("name", policy.getName());
		record.setAttribute("days", "" + policy.getRetentionDays());
		record.setAttribute("dateOption", "" + policy.getDateOption());
		record.setAttribute("template", policy.getTemplateName() != null ? policy.getTemplateName() : null);
		record.setAttribute("position", "" + policy.getPosition());
		record.setAttribute("action", "" + policy.getAction());

		list.refreshRow(list.getRecordIndex(record));
	}

	public void refresh() {
		list.refresh(new RetentionPoliciesDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_POLICY;
		detailsContainer.setMembers(details);
	}
}