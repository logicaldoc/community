package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.data.WorkflowsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to visualize the workflows histories.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class WorkflowHistoryDialog extends Window {

	private GUIWorkflow selectedWorkflow = null;

	private ComboBoxItem user = null;

	private VLayout instancesContainer = new VLayout();

	private ListGrid instancesGrid;

	private VLayout historiesContainer = new VLayout();

	private ListGrid historiesGrid;

	private HLayout buttons;

	private Long selectedWorkflowInstance = null;

	private String tagFilter = null;

	public WorkflowHistoryDialog() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("workflowhistory"));
		setWidth(950);
		setHeight100();
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		final ComboBoxItem workflow = new ComboBoxItem("workflowSelection", I18N.message("workflowselect"));
		workflow.setWrapTitle(false);
		ListGridField name = new ListGridField("name");
		workflow.setValueField("id");
		workflow.setDisplayField("name");
		workflow.setPickListFields(name);
		workflow.setOptionDataSource(new WorkflowsDS(false, false));
		if (selectedWorkflow != null)
			workflow.setValue(selectedWorkflow.getName());

		final TextItem tagItem = ItemFactory.newTextItem("tag", "tag", null);

		ToolStripButton search = new ToolStripButton();
		search.setAutoFit(true);
		search.setTitle(I18N.message("search"));
		search.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord selectedRecord = workflow.getSelectedRecord();
				if (selectedRecord == null)
					return;

				WorkflowService.Instance.get().get(selectedRecord.getAttributeAsString("name"),
						new AsyncCallback<GUIWorkflow>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow result) {
								selectedWorkflow = result;
								tagFilter = tagItem.getValueAsString();
								loadInstancesGrid();
							}
						});
			}
		});

		ToolStripButton export = new ToolStripButton();
		export.setAutoFit(true);
		export.setTitle(I18N.message("export"));
		export.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(instancesGrid, true);
			}
		});

		ToolStripButton print = new ToolStripButton();
		print.setAutoFit(true);
		print.setTitle(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { instancesGrid });
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(workflow);
		toolStrip.addFormItem(tagItem);
		toolStrip.addButton(search);
		toolStrip.addSeparator();
		toolStrip.addButton(export);
		toolStrip.addButton(print);
		toolStrip.addFill();
		toolStrip.setWidth100();

		instancesContainer.setWidth100();
		instancesContainer.setHeight("40%");
		instancesContainer.setShowResizeBar(true);

		historiesContainer.setWidth100();
		historiesContainer.setHeight100();
		historiesContainer.setMembersMargin(3);

		VLayout body = new VLayout();
		body.setMembers(toolStrip, instancesContainer, historiesContainer);
		addItem(body);
		loadInstancesGrid();
	}

	private void loadInstancesGrid() {
		if (instancesGrid != null)
			instancesContainer.removeMember(instancesGrid);

		if (historiesGrid != null)
			historiesContainer.removeMember(historiesGrid);

		if (buttons != null)
			historiesContainer.removeMember(buttons);

		ListGridField id = new ListGridField("id", I18N.message("instance"), 70);
		ListGridField startDate = new ListGridField("startdate", I18N.message("startdate"), 120);
		startDate.setAlign(Alignment.CENTER);
		startDate.setType(ListGridFieldType.DATE);
		startDate.setCellFormatter(new DateCellFormatter(false));
		startDate.setCanFilter(false);
		ListGridField endDate = new ListGridField("enddate", I18N.message("enddate"), 120);
		endDate.setAlign(Alignment.CENTER);
		endDate.setType(ListGridFieldType.DATE);
		endDate.setCellFormatter(new DateCellFormatter(false));
		endDate.setCanFilter(false);
		ListGridField tag = new ListGridField("tag", I18N.message("tag"), 150);
		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 250);
		ListGridField documentIds = new ListGridField("documentIds", I18N.message("documentids"), 300);
		documentIds.setHidden(true);
		ListGridField initiator = new ListGridField("initiator", I18N.message("initiator"), 100);

		instancesGrid = new ListGrid();
		instancesGrid.setCanFreezeFields(true);
		instancesGrid.setAutoFetchData(true);
		instancesGrid.setShowHeader(true);
		instancesGrid.setCanSelectAll(false);
		instancesGrid.setSelectionType(SelectionStyle.SINGLE);
		instancesGrid.setHeight100();
		instancesGrid.setWidth100();
		instancesGrid.setBorder("1px solid #E1E1E1");
		instancesGrid.sort("startdate", SortDirection.DESCENDING);
		if (selectedWorkflow != null)
			instancesGrid.setDataSource(new WorkflowHistoriesDS(null, Long.parseLong(selectedWorkflow.getId()), null,
					tagFilter));
		instancesGrid.setFields(id, tag, startDate, endDate, documents, initiator, documentIds);

		instancesGrid.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				onSelectedInstance();
			}
		});
		instancesGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showInstanceContextMenu();
				event.cancel();
			}
		});

		instancesContainer.addMember(instancesGrid);
	}

	private void onSelectedInstance() {
		Record record = instancesGrid.getSelectedRecord();
		selectedWorkflowInstance = Long.parseLong(record.getAttributeAsString("id"));

		if (historiesGrid != null)
			historiesContainer.removeMember(historiesGrid);

		if (buttons != null)
			historiesContainer.removeMember(buttons);

		WorkflowHistoriesPanel panel = new WorkflowHistoriesPanel(selectedWorkflowInstance,
				Long.parseLong(selectedWorkflow.getId()), true);

		historiesContainer.setMembers(panel);
	}

	public GUIWorkflow getSelectedWorkflow() {
		return selectedWorkflow;
	}

	public void setUser(String id) {
		user.setValue(id);
	}

	private void showInstanceContextMenu() {
		com.smartgwt.client.widgets.menu.Menu contextMenu = new com.smartgwt.client.widgets.menu.Menu();

		final ListGridRecord selection = instancesGrid.getSelectedRecord();
		if (selection == null)
			return;

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							WorkflowService.Instance.get().deleteInstance(selection.getAttributeAsString("id"),
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											instancesGrid.removeSelectedData();
											instancesGrid.deselectAllRecords();
											historiesGrid.removeSelectedData();
											historiesGrid.deselectAllRecords();
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
}
