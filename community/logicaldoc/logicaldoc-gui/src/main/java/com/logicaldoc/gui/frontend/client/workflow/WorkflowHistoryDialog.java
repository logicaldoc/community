package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.data.WorkflowsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.AvatarListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to visualize the workflows histories.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowHistoryDialog extends Window {

	private GUIWorkflow selectedWorkflow = null;

	private ComboBoxItem user = null;

	private RefreshableListGrid instancesGrid;

	private String tagFilter = null;

	private WorkflowHistoriesPanel historiesPanel = null;
	
	private SpinnerItem max;

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
		
		max = ItemFactory.newSpinnerItem("max", "display", 50);
		max.setMin(0);
		max.setStep(10);
		max.setHint(I18N.message("elements"));
		max.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				if(selectedWorkflow!=null)
					refreshInstancesGrid();
			}
		});

		ToolStripButton search = new ToolStripButton();
		search.setAutoFit(true);
		search.setTitle(I18N.message("search"));
		search.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord selectedRecord = workflow.getSelectedRecord();
				if (selectedRecord == null)
					return;

				WorkflowService.Instance.get().get(selectedRecord.getAttributeAsString("name"), null,
						new AsyncCallback<GUIWorkflow>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow result) {
								selectedWorkflow = result;
								tagFilter = tagItem.getValueAsString();
								refreshInstancesGrid();
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
				GridUtil.exportCSV(instancesGrid, true);
			}
		});

		ToolStripButton print = new ToolStripButton();
		print.setAutoFit(true);
		print.setTitle(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(instancesGrid);
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();
		toolStrip.addFormItem(workflow);
		toolStrip.addFormItem(tagItem);
		toolStrip.addButton(search);
		toolStrip.addSeparator();
		toolStrip.addButton(export);
		toolStrip.addButton(print);
		toolStrip.addFill();
		toolStrip.setWidth100();

		VLayout instancesContainer = new VLayout();
		instancesContainer.setWidth100();
		instancesContainer.setHeight("40%");
		instancesContainer.setShowResizeBar(true);

		VLayout historiesContainer = new VLayout();
		historiesContainer.setWidth100();
		historiesContainer.setHeight100();
		historiesContainer.setMembersMargin(3);

		VLayout body = new VLayout();
		body.setMembers(toolStrip, instancesContainer, historiesContainer);
		addItem(body);

		ListGridField id = new ListGridField("id", I18N.message("instance"), 70);
		ListGridField startDate = new DateListGridField("startdate", "startdate", DateCellFormatter.FORMAT_LONG);
		
		ListGridField endDate = new DateListGridField("enddate", "enddate", DateCellFormatter.FORMAT_LONG);
		
		ListGridField version = new ListGridField("templateVersion", I18N.message("version"), 70);
		version.setHidden(true);
		ListGridField templateId = new ListGridField("templateId", I18N.message("templateid"), 70);
		templateId.setHidden(true);
		ListGridField tag = new ListGridField("tag", I18N.message("tag"), 150);
		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 250);
		ListGridField documentIds = new ListGridField("documentIds", I18N.message("documentids"), 300);
		documentIds.setHidden(true);
		ListGridField initiator = new AvatarListGridField("initiator", "initiatorId", "initiator", 100);

		instancesGrid = new RefreshableListGrid();
		instancesGrid.setCanFreezeFields(true);
		instancesGrid.setAutoFetchData(true);
		instancesGrid.setShowHeader(true);
		instancesGrid.setCanSelectAll(false);
		instancesGrid.setSelectionType(SelectionStyle.SINGLE);
		instancesGrid.setHeight100();
		instancesGrid.setWidth100();
		instancesGrid.setBorder("1px solid #E1E1E1");
		instancesGrid.sort("startdate", SortDirection.DESCENDING);
		instancesGrid.setFields(id, version, templateId, tag, startDate, endDate, documents, initiator, documentIds);

		instancesGrid.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				onInstanceSelected();
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

		historiesPanel = new WorkflowHistoriesPanel(null, null, true);
		historiesContainer.setMembers(historiesPanel);
	}

	private void refreshInstancesGrid() {
		if (selectedWorkflow != null) {
			instancesGrid
					.refresh(new WorkflowHistoriesDS(null, Long.parseLong(selectedWorkflow.getId()), null, tagFilter, max.getValueAsInteger()));

			historiesPanel.setWfTemplateId(Long.parseLong(selectedWorkflow.getId()));
			historiesPanel.setWfInstanceId(null);
			historiesPanel.refresh();
		}
	}

	private void onInstanceSelected() {
		Record record = instancesGrid.getSelectedRecord();
		historiesPanel.setWfInstanceId(record.getAttributeAsLong("id"));
		historiesPanel.setWfTemplateId(record.getAttributeAsLong("templateId"));
		historiesPanel.refresh();
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
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											instancesGrid.removeSelectedData();
											instancesGrid.deselectAllRecords();
											historiesPanel.getHistoriesGrid().removeSelectedData();
											historiesPanel.getHistoriesGrid().deselectAllRecords();
										}
									});
						}
					}
				});
			}
		});

		MenuItem completionDiagram = new MenuItem();
		completionDiagram.setTitle(I18N.message("completiondiagram"));
		completionDiagram.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				WorkflowService.Instance.get().getCompletionDiagram(selectedWorkflow.getName(),
						selectedWorkflow.getVersion(), selection.getAttributeAsString("id"),
						new AsyncCallback<GUIWorkflow>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow workflow) {
								WorkflowPreview diagramWindow = new WorkflowPreview(workflow);
								diagramWindow.show();
							}
						});
			}
		});

		contextMenu.setItems(completionDiagram, delete);
		contextMenu.showContextMenu();
	}
}
