package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to visualize the workflows histories.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowHistoryDialog extends Window {

	private static final String STARTDATE = "startdate";

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

		final ComboBoxItem workflow = ItemFactory.newWorkflowCombo(Session.get().getUser().getId());
		if (selectedWorkflow != null)
			workflow.setValue(selectedWorkflow.getName());

		final TextItem tagItem = ItemFactory.newTextItem("tag", null);

		max = ItemFactory.newSpinnerItem("max", "display", 50);
		max.setMin(0);
		max.setStep(10);
		max.setHint(I18N.message("elements"));
		max.addChangedHandler(event -> {
			if (selectedWorkflow != null)
				refreshInstancesGrid();
		});

		ToolStripButton search = new ToolStripButton();
		search.setAutoFit(true);
		search.setTitle(I18N.message("search"));
		search.addClickHandler(event -> {
			ListGridRecord selectedRecord = workflow.getSelectedRecord();
			if (selectedRecord == null)
				return;

			WorkflowService.Instance.get().get(selectedRecord.getAttributeAsString("name"), null,
					new AsyncCallback<>() {
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
		});

		ToolStripButton export = new ToolStripButton();
		export.setAutoFit(true);
		export.setTitle(I18N.message("export"));
		export.addClickHandler(event -> GridUtil.exportCSV(instancesGrid, true));

		ToolStripButton print = new ToolStripButton();
		print.setAutoFit(true);
		print.setTitle(I18N.message("print"));
		print.addClickHandler(event -> GridUtil.print(instancesGrid));

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
		ListGridField startDate = new DateListGridField(STARTDATE, STARTDATE, DateCellFormatter.FORMAT_LONG);

		ListGridField endDate = new DateListGridField("enddate", "enddate", DateCellFormatter.FORMAT_LONG);

		ListGridField version = new VersionListGridField("templateVersion", I18N.message("version"));
		version.setHidden(true);
		ListGridField templateId = new ListGridField("templateId", I18N.message("templateid"), 70);
		templateId.setHidden(true);
		ListGridField tag = new ListGridField("tag", I18N.message("tag"), 150);
		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 250);
		ListGridField documentIds = new ListGridField("documentIds", I18N.message("documentids"), 300);
		documentIds.setHidden(true);
		ListGridField initiator = new UserListGridField("initiator", "initiatorId", "initiator");

		instancesGrid = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rcd, int rowNum, int colNum) {
				String color = rcd.getAttributeAsString("wfDisplay");
				if (color != null && !color.isEmpty()) {
					return "background-color:" + color;
				} else {
					return super.getCellCSSText(rcd, rowNum, colNum);
				}
			}
		};
		
		instancesGrid.setCanFreezeFields(true);
		instancesGrid.setAutoFetchData(true);
		instancesGrid.setShowHeader(true);
		instancesGrid.setCanSelectAll(false);
		instancesGrid.setSelectionType(SelectionStyle.SINGLE);
		instancesGrid.setHeight100();
		instancesGrid.setWidth100();
		instancesGrid.setBorder("1px solid #E1E1E1");
		instancesGrid.sort(STARTDATE, SortDirection.DESCENDING);
		instancesGrid.setFields(id, version, templateId, tag, startDate, endDate, documents, initiator, documentIds);
		
		instancesGrid.addCellDoubleClickHandler(event -> onInstanceSelected());
		instancesGrid.addCellContextClickHandler(event -> {
			showInstanceContextMenu();
			event.cancel();
		});

		instancesContainer.addMember(instancesGrid);

		historiesPanel = new WorkflowHistoriesPanel(null, null, true);
		historiesContainer.setMembers(historiesPanel);
	}

	private void refreshInstancesGrid() {
		if (selectedWorkflow != null) {
			instancesGrid.refresh(new WorkflowHistoriesDS(null, Long.parseLong(selectedWorkflow.getId()), null,
					tagFilter, max.getValueAsInteger()));

			historiesPanel.setWfTemplateId(Long.parseLong(selectedWorkflow.getId()));
			historiesPanel.setWfInstanceId(null);
			historiesPanel.refresh();
		}
	}

	private void onInstanceSelected() {
		Record rec = instancesGrid.getSelectedRecord();
		historiesPanel.setWfInstanceId(rec.getAttributeAsLong("id"));
		historiesPanel.setWfTemplateId(rec.getAttributeAsLong("templateId"));
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
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
			if (Boolean.TRUE.equals(value)) {
				WorkflowService.Instance.get().deleteInstance(selection.getAttributeAsString("id"),
						new AsyncCallback<>() {
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
		}));

		MenuItem completionDiagram = new MenuItem();
		completionDiagram.setTitle(I18N.message("completiondiagram"));
		completionDiagram.addClickHandler(event -> WorkflowService.Instance.get().getCompletionDiagram(
				selectedWorkflow.getName(), selectedWorkflow.getVersion(), selection.getAttributeAsString("id"),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIWorkflow workflow) {
						WorkflowPreview diagramWindow = new WorkflowPreview(workflow);
						diagramWindow.show();
					}
				}));

		contextMenu.setItems(completionDiagram, delete);
		contextMenu.showContextMenu();
	}
}
