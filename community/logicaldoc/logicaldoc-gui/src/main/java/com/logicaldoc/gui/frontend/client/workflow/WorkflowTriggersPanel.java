package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.WorkflowTriggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Displays the list of all workflow triggers on a folder.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowTriggersPanel extends VLayout {

	private ListGrid list;

	private GUIFolder folder;

	private SelectItem workflows = null;

	private SelectItem templates = null;

	public WorkflowTriggersPanel(final GUIFolder folder) {
		this.folder = folder;
	}

	@Override
	protected void onDraw() {
		refresh();

		Button addTrigger = new Button(I18N.message("workflowtriggeradd"));
		addTrigger.setAutoFit(true);

		Button applyTriggersToSubfolders = new Button(I18N.message("applytosubfolders"));
		applyTriggersToSubfolders.setAutoFit(true);
		applyTriggersToSubfolders.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				WorkflowService.Instance.get().applyTriggersToTree(folder.getId(), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void v) {
						ContactingServer.get().hide();
					}
				});
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembers(addTrigger, applyTriggersToSubfolders);
		buttons.setMembersMargin(3);
		buttons.setWidth100();
		buttons.setHeight(15);

		addTrigger.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				final Window window = new Window();
				window.setTitle(I18N.message("workflowtriggertext"));
				window.setCanDragResize(true);
				window.setIsModal(true);
				window.setShowModalMask(true);
				window.centerInPage();
				window.setAutoSize(true);

				VStack layout = new VStack(5);

				// Workflows list
				DynamicForm workflowForm = new DynamicForm();
				workflowForm.setAlign(Alignment.LEFT);
				workflowForm.setTitleOrientation(TitleOrientation.LEFT);
				workflowForm.setNumCols(2);
				workflowForm.setColWidths(110, "*");

				workflows = ItemFactory.newWorkflowSelector();
				workflows.setColSpan(2);
				workflows.setEndRow(true);
				workflows.setRequired(true);
				workflowForm.setItems(workflows);

				// Templates list
				DynamicForm templateForm = new DynamicForm();
				templateForm.setAlign(Alignment.LEFT);
				templateForm.setTitleOrientation(TitleOrientation.LEFT);
				templateForm.setNumCols(2);
				templateForm.setColWidths(110, "*");

				templates = ItemFactory.newTemplateSelector(true, null);
				templates.setWrapTitle(false);
				templates.setColSpan(2);
				templates.setEndRow(true);
				templates.setWidth(200);
				templates.setMultipleAppearance(MultipleAppearance.GRID);
				templateForm.setItems(templates);

				final RadioGroupItem checkin = ItemFactory.newBooleanSelector("checkin",
						I18N.message("triggerateverycheckin"));
				checkin.setValue("no");
				checkin.setWrapTitle(false);

				DynamicForm form = new DynamicForm();
				form.setTitleOrientation(TitleOrientation.LEFT);

				SubmitItem saveButton = new SubmitItem("save", I18N.message("save"));
				saveButton.setAlign(Alignment.LEFT);
				saveButton.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
					@Override
					public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
						String workflowSelectedId = "";
						if (workflows.getValue() != null) {
							workflowSelectedId = workflows.getValue().toString();

							String templateSelectedId = "";
							if (templates.getValue() != null) {
								templateSelectedId = templates.getValueAsString();
							}

							WorkflowService.Instance.get().saveTrigger(Long.toString(getFolder().getId()),
									workflowSelectedId, templateSelectedId,
									"yes".equals(checkin.getValueAsString()) ? 1 : 0, new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											removeMember(list);
											refresh();
											window.destroy();
										}
									});
						} else {
							SC.warn(I18N.message("workflowselection"));
						}
					}
				});

				form.setFields(checkin, saveButton);

				layout.addMember(workflowForm);
				layout.addMember(templateForm);
				layout.addMember(form);

				window.addItem(layout);
				window.show();
			}
		});

		setMembersMargin(4);
		addMember(buttons);
	}

	private void refresh() {
		ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"), 200);
		workflow.setCanFilter(true);

		ListGridField template = new ListGridField("template", I18N.message("template"), 200);
		template.setCanFilter(true);

		ListGridField checkin = new ListGridField("triggerAtCheckin", I18N.message("triggeratcheckin"));
		checkin.setCanFilter(false);
		checkin.setAlign(Alignment.LEFT);
		checkin.setWidth("*");

		list = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				return super.getCellCSSText(record, rowNum, colNum);
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));

		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(false);
		list.setDataSource(new WorkflowTriggersDS("" + folder.getId()));
		list.setFields(workflow, template, checkin);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		addMember(list, 0);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteTrigger = new MenuItem();
		deleteTrigger.setTitle(I18N.message("ddelete"));
		deleteTrigger.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ListGridRecord record = list.getSelectedRecord();
							WorkflowService.Instance.get().deleteTrigger(
									Long.parseLong(record.getAttributeAsString("id")), new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											removeMember(list);
											refresh();
										}
									});
						}
					}
				});
			}
		});

		contextMenu.setItems(deleteTrigger);
		contextMenu.showContextMenu();
	}

	public GUIFolder getFolder() {
		return folder;
	}
}