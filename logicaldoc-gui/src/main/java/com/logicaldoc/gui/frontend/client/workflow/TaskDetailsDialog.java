package com.logicaldoc.gui.frontend.client.workflow;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.data.WorkflowHistoriesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.StatusIconsListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentCheckin;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.selector.DocumentSelectorDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This popup window is used to display the details of a selected task in a
 * specific workflow instance.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TaskDetailsDialog extends Window {

	private static final String STARTED = "started";

	private static final String COMMENT = "comment";

	private static final String USER_ID = "userId";

	private static final String WORKFLOW_STR = "workflow";

	private GUIWorkflow workflow = null;

	private DocumentsListGrid appendedDocs;

	private SelectItem user = null;

	private ValuesManager vm = new ValuesManager();

	private HLayout mainPanel = null;

	private HLayout form = null;

	private VLayout sxLayout = null;

	private WorkflowDashboard workflowDashboard;

	private TabSet tabs = new TabSet();

	private VLayout buttonsPanel = null;

	private Tab docsTab = null;

	private Tab workflowTab = null;

	private Tab notesTab = null;

	private Tab historyTab = null;

	private VLayout notesPanel = null;

	private VLayout appendedDocsPanel = null;

	private boolean readOnly = false;

	private List<ButtonItem> transitionButtons = new ArrayList<>();

	public TaskDetailsDialog(final WorkflowDashboard dashboard, GUIWorkflow wfl, boolean readOnly) {
		this.workflow = wfl;
		this.workflowDashboard = dashboard;
		this.readOnly = readOnly;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message(WORKFLOW_STR));
		setWidth(760);
		setHeight(650);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();

		workflowTab = new Tab(I18N.message(WORKFLOW_STR));
		tabs.addTab(workflowTab, 0);

		buttonsPanel = new VLayout();

		mainPanel = new HLayout();
		mainPanel.setMembersMargin(5);
		mainPanel.setMembers(tabs, buttonsPanel);

		addItem(mainPanel);

		form = new HLayout(25);
		form.setMargin(20);
		form.setWidth100();
		form.setHeight100();

		sxLayout = new VLayout(10);
		appendedDocsPanel = new VLayout(5);
		appendedDocsPanel.setMargin(5);
		notesPanel = new VLayout(5);
		notesPanel.setMargin(5);

		reload(wfl);
	}

	private void reload(final GUIWorkflow wfl) {
		this.workflow = wfl;

		cleanPanels();

		// Workflow section
		prepareWorkflowSection();

		// Task section
		prepareTaskSection();

		HLayout spacer = new HLayout();
		spacer.setHeight(5);

		IButton reassignButton = prepareReassignButton();

		IButton takeButton = prepareTakeButton();

		IButton turnBackButton = prepareTurnBackButton();

		IButton completionDiagram = prepareCompletionDiagramButton(wfl);

		if (workflow.getSelectedTask().getEndDate() == null) {
			buttonsPanel.addMember(spacer);
			if (readOnly) {
				buttonsPanel.addMember(completionDiagram);
			} else {
				buttonsPanel.addMember(reassignButton);
				buttonsPanel.addMember(takeButton);
				buttonsPanel.addMember(turnBackButton);
				buttonsPanel.addMember(spacer);
				buttonsPanel.addMember(completionDiagram);
			}

			if (workflow.getSelectedTask().getTaskState().equals(STARTED)
					&& workflow.getSelectedTask().getOwner() != null) {
				prepareTransitionButtons();
			}
		} else {
			DynamicForm taskEndedForm = new DynamicForm();
			taskEndedForm.setWidth(180);
			taskEndedForm.setColWidths(1, "*");

			StaticTextItem taskEndedTitle = ItemFactory.newStaticTextItem("taskEndedTitle", "",
					"<b>" + I18N.message("workflowtaskended") + "</b>");
			taskEndedTitle.setShouldSaveValue(false);
			taskEndedTitle.setWrapTitle(false);

			taskEndedForm.setItems(taskEndedTitle);

			buttonsPanel.addMember(spacer);
			buttonsPanel.addMember(taskEndedForm);

			buttonsPanel.addMember(spacer);
			buttonsPanel.addMember(completionDiagram);
		}

		form.addMember(sxLayout);

		workflowTab.setPane(form);

		refreshAppendedDocsTab();

		refreshNotesTab();

		refreshHistoryTab();
	}

	private void prepareTransitionButtons() {
		DynamicForm transitionsForm = new DynamicForm();
		transitionsForm.setWidth(150);
		transitionsForm.setIsGroup(true);
		transitionsForm.setGroupTitle(I18N.message("actions"));

		// Add Transitions buttons
		if (workflow.getSelectedTask().getTransitions() != null)
			for (GUITransition transition : workflow.getSelectedTask().getTransitions()) {
				final String transitionName = transition.getText();
				if (transitionName == null || transitionName.trim().isEmpty())
					continue;

				ButtonItem transitionButton = new ButtonItem(transition.getText());
				transitionButton.setAutoFit(true);
				transitionButton.addClickHandler(event -> {
					toggleTransitionButtons();
					if (workflow.getSelectedTask().isRequiresNote() || transition.isRequiresNote()) {
						/*
						 * This task requires a note at completion, so we
						 * collect the input from the user
						 */
						collectNoteAndEndTask(getWorkflow().getSelectedTask(), transition);
					} else {
						onEndTask(getWorkflow().getSelectedTask(), transitionName);
					}
				});
				transitionButtons.add(transitionButton);
			}
		transitionsForm.setItems(transitionButtons.toArray(new FormItem[0]));

		if (!readOnly)
			buttonsPanel.addMember(transitionsForm);
	}

	private void toggleTransitionButtons() {
		for (FormItem transitionButton : transitionButtons)
			transitionButton.setDisabled(!transitionButton.isDisabled());
	}

	private void prepareTaskSection() {
		DynamicForm taskForm = new DynamicForm();
		taskForm.setColWidths(60, "*");
		taskForm.setValuesManager(vm);

		StaticTextItem taskTitle = ItemFactory.newStaticTextItem("taskTitle", "",
				"<b>" + I18N.message("task") + "</b>");
		taskTitle.setWrapTitle(false);
		taskTitle.setShowTitle(false);

		StaticTextItem taskId = ItemFactory.newStaticTextItem("taskId", I18N.message("id"),
				workflow.getSelectedTask().getId());

		StaticTextItem taskName = ItemFactory.newStaticTextItem("taskName", I18N.message("name"),
				workflow.getSelectedTask().getName());
		if (workflow.getSelectedTask().getDisplay() != null)
			taskName.setValue("<span style='color:" + workflow.getSelectedTask().getDisplay() + "'>"
					+ workflow.getSelectedTask().getName() + "</span>");

		StaticTextItem taskDescription = ItemFactory.newStaticTextItem("taskDescription", I18N.message("description"),
				workflow.getSelectedTask().getDescription());
		taskDescription.setShouldSaveValue(false);

		StaticTextItem taskAssignee = ItemFactory.newStaticTextItem("taskAssignee", I18N.message("assignee"), "");
		if (workflow.getSelectedTask().getOwner() != null && !workflow.getSelectedTask().getOwner().trim().isEmpty())
			taskAssignee.setValue(
					Util.avatarWithText(workflow.getSelectedTask().getOwner(), workflow.getSelectedTask().getOwner()));
		else if (workflow.getSelectedTask().getPooledActors() != null
				&& !workflow.getSelectedTask().getPooledActors().trim().isEmpty())
			taskAssignee.setValue(workflow.getSelectedTask().getPooledActors());

		StaticTextItem taskStartDate = ItemFactory.newStaticTextItem("taskStartDate", "startdate", null);
		if (workflow.getSelectedTask().getStartDate() != null)
			taskStartDate.setValue(I18N.formatDate(workflow.getSelectedTask().getStartDate()));

		StaticTextItem taskDueDate = ItemFactory.newStaticTextItem("taskDueDate", "duedate", null);
		if (workflow.getSelectedTask().getDueDate() != null)
			taskDueDate.setValue(I18N.formatDate(workflow.getSelectedTask().getDueDate()));

		StaticTextItem taskEndDate = ItemFactory.newStaticTextItem("taskEndDate", "enddate", null);
		if (workflow.getSelectedTask().getEndDate() != null)
			taskEndDate.setValue(I18N.formatDate(workflow.getSelectedTask().getEndDate()));

		taskForm.setItems(taskTitle, taskId, taskName, taskDescription, taskAssignee, taskStartDate, taskDueDate,
				taskEndDate);

		sxLayout.addMember(taskForm);
	}

	private void prepareWorkflowSection() {
		DynamicForm workflowForm = new DynamicForm();
		workflowForm.setColWidths(60, "*");

		StaticTextItem workflowTitle = ItemFactory.newStaticTextItem("workflowTitle", "",
				"<b>" + I18N.message(WORKFLOW_STR) + "</b>");
		workflowTitle.setShouldSaveValue(false);
		workflowTitle.setWrapTitle(false);
		workflowTitle.setShowTitle(false);

		StaticTextItem workflowInstanceId = ItemFactory.newStaticTextItem("workflowInstanceId", I18N.message("id"),
				workflow.getId());
		workflowInstanceId.setShouldSaveValue(false);

		StaticTextItem workflowLabel = ItemFactory.newStaticTextItem("workflowLabel", I18N.message("name"),
				workflow.getLabel());
		workflowLabel.setShouldSaveValue(false);

		StaticTextItem workflowName = ItemFactory.newStaticTextItem("workflowName", I18N.message("workflowname"),
				workflow.getName());
		workflowName.setShouldSaveValue(false);

		StaticTextItem version = ItemFactory.newStaticTextItem("version", "" + workflow.getVersion());
		version.setShouldSaveValue(false);

		StaticTextItem workflowTag = ItemFactory.newStaticTextItem("tag",
				workflow.getTag() != null ? workflow.getTag() : "");

		StaticTextItem workflowDescription = ItemFactory.newStaticTextItem("workflowDescription",
				I18N.message("description"), workflow.getDescription());

		StaticTextItem startDate = ItemFactory.newStaticTextItem("startdate", null);
		if (workflow.getStartDate() != null)
			startDate.setValue(I18N.formatDate(workflow.getStartDate()));

		StaticTextItem endDate = ItemFactory.newStaticTextItem("enddate", null);
		if (workflow.getEndDate() != null)
			endDate.setValue(I18N.formatDate(workflow.getEndDate()));

		workflowForm.setItems(workflowTitle, workflowInstanceId, workflowLabel, workflowName, version, workflowTag,
				workflowDescription, startDate, endDate);
		sxLayout.addMember(workflowForm);
	}

	private IButton prepareCompletionDiagramButton(final GUIWorkflow wfl) {
		IButton completionDiagram = new IButton(I18N.message("completiondiagram"));
		completionDiagram.setAutoFit(true);
		completionDiagram.setMargin(2);
		completionDiagram.addClickHandler(event -> WorkflowService.Instance.get().getCompletionDiagram(wfl.getName(),
				wfl.getVersion(), wfl.getId(), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIWorkflow workflow) {
						new WorkflowPreview(workflow).show();
					}
				}));
		return completionDiagram;
	}

	private IButton prepareTurnBackButton() {
		IButton turnBackButton = new IButton(I18N.message("workflowtaskturnback"));
		turnBackButton.setAutoFit(true);
		turnBackButton.setMargin(2);
		turnBackButton.setVisible(!(workflow.getSelectedTask().getPooledActors() == null
				|| workflow.getSelectedTask().getPooledActors().isEmpty())
				&& !(workflow.getSelectedTask().getOwner() == null
						|| workflow.getSelectedTask().getOwner().trim().isEmpty()));
		turnBackButton.addClickHandler(event -> WorkflowService.Instance.get()
				.turnBackTaskToPool(workflow.getSelectedTask().getId(), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						WorkflowService.Instance.get().getWorkflowDetailsByTask(workflow.getSelectedTask().getId(),
								new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIWorkflow result) {
										destroy();
										TaskDetailsDialog.this.workflowDashboard.refresh(workflow.getId());
									}
								});
					}
				}));
		return turnBackButton;
	}

	private IButton prepareTakeButton() {
		IButton takeButton = new IButton(I18N.message("workflowtasktake"));
		takeButton.setAutoFit(true);
		takeButton.setMargin(2);
		takeButton.setVisible(!(workflow.getSelectedTask().getPooledActors() == null
				|| workflow.getSelectedTask().getPooledActors().isEmpty())
				&& (workflow.getSelectedTask().getOwner() == null
						|| workflow.getSelectedTask().getOwner().trim().isEmpty()));
		takeButton.addClickHandler(event -> WorkflowService.Instance.get().claimTask(workflow.getSelectedTask().getId(),
				Long.toString(Session.get().getUser().getId()), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIWorkflow result) {
						workflow = result;
						result.getSelectedTask().setOwner(Session.get().getUser().getUsername());
						reload(workflow);
						TaskDetailsDialog.this.workflowDashboard.refresh(workflow.getId());
					}
				}));
		return takeButton;
	}

	private IButton prepareReassignButton() {
		IButton reassignButton = new IButton(I18N.message("workflowtaskreassign"));
		reassignButton.setAutoFit(true);
		reassignButton.setMargin(2);
		reassignButton.addClickHandler(event -> {
			final Window window = new Window();
			window.setTitle(I18N.message("workflowtaskreassign"));
			window.setCanDragResize(true);
			window.setIsModal(true);
			window.setShowModalMask(true);
			window.setAutoSize(true);
			window.centerInPage();

			DynamicForm reassignUserForm = new DynamicForm();
			reassignUserForm.setTitleOrientation(TitleOrientation.TOP);
			reassignUserForm.setNumCols(1);
			reassignUserForm.setValuesManager(vm);
			user = ItemFactory.newUserSelector("user", I18N.message("user"), null, true, true);
			user.setWidth(250);
			user.setShowTitle(true);
			user.setDisplayField("username");

			SubmitItem saveButton = new SubmitItem("save", I18N.message("save"));
			saveButton.setAlign(Alignment.LEFT);
			saveButton.addClickHandler(saveClick -> {
				if (user.getSelectedRecord() == null)
					return;
				setUser(user.getSelectedRecord().getAttribute("id"));

				WorkflowService.Instance.get().reassignTask(workflow.getSelectedTask().getId(),
						user.getSelectedRecord().getAttribute("id"), new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow result) {
								if (result != null) {
									window.destroy();
									workflow = result;
									reload(workflow);
									workflowDashboard.refresh(workflow.getId());
								}
							}
						});
			});

			reassignUserForm.setItems(user, saveButton);

			window.addItem(reassignUserForm);
			window.show();
		});
		return reassignButton;
	}

	private void cleanPanels() {
		Canvas[] members = sxLayout.getMembers();
		for (Canvas canvas : members) {
			sxLayout.removeMember(canvas);
		}

		members = buttonsPanel.getMembers();
		for (Canvas canvas : members) {
			buttonsPanel.removeMember(canvas);
		}

		members = form.getMembers();
		for (Canvas canvas : members) {
			form.removeMember(canvas);
		}

		members = appendedDocsPanel.getMembers();
		for (Canvas canvas : members) {
			appendedDocsPanel.removeMember(canvas);
		}

		members = notesPanel.getMembers();
		for (Canvas canvas : members) {
			notesPanel.removeMember(canvas);
		}
	}

	private void refreshHistoryTab() {
		if (historyTab != null)
			tabs.removeTab(historyTab);
		historyTab = new Tab(I18N.message("history"));
		historyTab
				.setPane(new WorkflowHistoriesPanel(Long.parseLong(workflow.getId()), workflow.getTemplateId(), false));
		tabs.addTab(historyTab, 3);
	}

	private void refreshAppendedDocsTab() {
		if (docsTab != null)
			tabs.removeTab(docsTab);
		docsTab = new Tab(I18N.message("appendeddocuments"));
		tabs.addTab(docsTab, 1);
		prepareAppendedDocsPanel();
		docsTab.setPane(appendedDocsPanel);
	}

	private void refreshNotesTab() {
		if (notesTab != null)
			tabs.removeTab(notesTab);
		notesTab = new Tab(I18N.message("notes"));
		tabs.addTab(notesTab, 2);
		prepareNotesPanel();
		notesTab.setPane(notesPanel);
	}

	void refreshAndSelectNotesTab() {
		refreshNotesTab();
		tabs.selectTab(notesTab);
	}

	public void onNewNote() {
		refreshAndSelectNotesTab();
		workflowDashboard.refresh(workflow.getId());
	}

	private void prepareNotesPanel() {
		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField userId = new ListGridField(USER_ID, "userid", 50);
		userId.setHidden(true);

		ListGridField taskId = new ListGridField("taskId", "taskid", 50);
		taskId.setHidden(true);

		ListGridField task = new ListGridField("name", I18N.message("task"), 150);
		ListGridField usr = new UserListGridField("user", USER_ID, "user");
		ListGridField date = new DateListGridField("date", "date");

		ListGrid notesGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return new HTMLFlow("<div class='details'>"
						+ (rec.getAttributeAsString(COMMENT) != null ? rec.getAttributeAsString(COMMENT) : "")
						+ "</div>");
			}
		};

		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setWidth100();
		notesGrid.setHeight100();
		notesGrid.setCanFreezeFields(true);
		notesGrid.setShowHeader(true);
		notesGrid.setCanSelectAll(false);
		notesGrid.setCanExpandRecords(true);
		notesGrid.setSelectionType(SelectionStyle.SINGLE);
		notesGrid.setFields(id, task, date, usr);
		notesGrid.setDataSource(new WorkflowHistoriesDS(Long.parseLong(workflow.getId()), workflow.getTemplateId(),
				"event.workflow.task.note", null, null));
		notesPanel.addMember(notesGrid);

		Button addNote = new Button(I18N.message("addnote"));
		addNote.setAutoFit(true);
		addNote.addClickHandler(event -> new WorkflowNoteEditor(TaskDetailsDialog.this).show());

		if (!readOnly)
			notesPanel.addMember(addNote);

		// Expand all notes after arrived
		notesGrid.addDataArrivedHandler(event -> {
			for (ListGridRecord rec : notesGrid.getRecords()) {
				notesGrid.expandRecord(rec);
			}
		});

		notesGrid.addCellContextClickHandler(eevent -> {
			Menu contextMenu = new Menu();
			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.addClickHandler(evnt -> WorkflowService.Instance.get()
					.deleteNote(notesGrid.getSelectedRecord().getAttributeAsLong("id"), new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							refreshAndSelectNotesTab();
						}
					}));

			MenuItem print = new MenuItem();
			print.setTitle(I18N.message("print"));
			print.addClickHandler(clickEvent -> {
				HTMLPane printContainer = new HTMLPane();
				printContainer.setContents(notesGrid.getSelectedRecord().getAttribute(COMMENT));
				Canvas.printComponents(new Canvas[] { printContainer });
			});

			ListGridRecord[] selection = notesGrid.getSelectedRecords();

			if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
				delete.setEnabled(selection.length > 0);
			} else {
				long usrId = Long.parseLong(selection[0].getAttribute(USER_ID));
				delete.setEnabled(selection.length == 1 && usrId == Session.get().getUser().getId());
			}

			print.setEnabled(selection.length == 1);

			contextMenu.setItems(print, delete);
			contextMenu.showContextMenu();
			eevent.cancel();
		});
	}

	private void prepareAppendedDocsPanel() {
		FileNameListGridField docFilename = new FileNameListGridField();
		docFilename.setWidth("*");
		docFilename.setShowDefaultContextMenu(false);

		ListGridField docLastModified = new DateListGridField("lastModified", "lastmodified");
		docLastModified.setShowDefaultContextMenu(false);
		docLastModified.setHidden(false);

		ListGridField statusIcons = new StatusIconsListGridField();
		statusIcons.setCanFilter(false);
		statusIcons.setCanSort(false);
		statusIcons.setHidden(false);

		appendedDocs = new DocumentsListGrid();
		appendedDocs.setEmptyMessage(I18N.message("notitemstoshow"));
		appendedDocs.setWidth100();
		appendedDocs.setHeight100();
		appendedDocs.setCanFreezeFields(true);
		appendedDocs.setAutoFetchData(true);
		appendedDocs.setShowHeader(true);
		appendedDocs.setCanSelectAll(false);
		appendedDocs.setShowCellContextMenus(false);
		appendedDocs.setSelectionType(SelectionStyle.SINGLE);
		appendedDocs.setBorder("1px solid #E1E1E1");
		appendedDocs.setDataSource(
				new DocumentsDS(workflow.getAppendedDocIds() != null && !workflow.getAppendedDocIds().isEmpty()
						? workflow.getAppendedDocIds()
						: "empty"));
		appendedDocs.setFields(statusIcons, docFilename, docLastModified);

		appendedDocs.registerCellContextClickHandler(event -> {
			event.cancel();
			showAppendedDocsContextMenu();
		});
		appendedDocs.registerDoubleClickHandler((DoubleClickEvent eevent) -> {
			final ListGridRecord selection = appendedDocs.getSelectedRecord();
			FolderService.Instance.get().getFolder(selection.getAttributeAsLong("folderId"), false, false, false,
					new AsyncCallback<>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIFolder folder) {
							if (folder != null) {
								destroy();
								if (com.logicaldoc.gui.common.client.Menu
										.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS))
									DocumentsPanel.get().openInFolder(selection.getAttributeAsLong("id"));
							}
						}
					});
		});

		appendedDocsPanel.addMember(appendedDocs);

		Button addDocuments = new Button(I18N.message("adddocuments"));
		addDocuments.setAutoFit(true);
		addDocuments.setVisible(workflow.getSelectedTask().getTaskState().equals(STARTED));
		addDocuments.addClickHandler(evnt -> new DocumentSelectorDialog() {

			@Override
			protected void onSelection(List<GUIDocument> selection) {
				appendDocuments(selection);
				close();
			}
		}.show());

		Button addDocumentsFromClipboard = new Button(I18N.message("adddocumentsfromclipboard"));
		addDocumentsFromClipboard.setAutoFit(true);
		addDocumentsFromClipboard.setVisible(workflow.getSelectedTask().getTaskState().equals(STARTED));
		addDocumentsFromClipboard.addClickHandler(eevnt -> {
			Clipboard clipboard = Clipboard.getInstance();
			if (clipboard.isEmpty()) {
				SC.warn(I18N.message("nodocsinclipboard"));
				return;
			}

			appendDocuments(Clipboard.getInstance());
			Clipboard.getInstance().clear();
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setMembers(addDocuments, addDocumentsFromClipboard);

		if (workflow.getSelectedTask().getEndDate() == null && !readOnly) {
			appendedDocsPanel.addMember(buttons);
		}
	}

	private void appendDocuments(Collection<GUIDocument> documents) {
		WorkflowService.Instance.get().appendDocuments(workflow.getSelectedTask().getId(),
				documents.stream().map(d -> d.getId()).collect(Collectors.toList()), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						WorkflowService.Instance.get().getWorkflowDetailsByTask(workflow.getSelectedTask().getId(),
								new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIWorkflow result) {
										TaskDetailsDialog.this.workflow.setAppendedDocIds(result.getAppendedDocIds());
										refreshAppendedDocsTab();
										tabs.selectTab(1);
									}
								});
					}
				});
	}

	public GUIWorkflow getWorkflow() {
		return workflow;
	}

	public void setUser(String id) {
		user.setValue(id);
	}

	public TabSet getTabs() {
		return tabs;
	}

	/**
	 * Prepares the context menu for the documents grid.
	 */
	private void showAppendedDocsContextMenu() {
		final GUIDocument selectedDocument = appendedDocs.getSelectedDocument();

		FolderService.Instance.get().getFolder(selectedDocument.getFolder().getId(), false, false, false,
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder folder) {
						final Menu contextMenu = new Menu();

						final MenuItem preview = preparePreviewContextMenuItem(selectedDocument);

						final MenuItem download = prepareDownloadContextMenuItem(selectedDocument);

						final MenuItem open = prepareOpenInFolderContextMenuItem(selectedDocument);

						final MenuItem remove = prepareRemoveContextMenuItem(selectedDocument);

						final MenuItem checkout = prepareCheckoutContextMenuItem(selectedDocument);

						final MenuItem unlock = prepareUnlockContextMenuItem(selectedDocument);

						final MenuItem checkin = prepareCheckinContextMenuItem(selectedDocument);

						final MenuItem office = prepareOfficeContextMenuItem(selectedDocument);

						if (readOnly)
							contextMenu.setItems(preview, download, open);
						else if (Feature.visible(Feature.OFFICE))
							contextMenu.setItems(preview, download, checkout, checkin, unlock, office, open, remove);
						else
							contextMenu.setItems(preview, download, checkout, checkin, unlock, open, remove);

						FolderService.Instance.get().getFolder(selectedDocument.getFolder().getId(), false, false,
								false, new AsyncCallback<>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIFolder folder) {
										if (folder != null) {
											preview.setEnabled(com.logicaldoc.gui.common.client.Menu
													.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
											open.setEnabled(true);
											if (folder.isDownload())
												download.setEnabled(true);
											checkout.setEnabled(selectedDocument.getStatus() == Constants.DOC_UNLOCKED
													&& folder.isDownload() && folder.isWrite());
											checkin.setEnabled(selectedDocument.getStatus() == Constants.DOC_CHECKED_OUT
													&& folder.isWrite() && Session.get().getUser()
															.getId() == selectedDocument.getLockUserId());
											unlock.setEnabled(selectedDocument.getStatus() != Constants.DOC_UNLOCKED
													&& Session.get().getUser().getId() == selectedDocument
															.getLockUserId());
											office.setEnabled(Feature.enabled(Feature.OFFICE)
													&& folder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE)
													&& folder.hasPermission(GUIAccessControlEntry.PERMISSION_DOWNLOAD)
													&& Util.isOfficeFile(selectedDocument.getFileName()));
										}

										contextMenu.showContextMenu();
									}
								});
					}
				});

	}

	private MenuItem prepareCheckinContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem checkin = new MenuItem();
		checkin.setTitle(I18N.message("checkin"));
		checkin.addClickHandler(
				event -> DocumentService.Instance.get().getById(selectedDocument.getId(), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument document) {
						DocumentCheckin checkin = new DocumentCheckin(document, document.getFileName());
						checkin.show();
					}
				}));
		return checkin;
	}

	private MenuItem prepareUnlockContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem unlock = new MenuItem();
		unlock.setTitle(I18N.message("unlock"));
		unlock.addClickHandler(event -> DocumentService.Instance.get().unlock(Arrays.asList(selectedDocument.getId()),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument doc = appendedDocs.getSelectedDocument();
						DocUtil.markUnlocked(doc);
					}
				}));
		return unlock;
	}

	private MenuItem prepareCheckoutContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem checkout = new MenuItem();
		checkout.setTitle(I18N.message("checkout"));
		checkout.addClickHandler(event -> DocumentService.Instance.get()
				.checkout(Arrays.asList(selectedDocument.getId()), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument doc = appendedDocs.getSelectedDocument();
						DocUtil.markCheckedOut(doc);
						GuiLog.info(I18N.message("documentcheckedout"), null);

						WindowUtils.openUrl(Util.downloadURL(selectedDocument.getId()));
					}
				}));
		return checkout;
	}

	private MenuItem prepareOfficeContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem office = new MenuItem(I18N.message("editwithoffice"));
		office.addClickHandler(event -> Util.openEditWithOffice(selectedDocument.getId()));
		return office;
	}

	private MenuItem prepareRemoveContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem remove = new MenuItem();
		remove.setTitle(I18N.message("remove"));
		remove.addClickHandler(event -> WorkflowService.Instance.get()
				.removeDocument(workflow.getSelectedTask().getId(), selectedDocument.getId(), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						appendedDocs.removeSelectedData();
					}
				}));
		return remove;
	}

	private MenuItem prepareOpenInFolderContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem open = new MenuItem();
		open.setTitle(I18N.message("openinfolder"));
		open.setEnabled(false);
		open.addClickHandler(event -> {
			destroy();

			if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS))
				DocumentsPanel.get().openInFolder(selectedDocument.getFolder().getId(), selectedDocument.getId());
		});
		return open;
	}

	private MenuItem prepareDownloadContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.setEnabled(false);
		download.addClickHandler(event -> DocUtil.download(selectedDocument.getId(), null));
		return download;
	}

	private MenuItem preparePreviewContextMenuItem(final GUIDocument selectedDocument) {
		final MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.setEnabled(false);
		preview.addClickHandler(
				event -> DocumentService.Instance.get().getById(selectedDocument.getId(), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				}));
		return preview;
	}

	/**
	 * Gets the required note to the user before terminating the task taking the
	 * given transition
	 * 
	 * @param task the task to end
	 * @param transition the transition to take
	 */
	private void collectNoteAndEndTask(final GUIWFState task, final GUITransition transition) {
		RichTextItem noteInput = ItemFactory.newRichTextItemForNote("note", "note", null);
		noteInput.setWidth("*");
		noteInput.setHeight(200);
		noteInput.setShowErrorText(true);
		noteInput.setValidators(new CustomValidator() {

			@Override
			protected boolean condition(Object value) {
				if (value == null || value.toString().length() < 1) {
					setErrorMessage(I18N.message("fieldrequired"));
					return false;
				}

				int maxlength = Session.get().getConfigAsInt("default.gui.note.maxlength");
				String errorMessage = I18N.message("contentexceedsmax", Integer.toString(maxlength));
				setErrorMessage(errorMessage);
				if (value.toString().length() > maxlength)
					return false;

				int minlength = task.getMinNoteSize() != null ? task.getMinNoteSize() : 0;
				if (transition.getMinNoteSize() != null && transition.getMinNoteSize() > minlength)
					minlength = transition.getMinNoteSize();
				errorMessage = I18N.message("notetoosmall", Integer.toString(minlength));
				setErrorMessage(errorMessage);
				return value.toString().length() >= minlength;
			}
		});

		LD.askForValue("providenotetocomplete", "note", null, noteInput, 500, value -> {
			if (Boolean.TRUE.equals(noteInput.validate()))
				WorkflowService.Instance.get().addNote(workflow.getSelectedTask().getId(), transition.getText(), value,
						new AsyncCallback<Long>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
								toggleTransitionButtons();
							}

							@Override
							public void onSuccess(Long noteId) {
								destroy();
								onEndTask(getWorkflow().getSelectedTask(), transition.getText());
							}
						});
		}, event -> toggleTransitionButtons());
	}

	/**
	 * Ends the task taking the specified transition
	 * 
	 * @param task the task to end
	 * @param transition name of the transition to take
	 */
	private void onEndTask(GUIWFState task, String transition) {
		WorkflowService.Instance.get().endTask(task.getId(), transition, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				toggleTransitionButtons();
			}

			@Override
			public void onSuccess(Void result) {
				TaskDetailsDialog.this.workflowDashboard.refresh(workflow.getId());
				destroy();
			}
		});
	}
}