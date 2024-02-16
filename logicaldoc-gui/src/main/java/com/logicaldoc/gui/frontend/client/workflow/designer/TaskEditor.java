package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This is the form used for the workflow task settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TaskEditor extends Window {

	private static final String AVATAR = "avatar";

	private static final String LABEL = "label";

	private ValuesManager vm = new ValuesManager();

	private GUIWFState state;

	private ListGrid participantsGrid;

	private StateWidget widget;

	public TaskEditor(StateWidget widget) {
		this.state = widget.getWFState();
		this.widget = widget;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("editworkflowstate",
				state.getType() == GUIWFState.TYPE_TASK ? I18N.message("task") : I18N.message("endstate")));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(600);
		setHeight(600);
		centerInPage();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTab.setPane(preparePropertiesPanel());

		Tab automationTab = new Tab(I18N.message("automation"));
		automationTab.setPane(prepareAutomationPanel());

		Tab messagesTab = new Tab(I18N.message("messages"));
		messagesTab.setPane(prepareMessagesPanel());

		TabSet tabSet = new TabSet();
		tabSet.setWidth100();
		tabSet.setTabs(propertiesTab, automationTab, messagesTab);
		addItem(tabSet);

		Button save = new Button(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> {
			if (Boolean.TRUE.equals(vm.validate())) {
				onSave();
				destroy();
			}
		});
		save.setMargin(3);

		save.setDisabled(!widget.getDrawingPanel().getWorkflowDesigner().getWorkflow().isLatestVersion());

		addItem(save);
	}

	private VLayout prepareMessagesPanel() {
		VLayout messagesPanel = new VLayout();
		messagesPanel.setWidth100();
		messagesPanel.setHeight100();

		DynamicForm messagesForm = new DynamicForm();
		messagesForm.setTitleOrientation(TitleOrientation.TOP);
		messagesForm.setNumCols(2);
		messagesForm.setTitleOrientation(TitleOrientation.LEFT);
		messagesForm.setValuesManager(vm);

		SelectItem creationMessageTemplate = ItemFactory.newSelectItem("creationMessageTemplate",
				state.getType() == GUIWFState.TYPE_TASK ? "messageontaskreached" : "messageonendstatusreached");
		creationMessageTemplate.setWrapTitle(false);

		SelectItem assignmentMessageTemplate = ItemFactory.newSelectItem("assignmentMessageTemplate",
				"messageontaskassignment");
		assignmentMessageTemplate.setWrapTitle(false);

		SelectItem reminderMessageTemplate = ItemFactory.newSelectItem("reminderMessageTemplate", "messageonremind");
		reminderMessageTemplate.setWrapTitle(false);

		SelectItem completionMessageTemplate = ItemFactory.newSelectItem("completionMessageTemplate",
				"messageontaskcompleted");
		completionMessageTemplate.setWrapTitle(false);

		if (state.getType() == GUIWFState.TYPE_TASK)
			messagesForm.setItems(creationMessageTemplate, assignmentMessageTemplate, reminderMessageTemplate,
					completionMessageTemplate);
		else
			messagesForm.setItems(creationMessageTemplate);

		messagesPanel.addMember(messagesForm);

		MessageService.Instance.get().loadTemplates(I18N.getLocale(), "user", new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIMessageTemplate> templates) {
				LinkedHashMap<String, String> map = new LinkedHashMap<>();
				map.put("", "");
				for (GUIMessageTemplate t : templates)
					map.put("" + t.getName(), t.getName());

				creationMessageTemplate.setValueMap(map);
				creationMessageTemplate.setValue("");
				creationMessageTemplate.setValue(state.getCreationMessageTemplate());

				completionMessageTemplate.setValueMap(map);
				completionMessageTemplate.setValue("");
				completionMessageTemplate.setValue(state.getCompletionMessageTemplate());

				assignmentMessageTemplate.setValueMap(map);
				assignmentMessageTemplate.setValue("");
				assignmentMessageTemplate.setValue(state.getAssignmentMessageTemplate());

				reminderMessageTemplate.setValueMap(map);
				reminderMessageTemplate.setValue("");
				reminderMessageTemplate.setValue(state.getReminderMessageTemplate());
			}
		});

		return messagesPanel;
	}

	private VLayout prepareAutomationPanel() {
		VLayout automationPanel = new VLayout();
		automationPanel.setWidth100();
		automationPanel.setHeight100();

		DynamicForm automationForm = new DynamicForm();
		automationForm.setTitleOrientation(TitleOrientation.TOP);
		automationForm.setNumCols(1);
		automationForm.setValuesManager(vm);

		TextAreaItem onCreation = ItemFactory.newTextAreaItemForAutomation("onCreation",
				state.getType() == GUIWFState.TYPE_TASK ? "execscriptontaskreached" : "execscriptonenstatusreached",
				state.getOnCreation(), null, false);
		onCreation.setWidth("*");
		onCreation.setHeight(125);
		onCreation.setWrapTitle(false);

		TextAreaItem onAssignment = ItemFactory.newTextAreaItemForAutomation("onAssignment",
				"execscriptontaskassignment", state.getOnAssignment(), null, false);
		onAssignment.setWidth("*");
		onAssignment.setHeight(125);
		onAssignment.setWrapTitle(false);

		TextAreaItem onCompletion = ItemFactory.newTextAreaItemForAutomation("onCompletion",
				"execscriptontaskcompletion", state.getOnCompletion(), null, false);
		onCompletion.setWidth("*");
		onCompletion.setHeight(125);
		onCompletion.setWrapTitle(false);

		if (state.getType() == GUIWFState.TYPE_TASK)
			automationForm.setItems(onCreation, onAssignment, onCompletion);
		else {
			onCreation.setHeight(400);
			automationForm.setItems(onCreation);
		}

		automationPanel.addMember(automationForm);

		return automationPanel;
	}

	// Checks if the task requires human interaction
	private boolean isHumanInteraction() {
		try {
			return !(state.getParticipants().size() == 1
					&& state.getParticipants().get(0).getCode().equals("_workflow"));
		} catch (Exception t) {
			return true;
		}
	}

	private VLayout preparePropertiesPanel() {
		DynamicForm taskForm = new DynamicForm();
		taskForm.setTitleOrientation(TitleOrientation.TOP);
		taskForm.setNumCols(2);
		taskForm.setValuesManager(vm);
		TextItem taskName = ItemFactory.newTextItem("taskName", "name", this.state.getName());
		taskName.setRequired(true);
		taskName.setWidth(220);

		ColorPickerItem taskColor = ItemFactory.newColorPickerItem("taskColor", "color", this.state.getDisplay(), true,
				null);

		TextAreaItem taskDescr = ItemFactory.newTextAreaItem("taskDescr", "description", this.state.getDescription());
		taskDescr.setWidth("*");
		taskDescr.setHeight(50);
		taskDescr.setWrapTitle(false);
		taskDescr.setColSpan(2);

		boolean isHumanInteraction = isHumanInteraction();

		VLayout propertiesPanel = new VLayout();
		propertiesPanel.setWidth100();
		propertiesPanel.setHeight100();
		propertiesPanel.addMember(taskForm);

		// The vertical panel that contains the participants
		VLayout participantsPanel = new VLayout();
		propertiesPanel.addMember(participantsPanel);

		// Horizontal panel that contains the forms related to the participants
		HLayout formsPanel = new HLayout();
		formsPanel.setMembersMargin(5);
		formsPanel.setHeight(70);

		RadioGroupItem humanInteraction = ItemFactory.newBooleanSelector("humanInteraction", "humaninteraction");
		humanInteraction.setDefaultValue(isHumanInteraction ? "yes" : "no");
		humanInteraction.setValue(isHumanInteraction);
		humanInteraction.addChangedHandler((ChangedEvent event) -> {
			if ("yes".equals(event.getValue())) {
				participantsPanel.show();
			} else
				participantsPanel.hide();
		});

		if (state.getType() == GUIWFState.TYPE_TASK) {
			taskForm.setNumCols(3);
			taskForm.setFields(taskName, taskColor, humanInteraction, taskDescr);
		} else
			taskForm.setFields(taskName, taskColor, taskDescr);
		participantsPanel.addMembers(formsPanel);

		addTaskItems(formsPanel);

		HTMLPane spacer = new HTMLPane();
		spacer.setHeight(2);
		spacer.setMargin(2);
		spacer.setOverflow(Overflow.HIDDEN);
		participantsPanel.addMember(spacer);

		VLayout addUsersAndGroupsPanel = new VLayout();
		addUsersAndGroupsPanel.setMargin(3);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL));
		label.setWidth("*");
		label.setCanFilter(false);

		ListGridField name = new ListGridField("name", I18N.message("name"), 50);
		name.setCanFilter(false);
		name.setHidden(true);

		UserListGridField avatar = new UserListGridField();
		participantsGrid = new RefreshableListGrid();
		participantsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		participantsGrid.setCanFreezeFields(true);
		participantsGrid.setAutoFetchData(true);
		participantsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		participantsGrid.setFilterOnKeypress(true);
		participantsGrid.setShowFilterEditor(false);
		participantsGrid.setShowHeader(false);
		participantsGrid.setFields(name, avatar, label);
		participantsGrid.addCellContextClickHandler(click -> {
			Menu contextMenu = new Menu();
			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.addClickHandler(itemClick -> participantsGrid.removeSelectedData());

			contextMenu.setItems(delete);
			contextMenu.showContextMenu();
			click.cancel();
		});

		SectionStackSection participantsSection = new SectionStackSection(I18N.message("participants"));
		participantsSection.setCanCollapse(false);
		participantsSection.setExpanded(true);
		participantsSection.setItems(participantsGrid);
		SectionStack participantsStack = new SectionStack();
		participantsStack.setWidth100();
		participantsStack.setHeight(220);
		participantsStack.setSections(participantsSection);

		addUsersAndGroupsPanel.addMember(participantsStack);
		addUsersAndGroupsPanel.addMember(prepareAddParticipantsForm());
		participantsPanel.addMember(addUsersAndGroupsPanel);

		initParticipantsList();

		if (isHumanInteraction)
			participantsPanel.show();
		else
			participantsPanel.hide();

		return propertiesPanel;
	}

	private DynamicForm prepareAddParticipantsForm() {
		// Prepare the combo and button for adding a new user
		DynamicForm participantsEditForm = new DynamicForm();
		participantsEditForm.setTitleOrientation(TitleOrientation.LEFT);
		participantsEditForm.setNumCols(6);
		participantsEditForm.setWidth(1);

		SelectItem addUser = prepareAddUserSelector();

		SelectItem addGroup = prepareAddGroupSelector();

		// Prepare dynamic user participant
		final TextItem addAttribute = prepareAddAttributeItem();

		participantsEditForm.setItems(addUser, addGroup, addAttribute);
		return participantsEditForm;
	}

	private TextItem prepareAddAttributeItem() {
		final TextItem addAttribute = ItemFactory.newTextItem("attribute", "addattribute", null);
		addAttribute.setWidth(110);
		FormItemIcon addIcon = ItemFactory.newItemIcon("add.png");
		addIcon.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			String val = addAttribute.getValueAsString();
			if (val != null)
				val = val.trim();
			if (val == null || "".equals(val))
				return;

			// Check if the digited attribute user is already present in the
			// participants list
			if (participantsGrid.find(new AdvancedCriteria("name", OperatorId.EQUALS, "att." + val)) != null)
				return;
			else
				addParticipant("att." + val, val);
			addAttribute.clearValue();
		});
		addAttribute.setIcons(addIcon);
		return addAttribute;
	}

	private SelectItem prepareAddGroupSelector() {
		SelectItem addGroup = ItemFactory.newGroupSelector("group", "addgroup");
		addGroup.setWidth(110);
		addGroup.setRequired(false);
		addGroup.addChangedHandler(event -> {
			if (event.getValue() != null && !"".equals(event.getValue())) {
				final ListGridRecord selectedRecord = addGroup.getSelectedRecord();
				if (selectedRecord == null)
					return;

				// Check if the selected user is already present in the
				// participants list
				if (participantsGrid.find(new AdvancedCriteria("name", OperatorId.EQUALS,
						"g." + selectedRecord.getAttribute("name"))) != null)
					return;
				else
					addParticipant("g." + selectedRecord.getAttribute("name"), selectedRecord.getAttribute("name"));
				addGroup.clearValue();
			}
		});
		return addGroup;
	}

	private SelectItem prepareAddUserSelector() {
		SelectItem addUser = ItemFactory.newUserSelector("user", "adduser", null, false, false);
		addUser.setWidth(110);
		addUser.setRequired(false);
		addUser.addChangedHandler(event -> {
			if (event.getValue() != null && !"".equals(event.getValue())) {
				final ListGridRecord selectedRecord = addUser.getSelectedRecord();
				if (selectedRecord == null)
					return;

				// Check if the selected user is already present in the
				// rights table
				if (participantsGrid.find(new AdvancedCriteria("name", OperatorId.EQUALS,
						selectedRecord.getAttribute("username"))) != null)
					return;
				else
					addParticipant(selectedRecord.getAttribute("username"), selectedRecord.getAttribute(LABEL));
				addUser.clearValue();
			}
		});
		return addUser;
	}

	private void addTaskItems(HLayout formsPanel) {
		if (state.getType() == GUIWFState.TYPE_TASK) {
			SpinnerItem duedateTimeItem = ItemFactory.newSpinnerItem("duedateNumber", "duedate",
					this.state.getDueDateNumber());
			duedateTimeItem.setWrapTitle(false);
			duedateTimeItem.setDefaultValue(0);

			SelectItem duedateTime = ItemFactory.newDueTimeSelector("duedateTime", "");
			duedateTime.setWrapTitle(false);
			duedateTime.setValue(this.state.getDueDateUnit());

			SpinnerItem remindTimeItem = ItemFactory.newSpinnerItem("remindtimeNumber", "remindtime",
					this.state.getReminderNumber());
			remindTimeItem.setDefaultValue(0);
			remindTimeItem.setWrapTitle(false);

			SelectItem remindTime = ItemFactory.newDueTimeSelector("remindTime", "");
			remindTime.setWrapTitle(false);
			remindTime.setValue(this.state.getReminderUnit());
			if (Session.get().isDemo()) {
				// In demo mode disable the remind setting because of this may
				// send massive emails
				remindTimeItem.setDisabled(true);
				remindTime.setDisabled(true);
			}

			DynamicForm escalationForm = new DynamicForm();
			escalationForm.setGroupTitle(I18N.message("escalationmanagement"));
			escalationForm.setIsGroup(true);
			escalationForm.setTitleOrientation(TitleOrientation.LEFT);
			escalationForm.setNumCols(4);
			escalationForm.setColWidths("35", "35", "50", "130");
			escalationForm.setValuesManager(vm);
			escalationForm.setFields(duedateTimeItem, duedateTime, remindTimeItem, remindTime);

			RadioGroupItem requiresNote = ItemFactory.newBooleanSelector("requiresNote", "requirenoteatcompletion");
			requiresNote.setWrapTitle(false);
			requiresNote.setDefaultValue(this.state.isRequiresNote() ? "yes" : "no");

			SpinnerItem minNoteSize = ItemFactory.newSpinnerItem("minnotesize",
					this.state.getMinNoteSize() != null && this.state.getMinNoteSize() > 0 ? this.state.getMinNoteSize()
							: null);
			minNoteSize.setMin(0);
			minNoteSize.setStep(10);
			if (Session.get().getConfig("gui.note.maxlength") != null)
				minNoteSize.setMax(Session.get().getConfigAsInt("gui.note.maxlength"));
			minNoteSize.setHint(I18N.message("chars").toLowerCase());
			minNoteSize.setWrapTitle(false);

			DynamicForm mandatoryNoteForm = new DynamicForm();
			mandatoryNoteForm.setGroupTitle(I18N.message("noteatcompletion"));
			mandatoryNoteForm.setIsGroup(true);
			mandatoryNoteForm.setTitleOrientation(TitleOrientation.LEFT);
			mandatoryNoteForm.setNumCols(2);
			mandatoryNoteForm.setValuesManager(vm);
			mandatoryNoteForm.setFields(requiresNote, minNoteSize);

			formsPanel.setMembers(escalationForm, mandatoryNoteForm);
		}
	}

	private void initParticipantsList() {
		// Initialize the participants list
		try {
			if (this.state.getParticipants() != null) {
				ArrayList<ListGridRecord> records = new ArrayList<>();

				for (GUIValue part : this.state.getParticipants()) {
					if (part.getCode() == null || part.getValue() == null)
						continue;

					ListGridRecord rec = createParticipantRecord(part.getCode(), part.getValue());
					records.add(rec);
				}

				if (!records.isEmpty())
					participantsGrid.setRecords(records.toArray(new ListGridRecord[0]));
			}
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private ListGridRecord createParticipantRecord(String name, String label) {
		ListGridRecord rec = new ListGridRecord();
		rec.setAttribute("name", name);
		rec.setAttribute(LABEL, label);
		if (name.startsWith("g."))
			rec.setAttribute(AVATAR, "group");
		else if (name.startsWith("att."))
			rec.setAttribute(AVATAR, "attribute");
		else
			rec.setAttribute(AVATAR, name);
		return rec;
	}

	/**
	 * Refresh the task's users participants list.
	 */
	private void addParticipant(String entityCode, String entityLabel) {
		if (entityCode != null && entityLabel != null)
			participantsGrid.getDataAsRecordList().add(createParticipantRecord(entityCode, entityLabel));
	}

	@SuppressWarnings("unchecked")
	private void onSave() {
		Map<String, Object> values = vm.getValues();
		boolean humanInteraction = "yes".equals(values.get("humanInteraction"));

		if (Boolean.FALSE.equals(vm.validate()) && humanInteraction)
			return;

		// Remove the ' because of the WF engine would go in error saving into
		// the DB
		TaskEditor.this.state.setName(values.get("taskName").toString().trim().replace("'", ""));
		TaskEditor.this.state.setDisplay((String) values.get("taskColor"));
		TaskEditor.this.state.setDescription((String) values.get("taskDescr"));
		TaskEditor.this.state.setOnCreation((String) values.get("onCreation"));
		TaskEditor.this.widget.setContents("<b>" + state.getName() + "</b>");
		TaskEditor.this.widget.getDrawingPanel().getDiagramController().update();
		TaskEditor.this.state.setCreationMessageTemplate((String) values.get("creationMessageTemplate"));

		if (state.getType() == GUIWFState.TYPE_TASK) {
			TaskEditor.this.state.setRequiresNote("yes".equals(values.get("requiresNote")));
			TaskEditor.this.state.setMinNoteSize((Integer) values.get("minnotesize"));
			TaskEditor.this.state.setDueDateNumber((Integer) values.get("duedateNumber"));
			TaskEditor.this.state.setDueDateUnit((String) values.get("duedateTime"));
			TaskEditor.this.state.setReminderNumber((Integer) values.get("remindtimeNumber"));
			TaskEditor.this.state.setReminderUnit((String) values.get("remindTime"));
			TaskEditor.this.state.setOnAssignment((String) values.get("onAssignment"));
			TaskEditor.this.state.setOnCompletion((String) values.get("onCompletion"));
			TaskEditor.this.state.setAssignmentMessageTemplate((String) values.get("assignmentMessageTemplate"));
			TaskEditor.this.state.setReminderMessageTemplate((String) values.get("reminderMessageTemplate"));
			TaskEditor.this.state.setCompletionMessageTemplate((String) values.get("completionMessageTemplate"));

			if (!humanInteraction) {
				participantsGrid.getRecordList().removeList(participantsGrid.getRecordList().toArray());
				participantsGrid.getRecordList().add(createParticipantRecord("_workflow", "Workflow Engine"));
			}
		}

		ArrayList<GUIValue> participants = new ArrayList<>();
		for (ListGridRecord rec : participantsGrid.getRecords())
			participants.add(new GUIValue(rec.getAttributeAsString("name"), rec.getAttributeAsString(LABEL)));
		TaskEditor.this.state.setParticipants(participants);

		if (humanInteraction && state.getType() == GUIWFState.TYPE_TASK
				&& TaskEditor.this.state.getParticipants().isEmpty())
			SC.warn(I18N.message("workflowtaskparticipantatleast"));
	}
}