package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This is the form used for the workflow task settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TaskEditor extends Window {

	private ValuesManager vm = new ValuesManager();

	private GUIWFState state;

	private SelectItem participantsList;

	private LinkedHashMap<String, String> participants = new LinkedHashMap<String, String>();

	private HLayout participantsListLayout;

	private DynamicForm participantsForm;

	private Button removeParticipant = null;

	private StateWidget widget;

	public TaskEditor(StateWidget widget) {
		this.state = widget.getWfState();
		this.widget = widget;
		participants.clear();

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("editworkflowstate",
				state.getType() == GUIWFState.TYPE_TASK ? I18N.message("task") : I18N.message("endstate")));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(600);
		setHeight(560);
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
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (vm.validate()) {
					onSave();
					destroy();
				}
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

		MessageService.Instance.get().loadTemplates(I18N.getLocale(), "user",
				new AsyncCallback<GUIMessageTemplate[]>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIMessageTemplate[] templates) {
						LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
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

	// Checks if the tast requires human interaction
	private boolean isHumanInteraction() {
		try {
			GUIValue[] parts = this.state.getParticipants();
			return !(parts != null && parts.length == 1 && parts[0].getCode().equals("_workflow"));
		} catch (Throwable t) {
			return true;
		}
	}

	private VLayout preparePropertiesPanel() {
		VLayout propertiesPanel = new VLayout();
		propertiesPanel.setWidth100();
		propertiesPanel.setHeight100();

		DynamicForm taskForm = new DynamicForm();
		taskForm.setTitleOrientation(TitleOrientation.TOP);
		taskForm.setNumCols(2);
		taskForm.setValuesManager(vm);
		TextItem taskName = ItemFactory.newTextItem("taskName", "name", this.state.getName());
		taskName.setRequired(true);
		taskName.setWidth(220);

		ColorPickerItem taskColor = ItemFactory.newColorItemPicker("taskColor", "color", this.state.getDisplay(), true,
				null);

		TextAreaItem taskDescr = ItemFactory.newTextAreaItem("taskDescr", "description", this.state.getDescription());
		taskDescr.setWidth("*");
		taskDescr.setHeight(50);
		taskDescr.setWrapTitle(false);
		taskDescr.setColSpan(2);

		boolean isHumanInteraction = isHumanInteraction();

		RadioGroupItem humanInteraction = ItemFactory.newBooleanSelector("humanInteraction", "humaninteraction");
		humanInteraction.setValue(isHumanInteraction ? "yes" : "no");
		humanInteraction.setDefaultValue(isHumanInteraction ? "yes" : "no");

		if (state.getType() == GUIWFState.TYPE_TASK) {
			taskForm.setNumCols(3);
			taskForm.setFields(taskName, taskColor, humanInteraction, taskDescr);
		} else
			taskForm.setFields(taskName, taskColor, taskDescr);

		propertiesPanel.addMember(taskForm);

		// The vertical panel that contains the participants
		final VLayout participantsPanel = new VLayout();
		propertiesPanel.addMember(participantsPanel);

		// Horizontal panel that contains the forms related to the participants
		HLayout formsPanel = new HLayout();
		formsPanel.setMembersMargin(5);
		participantsPanel.addMembers(formsPanel);

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

			SpinnerItem minNoteSize = ItemFactory.newSpinnerItem("minNoteSize", "minnotesize",
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

		HTMLPane spacer = new HTMLPane();
		spacer.setHeight(2);
		spacer.setMargin(2);
		spacer.setOverflow(Overflow.HIDDEN);
		participantsPanel.addMember(spacer);

		final DynamicForm participantsForm = new DynamicForm();
		participantsForm.setTitleOrientation(TitleOrientation.TOP);
		participantsForm.setNumCols(1);
		StaticTextItem participantsItem = ItemFactory.newStaticTextItem("participants", "",
				"<b>" + I18N.message("participants") + "</b>");
		participantsItem.setShouldSaveValue(false);
		participantsItem.setShowTitle(false);
		participantsItem.setWrapTitle(false);
		participantsItem.setRequired(true);
		participantsForm.setItems(participantsItem);
		participantsPanel.addMember(participantsForm);

		HLayout usergroupSelection = new HLayout();
		usergroupSelection.setHeight(25);
		usergroupSelection.setMargin(3);

		// Prepare the combo and button for adding a new user
		final DynamicForm usergroupForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "user", null, true, false);
		user.setRequired(true);
		user.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() != null && !"".equals((String) event.getValue())) {
					final ListGridRecord selectedRecord = user.getSelectedRecord();
					if (selectedRecord == null)
						return;

					// Check if the selected user is already present in the
					// rights table
					for (String participant : participantsList.getValues()) {
						if (participant.equals(selectedRecord.getAttribute("username"))) {
							return;
						}
					}

					if (participants.get(selectedRecord.getAttribute("username")) == null)
						addParticipant(selectedRecord.getAttribute("username"), selectedRecord.getAttribute("label"));
					user.clearValue();
				}
			}
		});

		final SelectItem group = ItemFactory.newGroupSelector("group", "group");
		group.setRequired(true);
		group.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() != null && !"".equals((String) event.getValue())) {
					final ListGridRecord selectedRecord = group.getSelectedRecord();
					if (selectedRecord == null)
						return;

					// Check if the selected user is already present in the
					// participants list
					for (String participant : participantsList.getValues()) {
						if (participant.equals("g." + selectedRecord.getAttribute("name"))) {
							return;
						}
					}

					if (participants.get("g." + selectedRecord.getAttribute("name")) == null)
						addParticipant("g." + selectedRecord.getAttribute("name"), selectedRecord.getAttribute("name"));
					user.clearValue();
				}
			}
		});

		// Prepare dynamic user participant
		final TextItem attr = ItemFactory.newTextItem("attribute", "attribute", null);
		FormItemIcon addIcon = ItemFactory.newItemIcon("add.png");
		addIcon.addFormItemClickHandler(new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				String val = attr.getValueAsString();
				if (val != null)
					val = val.trim();
				if (val == null || "".equals(val))
					return;

				// Check if the digited attribute user is already present in the
				// participants list
				for (String participant : participantsList.getValues()) {
					if (participant.equals("att." + val)) {
						return;
					}
				}

				if (participants.get("att." + val) == null)
					addParticipant("att." + val, val);
				attr.clearValue();
			}
		});
		attr.setIcons(addIcon);

		usergroupForm.setItems(user, group, attr);
		usergroupSelection.addMember(usergroupForm);
		participantsPanel.addMember(usergroupSelection);

		participantsListLayout = new HLayout();
		participantsListLayout.setHeight(100);
		participantsListLayout.setMembersMargin(5);
		participantsPanel.addMember(participantsListLayout);

		// Initialize the participants list
		try {
			if (this.state.getParticipants() != null)
				for (GUIValue part : this.state.getParticipants()) {
					if (part.getCode() == null || part.getValue() == null)
						continue;
					String prefix = I18N.message("user");
					if (part.getCode().startsWith("g."))
						prefix = I18N.message("group");
					else if (part.getCode().startsWith("att."))
						prefix = I18N.message("attribute");
					prefix += ": ";

					participants.put(part.getCode(),
							part.getValue().startsWith(prefix) ? part.getValue() : prefix + part.getValue());
				}
		} catch (Throwable t) {

		}

		addParticipant(null, null);

		if (isHumanInteraction)
			participantsPanel.show();
		else
			participantsPanel.hide();

		humanInteraction.setValue(isHumanInteraction);
		humanInteraction.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if ("yes".equals(event.getValue())) {
					participants.clear();
					TaskEditor.this.state.setParticipants(new GUIValue[0]);
					participantsList.setValueMap(participants);
					participantsPanel.show();
				} else
					participantsPanel.hide();

			}
		});

		return propertiesPanel;
	}

	/**
	 * Refresh the task's users participants list. If <code>operation</code> is
	 * 0, no operation is made to the list.
	 */
	private void addParticipant(String entityCode, String entityLabel) {
		if (participantsForm != null)
			participantsListLayout.removeMember(participantsForm);
		if (removeParticipant != null)
			participantsListLayout.removeMember(removeParticipant);

		participantsForm = new DynamicForm();
		participantsForm.setTitleOrientation(TitleOrientation.TOP);
		participantsForm.setNumCols(1);
		participantsForm.setValuesManager(vm);

		if (entityCode != null) {
			String prefix = I18N.message("user");
			if (entityCode.startsWith("g."))
				prefix = I18N.message("group");
			else if (entityCode.startsWith("att."))
				prefix = I18N.message("attribute");
			prefix += ": ";

			participants.put(entityCode.trim(), entityLabel.startsWith(prefix) ? entityLabel : prefix + entityLabel);
		}

		participantsList = new SelectItem();
		participantsList.setTitle("<b>" + I18N.message("participants") + "</b>");
		participantsList.setShowTitle(false);
		participantsList.setMultipleAppearance(MultipleAppearance.GRID);
		participantsList.setMultiple(true);
		participantsList.setWidth(350);
		participantsList.setHeight(130);
		participantsList.setEndRow(true);
		participantsList.setValueMap(participants);
		participantsForm.setItems(participantsList);

		removeParticipant = new Button(I18N.message("remove"));
		removeParticipant.setAutoFit(true);
		removeParticipant.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				@SuppressWarnings("unchecked")
				List<String> selection = (List<String>) participantsList.getValue();
				for (String key : selection) {
					participants.remove(key);
					participantsList.setValueMap(participants);
				}
			}
		});

		participantsListLayout.setMembers(participantsForm, removeParticipant);
	}

	@SuppressWarnings("unchecked")
	private void onSave() {
		vm.validate();
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		boolean humanInteraction = "yes".equals(values.get("humanInteraction"));

		if (!vm.validate() && humanInteraction)
			return;

		TaskEditor.this.state.setName((String) values.get("taskName"));
		TaskEditor.this.state.setDisplay((String) values.get("taskColor"));
		TaskEditor.this.state.setDescription((String) values.get("taskDescr"));
		TaskEditor.this.state.setOnCreation((String) values.get("onCreation"));
		TaskEditor.this.widget.setContents("<b>" + state.getName() + "</b>");
		TaskEditor.this.widget.getDrawingPanel().getDiagramController().update();
		TaskEditor.this.state.setCreationMessageTemplate((String) values.get("creationMessageTemplate"));

		if (state.getType() == GUIWFState.TYPE_TASK) {
			TaskEditor.this.state.setRequiresNote("yes".equals(values.get("requiresNote")));
			TaskEditor.this.state.setMinNoteSize((Integer) values.get("minNoteSize"));
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
				participants.clear();
				participants.put("_workflow", "Workflow Engine");
			}
		}

		GUIValue[] b = new GUIValue[participants.size()];
		int i = 0;
		for (String key : participants.keySet())
			b[i++] = new GUIValue(key, participants.get(key));
		TaskEditor.this.state.setParticipants(b);

		if (humanInteraction && state.getType() == GUIWFState.TYPE_TASK
				&& (TaskEditor.this.state.getParticipants() == null
						|| TaskEditor.this.state.getParticipants().length == 0)) {
			SC.warn(I18N.message("workflowtaskparticipantatleast"));
			return;
		}
	}
}