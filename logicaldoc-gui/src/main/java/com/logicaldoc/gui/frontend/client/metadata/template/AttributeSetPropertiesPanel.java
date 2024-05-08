package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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

/**
 * This panel shows the properties of an attribute set.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetPropertiesPanel extends HLayout {

	private static final String ATTRIBUTE_NAME = "attributename";

	private static final String EDITOR_STR = "editor";

	private static final String INITIALIZATION = "initialization";

	private static final String VALIDATION = "validation";

	private static final String GROUP_STR = "group";

	private static final String MULTIPLE = "multiple";

	private static final String READONLY = "readonly";

	private static final String HIDDEN = "hidden";

	private static final String MANDATORY = "mandatory";

	private static final String LABEL = "label";

	protected DynamicForm setPropertiesForm = new DynamicForm();

	protected DynamicForm attributeSettingsForm1 = new DynamicForm();

	protected DynamicForm attributeSettingsForm2 = new DynamicForm();

	protected DynamicForm attributeButtonsForm = new DynamicForm();

	protected ValuesManager vm = new ValuesManager();

	protected GUIAttributeSet attributeSet;

	protected ChangedHandler changedHandler;

	private AttributeSetDetailsPanel detailsPanel;

	private String updatingAttributeName = "";

	private ListGrid attributesList;

	private SectionStack attributesStack;

	private SelectItem type;

	private SelectItem editor;

	private TextItem group;

	private LinkItem options;

	public AttributeSetPropertiesPanel(GUIAttributeSet attributeSet, final ChangedHandler changedHandler,
			AttributeSetDetailsPanel detailsPanel) {
		if (attributeSet == null) {
			setMembers(AttributeSetsPanel.SELECT_SET);
			return;
		}

		this.attributeSet = attributeSet;
		this.changedHandler = changedHandler;
		this.detailsPanel = detailsPanel;
		setWidth100();
		setHeight100();
		setMembersMargin(5);

		attributesList = new ListGrid();
		attributesList.setEmptyMessage(I18N.message("notitemstoshow"));
		attributesList.setWidth100();
		attributesList.setHeight100();
		attributesList.setEmptyMessage(I18N.message("norecords"));
		attributesList.setCanReorderRecords(false);
		attributesList.setCanSort(false);
		attributesList.setCanReorderRecords(!attributeSet.isReadonly());
		attributesList.setCanAcceptDroppedRecords(!attributeSet.isReadonly());
		attributesList.setCanFreezeFields(false);
		attributesList.setCanGroupBy(false);
		attributesList.setLeaveScrollbarGap(false);
		attributesList.setShowHeader(true);
		attributesList.setCanEdit(!attributeSet.isReadonly());
		attributesList.setShowRowNumbers(true);
		attributesList.setSelectionType(SelectionStyle.SINGLE);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(150);
		name.setCanEdit(false);
		name.setCanSort(false);
		attributesList.addCellContextClickHandler(event -> {
			if (!AttributeSetPropertiesPanel.this.attributeSet.isReadonly())
				showContextMenu();
			event.cancel();
		});

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL));
		label.setCanEdit(true);
		label.setCanSort(false);
		label.addCellSavedHandler(labelSaved -> {
			AttributeSetPropertiesPanel.this.attributeSet.getAttribute(labelSaved.getRecord().getAttribute("name"))
					.setLabel((String) labelSaved.getNewValue());
			AttributeSetPropertiesPanel.this.changedHandler.onChanged(null);
		});

		attributesList.setFields(name, label);

		attributesList.addDropCompleteHandler(attributesListDropCompleted -> {
			List<String> attributes = new ArrayList<>();
			for (int i = 0; i < attributesList.getTotalRows(); i++) {
				ListGridRecord rec = attributesList.getRecord(i);
				attributes.add(rec.getAttributeAsString("name"));
			}

			AttributeSetPropertiesPanel.this.attributeSet.repositionAttributes(attributes);
			changedHandler.onChanged(null);
		});

		attributesStack = new SectionStack();
		attributesStack.setHeight100();
		attributesStack.setWidth(400);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("attributes") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		section.setItems(attributesList);
		attributesStack.setSections(section);

		fillAttributesList();

		refresh();
	}

	protected void fillAttributesList() {
		if (attributeSet != null && attributeSet.getAttributes() != null) {
			for (GUIAttribute att : attributeSet.getAttributesOrderedByPosition()) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("name", att.getName());
				rec.setAttribute(LABEL, att.getLabel());
				attributesList.addData(rec);
			}
		}
	}

	protected void refresh() {
		VLayout attributesLayout = prepareForms();

		final TextItem attributeName = addAttributeNameItem();

		// Attribute Label
		final TextItem label = ItemFactory.newTextItem(LABEL, null);
		label.setWidth(400);

		// Mandatory
		final CheckboxItem mandatory = new CheckboxItem();
		mandatory.setName(MANDATORY);
		mandatory.setTitle(I18N.message(MANDATORY));
		mandatory.setRedrawOnChange(true);
		mandatory.setWidth(50);
		mandatory.setDefaultValue(false);
		mandatory.setDisabled(attributeSet.isReadonly());
		mandatory.setVisible(true);

		boolean updatingAttributeIsNotSection = updatingAttributeName == null
				|| attributeSet.getAttribute(updatingAttributeName.trim()) == null
				|| attributeSet.getAttribute(updatingAttributeName.trim()).getType() != GUIAttribute.TYPE_SECTION;

		// Hidden
		final CheckboxItem hidden = new CheckboxItem();
		hidden.setName(HIDDEN);
		hidden.setTitle(I18N.message(HIDDEN));
		hidden.setRedrawOnChange(true);
		hidden.setWidth(50);
		hidden.setDefaultValue(false);
		hidden.setDisabled(attributeSet.isReadonly());
		hidden.setVisible(updatingAttributeIsNotSection);

		// Readonly
		final CheckboxItem readonly = new CheckboxItem();
		readonly.setName(READONLY);
		readonly.setTitle(I18N.message(READONLY));
		readonly.setRedrawOnChange(true);
		readonly.setWidth(50);
		readonly.setDefaultValue(false);
		readonly.setDisabled(attributeSet.isReadonly());
		readonly.setVisible(updatingAttributeIsNotSection);

		// Multiple
		final CheckboxItem multiple = new CheckboxItem();
		multiple.setName(MULTIPLE);
		multiple.setTitle(I18N.message("multiplevalues"));
		multiple.setRedrawOnChange(true);
		multiple.setWidth(50);
		multiple.setDefaultValue(false);
		multiple.setDisabled(attributeSet.isReadonly());
		multiple.setEndRow(true);
		multiple.setVisible(updatingAttributeIsNotSection);

		// Editor
		addEditorItem();

		// Type
		addTypeSelector();

		// Values (for preset editor)
		group = ItemFactory.newTextItem(GROUP_STR, null);
		group.setHint(I18N.message("groupname"));
		group.setDisabled(attributeSet.isReadonly());
		group.setVisible(updatingAttributeIsNotSection);

		// Options (for preset editor)
		addOptionsItem(attributeName);

		TextAreaItem validation = prepareValidationItem();

		TextAreaItem initialization = prepareInitializationItem();

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.setEndRow(false);
		save.addClickHandler(saveClick -> onSaveClicked(attributeName, label, mandatory, hidden, readonly, multiple,
				validation, initialization));

		ButtonItem clean = prepareCleanButton();

		attributeSettingsForm1.setItems(attributeName, new SpacerItem(), mandatory, readonly, hidden, multiple);
		attributeSettingsForm2.setItems(label, type, editor, group, options, initialization, validation);
		attributeButtonsForm.setItems(save, clean);

		/*
		 * Make sure that all the controls are not visible when editing a
		 * Section, but not the initialization
		 */
		AdvancedCriteria visibleCriteria = new AdvancedCriteria("type", OperatorId.INOT_EQUAL,
				"" + GUIAttribute.TYPE_SECTION);
		editor.setVisibleWhen(visibleCriteria);
		group.setVisibleWhen(visibleCriteria);
		options.setVisibleWhen(visibleCriteria);
		validation.setVisibleWhen(visibleCriteria);

		type.addChangedHandler(changed -> {
			readonly.setVisible(!changed.getValue().equals("" + GUIAttribute.TYPE_SECTION));
			hidden.setVisible(!changed.getValue().equals("" + GUIAttribute.TYPE_SECTION));
			multiple.setVisible(!changed.getValue().equals("" + GUIAttribute.TYPE_SECTION));
		});

		attributesLayout.setMembers(attributeSettingsForm1, attributeSettingsForm2, attributeButtonsForm);
		attributesLayout.setMembersMargin(10);
		attributesLayout.setWidth100();
		addMember(attributesLayout);

		refreshFieldForm();
	}

	private void onSaveClicked(final TextItem attributeName, final TextItem label, final CheckboxItem mandatory,
			final CheckboxItem hidden, final CheckboxItem readonly, final CheckboxItem multiple,
			TextAreaItem validation, TextAreaItem initialization) {
		if (!attributeSettingsForm1.validate()) {
			return;
		} else {
			String name = (String) attributeName.getValue();
			if (GUIAttribute.isForbidden(name.trim())) {
				final String message = I18N.message("attributenameforbidden",
						Arrays.asList(GUIAttribute.getForbiddenNames()).toString().substring(1).replace("]", ""));
				SC.warn(I18N.message("error"), message);
				return;
			}
		}

		if (attributeName.getValue() != null && !((String) attributeName.getValue()).trim().isEmpty()) {
			collectValuesForAttribute(attributeName, label, mandatory, hidden, readonly, multiple, validation,
					initialization);
		}
	}

	private void collectValuesForAttribute(final TextItem attributeName, final TextItem label,
			final CheckboxItem mandatory, final CheckboxItem hidden, final CheckboxItem readonly,
			final CheckboxItem multiple, TextAreaItem validation, TextAreaItem initialization) {
		if (updatingAttributeName.trim().isEmpty()) {
			GUIAttribute att = new GUIAttribute();
			att.setName(attributeName.getValueAsString());
			att.setLabel(label.getValueAsString());
			att.setMandatory((Boolean) mandatory.getValue());
			att.setHidden((Boolean) hidden.getValue());
			att.setReadonly((Boolean) readonly.getValue());
			att.setMultiple((Boolean) multiple.getValue());
			att.setType(Integer.parseInt(type.getValueAsString()));
			att.setEditor(Integer.parseInt(editor.getValueAsString()));
			att.setValidation(validation.getValueAsString());
			att.setInitialization(initialization.getValueAsString());

			if (att.getType() == GUIAttribute.TYPE_USER)
				att.setStringValue(group.getValueAsString());

			if (attributeSettingsForm1.validate()) {
				changedHandler.onChanged(null);
				addAttribute(att);
			}
		} else {
			GUIAttribute att = attributeSet.getAttribute(updatingAttributeName.trim());
			att.setName(attributeName.getValueAsString());
			att.setLabel(label.getValueAsString());
			att.setMandatory((Boolean) mandatory.getValue());
			att.setHidden((Boolean) hidden.getValue());
			att.setReadonly((Boolean) readonly.getValue());
			att.setMultiple((Boolean) multiple.getValue());
			att.setType(Integer.parseInt(type.getValueAsString()));
			att.setEditor(Integer.parseInt(editor.getValueAsString()));

			if (att.getType() == GUIAttribute.TYPE_USER)
				att.setStringValue(group.getValueAsString());
			else
				att.setStringValue(null);
			att.setValidation(validation.getValueAsString());
			att.setInitialization(initialization.getValueAsString());

			ListGridRecord rec = attributesList.getSelectedRecord();
			rec.setAttribute("name", att.getName());
			rec.setAttribute(LABEL, att.getLabel());
			rec.setAttribute(VALIDATION, att.getValidation());
			rec.setAttribute(INITIALIZATION, att.getInitialization());

			changedHandler.onChanged(null);
		}
	}

	private ButtonItem prepareCleanButton() {
		ButtonItem clean = new ButtonItem();
		clean.setTitle(I18N.message("clean"));
		clean.setAutoFit(true);
		clean.setStartRow(false);
		clean.addClickHandler(cleanClick -> clean());
		return clean;
	}

	private TextAreaItem prepareInitializationItem() {
		TextAreaItem initialization = ItemFactory.newTextAreaItemForAutomation(INITIALIZATION, null, null, false);
		initialization.setWidth("*");
		initialization.setDisabled(attributeSet.isReadonly());
		initialization.setStartRow(true);
		initialization.setColSpan(6);

		FormItemIcon initializationComposer = new FormItemIcon();
		initializationComposer.setName("composer");
		initializationComposer.setWidth(16);
		initializationComposer.setHeight(16);
		initializationComposer.setSrc("[SKIN]/cog.png");
		initializationComposer.setPrompt(I18N.message("openinitializatorcomposer"));
		initializationComposer
				.addFormItemClickHandler(initializationComposerClick -> new AttributeInitializerComposer(initialization,
						type.getValue() != null && !type.getValue().toString().isEmpty()
								? Integer.parseInt(type.getValueAsString())
								: GUIAttribute.TYPE_STRING).show());
		List<FormItemIcon> initializationIcons = new ArrayList<>();
		initializationIcons.addAll(Arrays.asList(initialization.getIcons()));
		initializationIcons.add(initializationComposer);
		initialization.setIcons(initializationIcons.toArray(new FormItemIcon[0]));
		return initialization;
	}

	private TextAreaItem prepareValidationItem() {
		TextAreaItem validation = ItemFactory.newTextAreaItemForAutomation(VALIDATION, null, null, false);
		validation.setWidth("*");
		validation.setDisabled(attributeSet.isReadonly());
		validation.setStartRow(true);
		validation.setColSpan(7);

		FormItemIcon validationComposer = new FormItemIcon();
		validationComposer.setName("composer");
		validationComposer.setWidth(16);
		validationComposer.setHeight(16);
		validationComposer.setSrc("[SKIN]/cog.png");
		validationComposer.setPrompt(I18N.message("openvalidatorcomposer"));
		validationComposer.addFormItemClickHandler(
				(FormItemIconClickEvent validationComposerClick) -> new AttributeValidatorComposer(validation,
						type.getValue() != null && !type.getValue().toString().isEmpty()
								? Integer.parseInt(type.getValueAsString())
								: GUIAttribute.TYPE_STRING).show());
		List<FormItemIcon> validationIcons = new ArrayList<>();
		validationIcons.addAll(Arrays.asList(validation.getIcons()));
		validationIcons.add(validationComposer);
		validation.setIcons(validationIcons.toArray(new FormItemIcon[0]));
		return validation;
	}

	private void addOptionsItem(final TextItem attributeName) {
		options = new LinkItem("options");
		options.setTitle(I18N.message("options"));
		options.setLinkTitle(I18N.message("attributeoptions"));
		options.addClickHandler(optionsClick -> {
			if (attributeSet.getId() == 0L) {
				SC.say(I18N.message("saveattributesetfirst"));
			} else {
				Options attributeOptions = new Options(attributeSet.getId(), attributeName.getValueAsString(),
						attributeSet.isReadonly());
				attributeOptions.show();
			}
		});
	}

	private void addTypeSelector() {
		type = new SelectItem("type", I18N.message("type"));
		LinkedHashMap<String, String> types = new LinkedHashMap<>();
		types.put("" + GUIAttribute.TYPE_STRING, I18N.message("string"));
		types.put("" + GUIAttribute.TYPE_INT, I18N.message("integer"));
		types.put("" + GUIAttribute.TYPE_DOUBLE, I18N.message("decimal"));
		types.put("" + GUIAttribute.TYPE_DATE, I18N.message("date"));
		types.put("" + GUIAttribute.TYPE_BOOLEAN, I18N.message("boolean"));
		types.put("" + GUIAttribute.TYPE_USER, I18N.message("user"));
		types.put("" + GUIAttribute.TYPE_FOLDER, I18N.message("folder"));
		types.put("" + GUIAttribute.TYPE_DOCUMENT, I18N.message("document"));
		types.put("" + GUIAttribute.TYPE_SECTION, I18N.message("section"));
		type.setValueMap(types);
		type.setWrapTitle(false);
		type.setDefaultValue("" + GUIAttribute.TYPE_STRING);
		type.setValue("" + GUIAttribute.TYPE_STRING);
		type.setDisabled(attributeSet.isReadonly());
		type.addChangedHandler(typeChanged -> refreshFieldForm());
	}

	private void addEditorItem() {
		editor = new SelectItem(EDITOR_STR, I18N.message("inputmode"));
		LinkedHashMap<String, String> editors = new LinkedHashMap<>();
		editors.put("" + GUIAttribute.EDITOR_DEFAULT, I18N.message("free"));
		editors.put("" + GUIAttribute.EDITOR_TEXTAREA, I18N.message("freetextarea"));
		editors.put("" + GUIAttribute.EDITOR_LISTBOX, I18N.message("preset"));
		editor.setValueMap(editors);
		editor.setWrapTitle(false);
		editor.setDefaultValue("" + GUIAttribute.EDITOR_DEFAULT);
		editor.setDisabled(attributeSet.isReadonly());
		editor.addChangedHandler(editorChanged -> refreshFieldForm());
	}

	private TextItem addAttributeNameItem() {
		// Attribute Name
		final TextItem attributeName = ItemFactory.newSimpleTextItem(ATTRIBUTE_NAME, null);
		attributeName.setRequired(true);
		attributeName.setWidth(180);
		PickerIcon cleanPicker = new PickerIcon(PickerIcon.CLEAR, attributeNameClick -> {
			clean();
			attributeSettingsForm1.getField(MANDATORY).setDisabled(false);
			attributeSettingsForm1.getField(HIDDEN).setDisabled(false);
			attributeSettingsForm1.getField(READONLY).setDisabled(false);
			attributeSettingsForm1.getField(MULTIPLE).setDisabled(false);
			attributeSettingsForm2.getField("type").setDisabled(false);
			attributeSettingsForm2.getField(EDITOR_STR).setDisabled(false);
			attributeSettingsForm2.getField(GROUP_STR).setDisabled(true);
			attributeSettingsForm2.getField(VALIDATION).setDisabled(false);
			attributeSettingsForm2.getField(INITIALIZATION).setDisabled(false);
			refreshFieldForm();
		});
		if (!attributeSet.isReadonly()) {
			cleanPicker.setNeverDisable(true);
			attributeName.setIcons(cleanPicker);
		} else
			attributeName.setDisabled(true);
		return attributeName;
	}

	private VLayout prepareForms() {
		vm.clearValues();
		vm.clearErrors(false);

		if (setPropertiesForm != null)
			setPropertiesForm.destroy();

		if (Boolean.TRUE.equals(contains(setPropertiesForm)))
			removeChild(setPropertiesForm);
		addMetadata();
		addMember(setPropertiesForm);

		attributesList.addSelectionChangedHandler(attributesListSelected -> {
			Record rec = attributesList.getSelectedRecord();
			onChangeSelectedAttribute(rec);
		});

		HLayout setInfo = new HLayout();
		setInfo.setMembers(attributesStack);
		setInfo.setMembersMargin(3);
		setInfo.setWidth(200);

		addMember(setInfo);

		/*
		 * Prepare the second form for adding or updating the extended
		 * attributes
		 */
		VLayout attributesLayout = new VLayout();

		if (attributeSettingsForm1 != null)
			attributeSettingsForm1.destroy();
		if (Boolean.TRUE.equals(contains(attributeSettingsForm1)))
			removeChild(attributeSettingsForm1);
		attributeSettingsForm1 = new DynamicForm();
		attributeSettingsForm1.setTitleOrientation(TitleOrientation.LEFT);
		attributeSettingsForm1.setNumCols(11);
		attributeSettingsForm1.setWidth(1);

		if (attributeSettingsForm2 != null)
			attributeSettingsForm2.destroy();
		if (Boolean.TRUE.equals(contains(attributeSettingsForm2)))
			removeChild(attributeSettingsForm2);
		attributeSettingsForm2 = new DynamicForm();
		attributeSettingsForm2.setTitleOrientation(TitleOrientation.TOP);
		attributeSettingsForm2.setNumCols(5);
		attributeSettingsForm2.setWidth100();
		attributeSettingsForm2.setHeight100();

		if (attributeButtonsForm != null)
			attributeButtonsForm.destroy();
		if (Boolean.TRUE.equals(contains(attributeButtonsForm)))
			removeChild(attributeButtonsForm);
		attributeButtonsForm = new DynamicForm();
		attributeButtonsForm.setTitleOrientation(TitleOrientation.TOP);
		attributeButtonsForm.setNumCols(2);
		attributeButtonsForm.setWidth(1);
		return attributesLayout;
	}

	protected void addMetadata() {
		setPropertiesForm = new DynamicForm();
		setPropertiesForm.setNumCols(1);
		setPropertiesForm.setValuesManager(vm);
		setPropertiesForm.setTitleOrientation(TitleOrientation.LEFT);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(attributeSet.getId()));
		id.setDisabled(true);

		TextItem name = ItemFactory.newSimpleTextItem("name", attributeSet.getName());
		name.setRequired(true);
		name.setDisabled(attributeSet.isReadonly());
		if (!attributeSet.isReadonly())
			name.addChangedHandler(changedHandler);

		TextItem label = ItemFactory.newTextItem(LABEL, attributeSet.getLabel());
		label.setDisabled(attributeSet.isReadonly());
		if (!attributeSet.isReadonly())
			label.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", attributeSet.getDescription());
		description.setDisabled(attributeSet.isReadonly());

		if (!attributeSet.isReadonly())
			description.addChangedHandler(changedHandler);

		setPropertiesForm.setItems(id, name, label, description);

		setPropertiesForm.setWidth(200);
	}

	@SuppressWarnings("unchecked")
	protected boolean validate() {
		Map<String, Object> values = vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			attributeSet.setName((String) values.get("name"));
			attributeSet.setDescription((String) values.get("description"));
			attributeSet.setLabel((String) values.get(LABEL));
		}
		return !vm.hasErrors();
	}

	private void addAttribute(GUIAttribute att) {
		ListGridRecord rec = new ListGridRecord();
		rec.setAttribute("name", att.getName());
		rec.setAttribute(LABEL, att.getLabel());
		updatingAttributeName = att.getName();
		attributesList.getDataAsRecordList().add(rec);
		attributeSet.appendAttribute(att);
		detailsPanel.enableSave();
		attributesList.selectRecord(rec);

	}

	private void clean() {
		attributeSettingsForm1.clearValues();
		attributeSettingsForm1.getField(ATTRIBUTE_NAME).setDisabled(false);
		attributeSettingsForm1.setValue(ATTRIBUTE_NAME, "");
		updatingAttributeName = "";

		attributeSettingsForm2.setValue(LABEL, (String) null);
		attributeSettingsForm1.setValue(MANDATORY, false);
		attributeSettingsForm1.setValue(HIDDEN, false);
		attributeSettingsForm1.setValue(READONLY, false);
		attributeSettingsForm1.setValue(MULTIPLE, false);
		attributeSettingsForm2.setValue("type", GUIAttribute.TYPE_STRING);
		attributeSettingsForm2.setValue(EDITOR_STR, GUIAttribute.EDITOR_DEFAULT);
		attributeSettingsForm2.setValue(GROUP_STR, (String) null);
		attributeSettingsForm2.setValue(VALIDATION, (String) null);
		attributeSettingsForm2.setValue(INITIALIZATION, (String) null);

		attributesList.deselectAllRecords();
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = prepareDeleteContextMenuItem();

		MenuItem applyValidationToTemplates = prepareApplyValidationToTemplatesContextMenuItem();

		MenuItem applyInitializationToTemplates = prepareApplyInitializationToTemplatesContextMenuItem();

		MenuItem applyAllToTemplates = prepareApplyAllToTemplatesContextMenuItem();

		contextMenu.setItems(applyInitializationToTemplates, applyValidationToTemplates, applyAllToTemplates, delete);
		contextMenu.showContextMenu();
	}

	private MenuItem prepareApplyAllToTemplatesContextMenuItem() {
		MenuItem applyToTemplates = new MenuItem();
		applyToTemplates.setTitle(I18N.message("applyalltotemplates"));
		applyToTemplates.addClickHandler(applyInitializationToTemplatesClick -> {
			final ListGridRecord selection = attributesList.getSelectedRecord();

			LD.ask(I18N.message("applyalltotemplates"), I18N.message("applyalltotemplatestotemplatesquestion"), yes -> {
				if (Boolean.TRUE.equals(yes)) {
					LD.contactingServer();
					AttributeSetService.Instance.get().applyAllToTemplates(attributeSet.getId(),
							selection.getAttributeAsString("name"), new AsyncCallback<>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
									LD.clearPrompt();
								}

								@Override
								public void onSuccess(Void arg0) {
									LD.clearPrompt();
								}
							});
				}
			});
		});
		applyToTemplates.setEnabled(attributeSet.getId() != 0L);
		return applyToTemplates;
	}

	private MenuItem prepareApplyInitializationToTemplatesContextMenuItem() {
		MenuItem applyInitializationToTemplates = new MenuItem();
		applyInitializationToTemplates.setTitle(I18N.message("applyinitializationtotemplates"));
		applyInitializationToTemplates.addClickHandler(applyInitializationToTemplatesClick -> {
			final ListGridRecord selection = attributesList.getSelectedRecord();

			LD.ask(I18N.message("applyinitializationtotemplates"),
					I18N.message("applyinitializationtotemplatesquestion"), yes -> {
						if (Boolean.TRUE.equals(yes)) {
							LD.contactingServer();
							AttributeSetService.Instance.get().applyInitializationToTemplates(attributeSet.getId(),
									selection.getAttributeAsString("name"), new AsyncCallback<>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
											LD.clearPrompt();
										}

										@Override
										public void onSuccess(Void arg0) {
											LD.clearPrompt();
										}
									});
						}
					});
		});
		applyInitializationToTemplates.setEnabled(attributeSet.getId() != 0L);
		return applyInitializationToTemplates;
	}

	private MenuItem prepareApplyValidationToTemplatesContextMenuItem() {
		MenuItem applyValidationToTemplates = new MenuItem();
		applyValidationToTemplates.setTitle(I18N.message("applyvalidationtotemplates"));
		applyValidationToTemplates.addClickHandler(applyValidationToTemplatesClick -> {
			final ListGridRecord selection = attributesList.getSelectedRecord();

			LD.ask(I18N.message("applyvalidationtotemplates"), I18N.message("applyvalidationtotemplatesquestion"),
					confirm -> {
						if (Boolean.TRUE.equals(confirm)) {
							LD.contactingServer();
							AttributeSetService.Instance.get().applyValidationToTemplates(attributeSet.getId(),
									selection.getAttributeAsString("name"), new AsyncCallback<>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
											LD.clearPrompt();
										}

										@Override
										public void onSuccess(Void arg0) {
											LD.clearPrompt();
										}
									});
						}
					});
		});
		applyValidationToTemplates.setEnabled(attributeSet.getId() != 0L);
		return applyValidationToTemplates;
	}

	private MenuItem prepareDeleteContextMenuItem() {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
			}

			LD.ask(I18N.message("ddelete"), I18N.message("confirmdelete"), confirm -> {
				if (Boolean.TRUE.equals(confirm)) {
					for (String attrName : names)
						attributeSet.removeAttribute(attrName);
					attributesList.removeSelectedData();
					clean();
					detailsPanel.enableSave();
				}
			});
		});
		return delete;
	}

	protected void onChangeSelectedAttribute(Record rec) {
		if (rec != null) {
			String selectedAttributeName = rec.getAttributeAsString("name");
			GUIAttribute extAttr = attributeSet.getAttribute(selectedAttributeName);
			attributeSettingsForm1.setValue(ATTRIBUTE_NAME, extAttr.getName());
			attributeSettingsForm1.setValue(MANDATORY, extAttr.isMandatory());
			attributeSettingsForm1.setValue(HIDDEN, extAttr.isHidden());
			attributeSettingsForm1.setValue(READONLY, extAttr.isReadonly());
			attributeSettingsForm1.setValue(MULTIPLE, extAttr.isMultiple());

			attributeSettingsForm1.getItem(HIDDEN).setVisible(!extAttr.isSection());
			attributeSettingsForm1.getItem(READONLY).setVisible(!extAttr.isSection());
			attributeSettingsForm1.getItem(MULTIPLE).setVisible(!extAttr.isSection());

			attributeSettingsForm2.setValue(LABEL, extAttr.getLabel());
			attributeSettingsForm2.setValue("type", extAttr.getType());
			attributeSettingsForm2.setValue(EDITOR_STR, extAttr.getEditor());
			attributeSettingsForm2.setValue(GROUP_STR, extAttr.getStringValue());
			attributeSettingsForm2.setValue(VALIDATION, extAttr.getValidation());
			attributeSettingsForm2.setValue(INITIALIZATION, extAttr.getInitialization());
			updatingAttributeName = selectedAttributeName;
			refreshFieldForm();
		}
	}

	public void refreshFieldForm() {
		if (type.getValueAsString().equals("" + GUIAttribute.TYPE_STRING)) {
			editor.setVisible(true);
			group.setVisible(false);
			group.setValue("");
			options.setVisible(editor.getValueAsString().equals("" + GUIAttribute.EDITOR_LISTBOX));
		} else if (type.getValueAsString().equals("" + GUIAttribute.TYPE_USER)) {
			editor.setVisible(false);
			group.setVisible(true);
			options.setVisible(false);
		} else if (type.getValueAsString().equals("" + GUIAttribute.TYPE_FOLDER)) {
			editor.setVisible(false);
			options.setVisible(false);
			group.setVisible(false);
			group.setValue("");
		} else if (type.getValueAsString().equals("" + GUIAttribute.TYPE_SECTION)) {
			editor.setVisible(false);
			options.setVisible(true);
			group.setVisible(false);
			group.setValue("");
		} else {
			editor.setVisible(false);
			group.setVisible(false);
			options.setVisible(false);
			group.setValue("");
		}

		if (!updatingAttributeName.isEmpty())
			attributeSettingsForm1.getItem(ATTRIBUTE_NAME).setDisabled(true);

		attributeSettingsForm1.markForRedraw();
		attributeSettingsForm2.markForRedraw();
	}
}