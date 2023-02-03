package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.PickerIconName;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.DropCompleteEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows all template properties.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TemplatePropertiesPanel extends HLayout {

	private static final String DEPENDS_ON = "dependsOn";

	private static final String INITIALIZATION = "initialization";

	private static final String VALIDATION = "validation";

	private static final String EDITOR = "editor";

	private static final String SET_ID = "setId";

	private static final String MULTIPLE = "multiple";

	private static final String READONLY = "readonly";

	private static final String HIDDEN = "hidden";

	private static final String MANDATORY = "mandatory";

	private static final String LABEL = "label";

	protected DynamicForm templateForm = new DynamicForm();

	protected ValuesManager vm = new ValuesManager();

	protected GUITemplate template;

	protected ChangedHandler changedHandler;

	public String updatingAttributeName = "";

	private ListGrid attributesList;

	private HLayout container = new HLayout();

	public TemplatePropertiesPanel(GUITemplate template, ChangedHandler changedHandler) {
		if (template == null) {
			setMembers(TemplatesPanel.SELECT_TEMPLATE);
			return;
		}

		this.template = template;
		this.changedHandler = changedHandler;
		setWidth100();
		setHeight100();
		setMembersMargin(10);

		refresh();
	}

	public void addAttributes(ListGridRecord[] recs) {
		if (recs == null)
			return;

		for (ListGridRecord rec : recs) {
			if (template.getAttribute(rec.getAttributeAsString("name")) != null)
				continue;

			GUIAttribute att = new GUIAttribute();
			att.setName(rec.getAttributeAsString("name"));
			att.setLabel(rec.getAttributeAsString(LABEL));
			att.setType(Integer.parseInt(rec.getAttributeAsString("type")));
			att.setSet(rec.getAttributeAsString("set"));
			att.setMandatory(rec.getAttributeAsBoolean(MANDATORY));
			att.setHidden(rec.getAttributeAsBoolean(HIDDEN));
			att.setReadonly(rec.getAttributeAsBoolean(READONLY));
			att.setMultiple(rec.getAttributeAsBoolean(MULTIPLE));
			att.setSetId(Long.parseLong(rec.getAttributeAsString(SET_ID)));
			att.setEditor(Integer.parseInt(rec.getAttributeAsString(EDITOR)));
			att.setValidation(rec.getAttributeAsString(VALIDATION));
			att.setInitialization(rec.getAttributeAsString(INITIALIZATION));
			att.setDependsOn(rec.getAttributeAsString(DEPENDS_ON));

			template.appendAttribute(att);

			ListGridRecord[] records = attributesList.getRecords();
			if (records != null)
				for (ListGridRecord recd : records)
					attributesList.removeData(recd);

			fillAttributesList();
		}

		if (changedHandler != null)
			changedHandler.onChanged(null);
	}

	private void prepareTemplateAttributes() {
		attributesList = new ListGrid();
		attributesList.setEmptyMessage(I18N.message("notitemstoshow"));
		attributesList.setWidth100();
		attributesList.setHeight100();
		attributesList.setEmptyMessage(I18N.message("norecords"));
		attributesList.setCanSort(false);
		attributesList.setCanFreezeFields(false);
		attributesList.setCanGroupBy(false);
		attributesList.setLeaveScrollbarGap(false);
		attributesList.setShowHeader(true);
		attributesList.setSelectionType(SelectionStyle.MULTIPLE);
		attributesList.setCanEdit(false);
		attributesList.setShowRowNumbers(true);
		attributesList.setCanReorderRecords(!template.isReadonly() && template.isWrite());
		attributesList.setCanAcceptDroppedRecords(!template.isReadonly() && template.isWrite());
		attributesList.setAutoFetchData(true);
		attributesList.setShowRecordComponents(true);
		attributesList.setShowRecordComponentsByCell(true);

		attributesList.addCellContextClickHandler((CellContextClickEvent event) -> {
			if (!TemplatePropertiesPanel.this.template.isReadonly() && TemplatePropertiesPanel.this.template.isWrite())
				showContextMenu();
			event.cancel();
		});

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanEdit(false);
		name.setCanSort(false);
		name.setAutoFitWidth(true);
		name.setMinWidth(80);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL));
		label.setCanEdit(false);
		label.setCanSort(false);
		label.setAutoFitWidth(true);
		label.setMinWidth(80);

		ListGridField set = new ListGridField("set", I18N.message("set"));
		set.setCanEdit(false);
		set.setCanSort(false);
		set.setHidden(true);
		set.setWidth("*");

		ListGridField mandatory = new ListGridField(MANDATORY, I18N.message(MANDATORY));
		mandatory.setCanEdit(false);
		mandatory.setCanSort(false);
		mandatory.setAutoFitWidth(true);
		mandatory.setMinWidth(70);

		ListGridField hidden = new ListGridField(HIDDEN, I18N.message(HIDDEN));
		hidden.setCanEdit(false);
		hidden.setCanSort(false);
		hidden.setAutoFitWidth(true);
		hidden.setMinWidth(70);

		ListGridField multiple = new ListGridField(MULTIPLE, I18N.message(MULTIPLE));
		multiple.setCanEdit(false);
		multiple.setCanSort(false);
		multiple.setAutoFitWidth(true);
		multiple.setMinWidth(70);

		ListGridField readonly = new ListGridField(READONLY, I18N.message(READONLY));
		readonly.setCanEdit(false);
		readonly.setCanSort(false);
		readonly.setAutoFitWidth(true);
		readonly.setMinWidth(70);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanEdit(false);
		type.setCanSort(false);
		type.setAutoFitWidth(true);
		type.setMinWidth(70);
		type.setCellFormatter(new AttributeTypeFormatter());

		ListGridField preset = new ListGridField("preset", I18N.message("preset"));
		preset.setCanEdit(false);
		preset.setCanSort(false);
		preset.setAutoFitWidth(true);
		preset.setMinWidth(70);

		ListGridField dependsOn = new ListGridField(DEPENDS_ON, I18N.message("dependson"));
		dependsOn.setCanEdit(false);
		dependsOn.setCanSort(false);
		dependsOn.setAutoFitWidth(true);
		dependsOn.setMinWidth(70);

		attributesList.addDropCompleteHandler((DropCompleteEvent event) -> {
			List<String> attributes = new ArrayList<>();
			for (int i = 0; i < attributesList.getTotalRows(); i++) {
				ListGridRecord rec = attributesList.getRecord(i);
				attributes.add(rec.getAttributeAsString("name"));
			}

			TemplatePropertiesPanel.this.template.reorderAttributes(attributes);
			changedHandler.onChanged(null);
		});

		attributesList.setFields(name, label, type, mandatory, hidden, readonly, multiple, preset, dependsOn, set);

		Button addAttributes = new Button(I18N.message("addattributes"));
		addAttributes.setMargin(2);
		addAttributes.setHeight(30);
		addAttributes.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			AddTemplateAttributeDialog dialog = new AddTemplateAttributeDialog(TemplatePropertiesPanel.this);
			dialog.show();
		});

		SectionStack attributesStack = new SectionStack();
		attributesStack.setHeight100();
		attributesStack.setWidth(680);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("attributes") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		if (template.isReadonly() || !template.isWrite())
			section.setItems(attributesList);
		else
			section.setItems(attributesList, addAttributes);

		attributesStack.setSections(section);
		attributesStack.draw();

		container.addMember(attributesStack);
		fillAttributesList();
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = prepareDeleteItem();

		MenuItem makeMandatory = pepareMakeMandatoryItem();

		MenuItem makeOptional = prepareMakeOptionalItem();

		MenuItem makeHidden = prepareMakeHiddenItem();

		MenuItem makeReadonly = prepareMakeRedonlyItem();

		MenuItem makeVisible = prepareMakeVisibleItem();

		MenuItem validation = prepareValidationItem();

		MenuItem resetValidation = prepareResetValidationItem();

		MenuItem initialization = prepareInitializationItem();

		MenuItem resetInitialization = prepareResetInitializationItem();

		MenuItem dependsOn = prepareDependsOnItem();

		contextMenu.setItems(makeMandatory, makeOptional, makeVisible, makeReadonly, makeHidden, dependsOn,
				initialization, resetInitialization, validation, resetValidation, delete);
		contextMenu.showContextMenu();
	}

	private MenuItem prepareDependsOnItem() {
		MenuItem dependsOn = new MenuItem();
		dependsOn.setTitle(I18N.message("dependson"));
		dependsOn.addClickHandler((MenuItemClickEvent event) -> {
				ListGridRecord selection = attributesList.getSelectedRecord();
				LD.askForString("dependson", "attributename", selection.getAttributeAsString(DEPENDS_ON),
						(String value) -> {
							ListGridRecord selectedRecord = attributesList.getSelectedRecord();
							selectedRecord.setAttribute(DEPENDS_ON, value);
							if (changedHandler != null)
								changedHandler.onChanged(null);
						});
		});
		dependsOn.setEnabled(attributesList.getSelectedRecord().getAttributeAsBoolean("preset"));
		return dependsOn;
	}

	private MenuItem prepareResetInitializationItem() {
		MenuItem resetInitialization = new MenuItem();
		resetInitialization.setTitle(I18N.message("resetinitialization"));
		resetInitialization.addClickHandler((MenuItemClickEvent event) -> {
			LD.ask(I18N.message("resetinitialization"), I18N.message("resetinitializationnquestion"), (Boolean yes) -> {
				if (Boolean.TRUE.equals(yes))
					resetInitialization();
			});
		});
		return resetInitialization;
	}

	private void resetInitialization() {
		ListGridRecord[] selection = attributesList.getSelectedRecords();

		LD.contactingServer();
		AttributeSetService.Instance.get().getAttributeSets(new AsyncCallback<GUIAttributeSet[]>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttributeSet[] sets) {
				LD.clearPrompt();
				Map<Long, GUIAttributeSet> setsMap = new HashMap<Long, GUIAttributeSet>();
				for (GUIAttributeSet set : sets)
					setsMap.put(set.getId(), set);

				for (ListGridRecord rec : selection) {
					GUIAttribute setAttribute = null;
					GUIAttributeSet set = setsMap.get(rec.getAttributeAsLong(SET_ID));
					if (set != null)
						setAttribute = set.getAttribute(rec.getAttributeAsString("name"));

					if (setAttribute != null) {
						template.getAttribute(rec.getAttributeAsString("name"))
								.setInitialization(setAttribute.getInitialization());
						rec.setAttribute(INITIALIZATION, setAttribute.getInitialization());
					} else {
						template.getAttribute(rec.getAttributeAsString("name")).setInitialization(null);
						rec.setAttribute(INITIALIZATION, (String) null);
					}
				}

				if (changedHandler != null)
					changedHandler.onChanged(null);
			}
		});
	}

	private MenuItem prepareInitializationItem() {
		MenuItem initialization = new MenuItem();
		initialization.setTitle(I18N.message(INITIALIZATION));
		initialization.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord selection = attributesList.getSelectedRecord();
			TextAreaItem initializationItem = ItemFactory.newTextAreaItemForAutomation(INITIALIZATION, INITIALIZATION,
					null, null, false);
			initializationItem.setWidth(600);
			initializationItem.setHeight(400);

			GUIAttribute attribute = template.getAttribute(selection.getAttributeAsString("name"));

			LD.askForValue(I18N.message(INITIALIZATION), I18N.message(INITIALIZATION), attribute.getInitialization(),
					initializationItem, 600, (String initializationScript) -> {
						attribute.setInitialization(initializationScript);
						fillAttributesList();
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		return initialization;
	}

	private MenuItem prepareResetValidationItem() {
		MenuItem resetValidation = new MenuItem();
		resetValidation.setTitle(I18N.message("resetvalidation"));
		resetValidation.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("resetvalidation"), I18N.message("resetvalidationquestion"), (Boolean yes) -> {
					if (Boolean.TRUE.equals(yes))
						resetValidation();
				});
			}
		});
		return resetValidation;
	}

	private MenuItem prepareValidationItem() {
		MenuItem validation = new MenuItem();
		validation.setTitle(I18N.message(VALIDATION));
		validation.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord selection = attributesList.getSelectedRecord();
			TextAreaItem validationItem = ItemFactory.newTextAreaItemForAutomation(VALIDATION, VALIDATION, null, null,
					false);
			validationItem.setWidth(600);
			validationItem.setHeight(400);

			GUIAttribute attribute = template.getAttribute(selection.getAttributeAsString("name"));

			LD.askForValue(I18N.message(VALIDATION), I18N.message(VALIDATION), attribute.getValidation(),
					validationItem, 600, (String validationScript) -> {
						attribute.setValidation(validationScript);
						fillAttributesList();
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		return validation;
	}

	private MenuItem prepareMakeVisibleItem() {
		MenuItem makeVisible = new MenuItem();
		makeVisible.setTitle(I18N.message("makevisible"));
		makeVisible.addClickHandler((MenuItemClickEvent event) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
				template.getAttribute(names[i]).setHidden(false);
			}

			fillAttributesList();

			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return makeVisible;
	}

	private MenuItem prepareMakeRedonlyItem() {
		MenuItem makeReadonly = new MenuItem();
		makeReadonly.setTitle(I18N.message("makereadonly"));
		makeReadonly.addClickHandler((MenuItemClickEvent event) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
				template.getAttribute(names[i]).setReadonly(true);
			}

			fillAttributesList();

			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return makeReadonly;
	}

	private MenuItem prepareMakeHiddenItem() {
		MenuItem makeHidden = new MenuItem();
		makeHidden.setTitle(I18N.message("makehidden"));
		makeHidden.addClickHandler((MenuItemClickEvent event) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
				template.getAttribute(names[i]).setHidden(true);
			}

			fillAttributesList();

			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return makeHidden;
	}

	private MenuItem prepareMakeOptionalItem() {
		MenuItem makeOptional = new MenuItem();
		makeOptional.setTitle(I18N.message("makeoptional"));
		makeOptional.addClickHandler((MenuItemClickEvent event) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
				template.getAttribute(names[i]).setMandatory(false);
			}

			fillAttributesList();

			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return makeOptional;
	}

	private MenuItem pepareMakeMandatoryItem() {
		MenuItem makeMandatory = new MenuItem();
		makeMandatory.setTitle(I18N.message("makemandatory"));
		makeMandatory.addClickHandler((MenuItemClickEvent makeMandatoryClicked) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
				template.getAttribute(names[i]).setMandatory(true);
			}

			fillAttributesList();

			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return makeMandatory;
	}

	private MenuItem prepareDeleteItem() {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent deleteClick) -> {
			final ListGridRecord[] selection = attributesList.getSelectedRecords();
			final String[] names = new String[selection.length];
			for (int i = 0; i < selection.length; i++) {
				names[i] = selection[i].getAttribute("name");
			}

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean yes) -> {
					if (Boolean.TRUE.equals(yes)) {
						for (String attrName : names)
							template.removeAttribute(attrName);
						attributesList.removeSelectedData();
						if (TemplatePropertiesPanel.this.changedHandler != null)
							TemplatePropertiesPanel.this.changedHandler.onChanged(null);
					}
			});
		});
		return delete;
	}

	private void fillAttributesList() {
		if (attributesList.getRecordList().getLength() > 0)
			attributesList.getRecordList().removeList(attributesList.getRecords());

		if (template == null)
			return;

		GUIAttribute[] attributes = template.getAttributesOrderedByPosition();
		if (attributes == null)
			return;

		for (int i = 0; i < attributes.length; i++) {
			GUIAttribute att = attributes[i];
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("name", att.getName());
			rec.setAttribute(LABEL, att.getLabel());
			rec.setAttribute("set", att.getSet());
			rec.setAttribute(SET_ID, att.getSetId());
			rec.setAttribute("type", att.getType());
			rec.setAttribute(EDITOR, att.getEditor());
			rec.setAttribute(MANDATORY, att.isMandatory());
			rec.setAttribute(HIDDEN, att.isHidden());
			rec.setAttribute(READONLY, att.isReadonly());
			rec.setAttribute(MULTIPLE, att.isMultiple());
			rec.setAttribute(VALIDATION, att.getValidation());
			rec.setAttribute(INITIALIZATION, att.getInitialization());
			rec.setAttribute("preset", att.getEditor() == GUIAttribute.EDITOR_LISTBOX);
			rec.setAttribute(DEPENDS_ON, att.getDependsOn());
			attributesList.getRecordList().add(rec);
		}
	}

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (templateForm != null)
			templateForm.destroy();

		if (Boolean.TRUE.equals(contains(container)))
			removeMember(container);

		container = new HLayout();
		container.setMembersMargin(5);

		prepareTemplateForm();

		if (template.getId() != 0L)
			prepareTemplateAttributes();

		addMember(container);
	}

	private void prepareTemplateForm() {
		templateForm = new DynamicForm();
		templateForm.setNumCols(1);
		templateForm.setValuesManager(vm);
		templateForm.setTitleOrientation(TitleOrientation.LEFT);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(template.getId()));
		id.setDisabled(true);

		TextItem name = ItemFactory.newSimpleTextItem("name", template.getName());
		name.setRequired(true);
		name.setDisabled(template.isReadonly() || !template.isWrite());
		if (!template.isReadonly() && template.isWrite())
			name.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description", template.getDescription());
		description.setDisabled(template.isReadonly() || !template.isWrite());

		PickerIcon computeStat = new PickerIcon(PickerIconName.REFRESH, new FormItemClickHandler() {
			public void onFormItemClick(final FormItemIconClickEvent event) {
				event.getItem().setValue(I18N.message("computing") + "...");
				TemplateService.Instance.get().countDocuments(template.getId(), new AsyncCallback<Long>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Long count) {
						event.getItem().setValue(Util.formatLong(count));
					}
				});
			}
		});
		computeStat.setPrompt(I18N.message("calculatestats"));

		StaticTextItem docs = ItemFactory.newStaticTextItem("docs", "documents", "-");
		docs.setIconHSpace(2);
		docs.setIconWidth(16);
		docs.setIconHeight(16);
		docs.setIcons(computeStat);
		docs.setWidth("1%");

		if (!template.isReadonly() && template.isWrite())
			description.addChangedHandler(changedHandler);

		templateForm.setItems(id, name, description, docs);

		container.addMember(templateForm);
	}

	@SuppressWarnings("unchecked")
	protected boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();

		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			template.setName((String) values.get("name"));
			template.setDescription((String) values.get("description"));
		}

		if (template.getId() != 0L) {
			template.setAttributes(null);
			ListGridRecord[] records = attributesList.getRecords();
			int position = 0;
			if (records != null) {
				for (int i = 0; i < attributesList.getTotalRows(); i++) {
					ListGridRecord rec = attributesList.getRecord(i);
					GUIAttribute att = new GUIAttribute();
					att.setPosition(position++);
					att.setName(rec.getAttributeAsString("name"));
					att.setLabel(rec.getAttributeAsString(LABEL));
					att.setType(Integer.parseInt(rec.getAttributeAsString("type")));
					att.setSet(rec.getAttributeAsString("set"));
					att.setSetId(Long.parseLong(rec.getAttributeAsString(SET_ID)));
					att.setEditor(Integer.parseInt(rec.getAttributeAsString(EDITOR)));
					att.setDependsOn(rec.getAttributeAsString(DEPENDS_ON));
					att.setMandatory(rec.getAttributeAsBoolean(MANDATORY));
					att.setHidden(rec.getAttributeAsBoolean(HIDDEN));
					att.setReadonly(rec.getAttributeAsBoolean(READONLY));
					att.setMultiple(rec.getAttributeAsBoolean(MULTIPLE));
					att.setValidation(rec.getAttributeAsString(VALIDATION));
					att.setInitialization(rec.getAttributeAsString(INITIALIZATION));
					template.appendAttribute(att);
				}
			}
		}

		return !vm.hasErrors();
	}

	public GUITemplate getTemplate() {
		return template;
	}

	private void resetValidation() {
		ListGridRecord[] selection = attributesList.getSelectedRecords();
		LD.contactingServer();
		AttributeSetService.Instance.get().getAttributeSets(new AsyncCallback<GUIAttributeSet[]>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttributeSet[] sets) {
				LD.clearPrompt();
				Map<Long, GUIAttributeSet> setsMap = new HashMap<>();
				for (GUIAttributeSet set : sets)
					setsMap.put(set.getId(), set);

				for (ListGridRecord rec : selection) {
					GUIAttribute setAttribute = null;
					GUIAttributeSet set = setsMap.get(rec.getAttributeAsLong(SET_ID));
					if (set != null)
						setAttribute = set.getAttribute(rec.getAttributeAsString("name"));

					if (setAttribute != null) {
						template.getAttribute(rec.getAttributeAsString("name"))
								.setValidation(setAttribute.getValidation());
						rec.setAttribute(VALIDATION, setAttribute.getValidation());
					} else {
						template.getAttribute(rec.getAttributeAsString("name")).setValidation(null);
						rec.setAttribute(VALIDATION, (String) null);
					}
				}

				if (changedHandler != null)
					changedHandler.onChanged(null);
			}
		});
	}
}