package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.PickerIconName;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.DropCompleteEvent;
import com.smartgwt.client.widgets.events.DropCompleteHandler;
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
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
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

	protected DynamicForm templateForm = new DynamicForm();

	protected ValuesManager vm = new ValuesManager();

	protected GUITemplate template;

	protected ChangedHandler changedHandler;

	public String updatingAttributeName = "";

	private ListGrid attributesList;

	private HLayout container = new HLayout();

	public TemplatePropertiesPanel(GUITemplate template, ChangedHandler changedHandler,
			TemplateDetailsPanel detailsPanel) {
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
			att.setLabel(rec.getAttributeAsString("label"));
			att.setType(Integer.parseInt(rec.getAttributeAsString("type")));
			att.setSet(rec.getAttributeAsString("set"));
			att.setMandatory(rec.getAttributeAsBoolean("mandatory"));
			att.setHidden(rec.getAttributeAsBoolean("hidden"));
			att.setMultiple(rec.getAttributeAsBoolean("multiple"));
			att.setSetId(Long.parseLong(rec.getAttributeAsString("setId")));
			att.setEditor(Integer.parseInt(rec.getAttributeAsString("editor")));

			template.appendAttribute(att);

			ListGridRecord[] records = attributesList.getRecords();
			if (records != null)
				for (ListGridRecord record : records)
					attributesList.removeData(record);

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
		attributesList.setCanAcceptDroppedRecords(!template.isReadonly() &&  template.isWrite());
		attributesList.setAutoFetchData(true);
		attributesList.setShowRecordComponents(true);
		attributesList.setShowRecordComponentsByCell(true);

		attributesList.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (!TemplatePropertiesPanel.this.template.isReadonly() && TemplatePropertiesPanel.this.template.isWrite())
					showContextMenu();
				event.cancel();
			}
		});

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanEdit(false);
		name.setCanSort(false);
		name.setWidth(100);

		ListGridField label = new ListGridField("label", I18N.message("label"));
		label.setCanEdit(false);
		label.setCanSort(false);
		label.setWidth(150);

		ListGridField set = new ListGridField("set", I18N.message("set"));
		set.setCanEdit(false);
		set.setCanSort(false);
		set.setWidth("*");

		ListGridField mandatory = new ListGridField("mandatory", I18N.message("mandatory"));
		mandatory.setCanEdit(false);
		mandatory.setCanSort(false);
		mandatory.setWidth(90);

		ListGridField hidden = new ListGridField("hidden", I18N.message("hidden"));
		hidden.setCanEdit(false);
		hidden.setCanSort(false);
		hidden.setWidth(80);

		ListGridField multiple = new ListGridField("multiple", I18N.message("multiple"));
		multiple.setCanEdit(false);
		multiple.setCanSort(false);
		multiple.setWidth(80);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanEdit(false);
		type.setCanSort(false);
		type.setWidth(60);
		type.setCellFormatter(new AttributeTypeFormatter());

		attributesList.addDropCompleteHandler(new DropCompleteHandler() {

			@Override
			public void onDropComplete(DropCompleteEvent event) {
				List<String> attributes = new ArrayList<String>();
				for (int i = 0; i < attributesList.getTotalRows(); i++) {
					ListGridRecord record = attributesList.getRecord(i);
					attributes.add(record.getAttributeAsString("name"));
				}

				TemplatePropertiesPanel.this.template.reorderAttributes(attributes);
				changedHandler.onChanged(null);
			}
		});

		attributesList.setFields(name, label, type, mandatory, hidden, multiple, set);

		Button addAttributes = new Button(I18N.message("addattributes"));
		addAttributes.setMargin(2);
		addAttributes.setHeight(30);
		addAttributes.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				AddTemplateAttributeDialog dialog = new AddTemplateAttributeDialog(TemplatePropertiesPanel.this);
				dialog.show();
			}
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

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				final ListGridRecord[] selection = attributesList.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				final String[] names = new String[selection.length];
				for (int i = 0; i < selection.length; i++) {
					names[i] = selection[i].getAttribute("name");
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							for (String attrName : names)
								template.removeAttribute(attrName);
							attributesList.removeSelectedData();
							if (TemplatePropertiesPanel.this.changedHandler != null)
								TemplatePropertiesPanel.this.changedHandler.onChanged(null);
						}
					}
				});
			}
		});

		MenuItem makeMandatory = new MenuItem();
		makeMandatory.setTitle(I18N.message("makemandatory"));
		makeMandatory.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
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
			}
		});

		MenuItem makeOptional = new MenuItem();
		makeOptional.setTitle(I18N.message("makeoptional"));
		makeOptional.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
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
			}
		});

		MenuItem makeHidden = new MenuItem();
		makeHidden.setTitle(I18N.message("makehidden"));
		makeHidden.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
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
			}
		});

		MenuItem makeVisible = new MenuItem();
		makeVisible.setTitle(I18N.message("makevisible"));
		makeVisible.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
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
			}
		});

		contextMenu.setItems(makeMandatory, makeOptional, makeVisible, makeHidden, delete);
		contextMenu.showContextMenu();
	}

	protected void fillAttributesList() {
		if (attributesList.getRecordList().getLength() > 0)
			attributesList.getRecordList().removeList(attributesList.getRecords());

		if (template == null)
			return;

		GUIAttribute[] attributes = template.getAttributesOrderedByPosition();
		if (attributes == null)
			return;

		for (int i = 0; i < attributes.length; i++) {
			GUIAttribute att = attributes[i];
			ListGridRecord record = new ListGridRecord();
			record.setAttribute("name", att.getName());
			record.setAttribute("label", att.getLabel());
			record.setAttribute("set", att.getSet());
			record.setAttribute("setId", att.getSetId());
			record.setAttribute("type", att.getType());
			record.setAttribute("editor", att.getEditor());
			record.setAttribute("mandatory", att.isMandatory());
			record.setAttribute("hidden", att.isHidden());
			record.setAttribute("multiple", att.isMultiple());
			attributesList.getRecordList().add(record);
		}
	}

	protected void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (templateForm != null)
			templateForm.destroy();

		if (contains(container))
			removeMember(container);

		container = new HLayout();
		container.setMembersMargin(5);

		prepareTemplateForm();

		if (template.getId() != 0L)
			prepareTemplateAttributes();

		addMember(container);
	}

	protected void prepareTemplateForm() {
		templateForm = new DynamicForm();
		templateForm.setNumCols(1);
		templateForm.setValuesManager(vm);
		templateForm.setTitleOrientation(TitleOrientation.LEFT);
		
		StaticTextItem id = ItemFactory.newStaticTextItem("id", "id", Long.toString(template.getId()));
		id.setDisabled(true);

		TextItem name = ItemFactory.newSimpleTextItem("name", I18N.message("name"), template.getName());
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
		if (!vm.hasErrors()) {
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
					att.setLabel(rec.getAttributeAsString("label"));
					att.setType(Integer.parseInt(rec.getAttributeAsString("type")));
					att.setSet(rec.getAttributeAsString("set"));
					att.setSetId(Long.parseLong(rec.getAttributeAsString("setId")));
					att.setEditor(Integer.parseInt(rec.getAttributeAsString("editor")));
					att.setMandatory(rec.getAttributeAsBoolean("mandatory"));
					att.setHidden(rec.getAttributeAsBoolean("hidden"));
					att.setMultiple(rec.getAttributeAsBoolean("multiple"));
					template.appendAttribute(att);
				}
			}
		}

		return !vm.hasErrors();
	}

	public GUITemplate getTemplate() {
		return template;
	}
}