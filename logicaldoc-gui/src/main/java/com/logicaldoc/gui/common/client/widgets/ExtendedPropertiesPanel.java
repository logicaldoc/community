package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.InputValues;
import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * A panel for editing extended attributes of an extensible object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ExtendedPropertiesPanel extends HLayout {

	private static final String TEMPLATE = "template";

	protected GUIExtensibleObject object;

	protected ChangedHandler changedHandler;

	protected ChangedHandler templateChangedHandler;

	protected boolean updateEnabled = false;

	protected boolean checkMandatory = false;

	protected boolean allowTemplateSelection = true;

	protected DynamicForm templateForm = new DynamicForm();

	protected DynamicForm attributesForm = new DynamicForm();

	protected ValuesManager vm = new ValuesManager();

	protected List<FormItem> standardItems = new ArrayList<>();

	protected SelectItem templateItem = null;

	protected List<FormItem> extendedItems = new ArrayList<>();

	private ClearDependantAttributes dependeciesHandler = new ClearDependantAttributes();

	public ExtendedPropertiesPanel(GUIExtensibleObject object, ChangedHandler changedHandler,
			ChangedHandler templateChangedHandler, boolean updateEnabled, boolean checkMandatory,
			boolean allowTemplateSelection) {
		super();
		this.object = object;
		this.changedHandler = changedHandler;
		this.templateChangedHandler = templateChangedHandler;
		this.updateEnabled = updateEnabled;
		this.checkMandatory = checkMandatory;
		this.allowTemplateSelection = allowTemplateSelection;

		setWidth100();
		setHeight100();
		setMembersMargin(20);
		refresh();

		addResizedHandler(event -> adaptForms());
	}

	public ExtendedPropertiesPanel(GUIExtensibleObject object, ChangedHandler changedHandler, boolean updateEnabled,
			boolean checkMandatory, boolean allowTemplateSelection) {
		this(object, changedHandler, null, updateEnabled, checkMandatory, allowTemplateSelection);
	}

	protected void adaptForms() {
		if (templateItem != null && templateItem.getValue() != null) {
			int maxExtCols = (getWidth() - 500) / 160; // 160 = length of
														// an item
			int maxExtRows = getHeight() / 46; // 46 = height of an item
			if (maxExtRows < 3)
				maxExtCols = 3;

			if (extendedItems != null) {
				maxExtCols = extendedItems.size() / maxExtRows;
			}

			if (maxExtCols < 2)
				maxExtCols = 2;

			attributesForm.setNumCols(maxExtCols);
		}
	}

	protected void refresh() {
		vm.clearValues();
		vm.clearErrors(false);
		extendedItems.clear();

		if (templateForm != null)
			templateForm.destroy();

		if (Boolean.TRUE.equals(contains(templateForm)))
			removeChild(templateForm);
		templateForm = new DynamicForm();
		templateForm.setValuesManager(vm);
		templateForm.setTitleOrientation(TitleOrientation.TOP);
		templateForm.setNumCols(1);
		standardItems.clear();

		putCustomIdField();

		templateItem = ItemFactory.newTemplateSelector(true, object.getTemplateId());
		if (changedHandler != null)
			templateItem.addChangedHandler(changedHandler);
		templateItem.setMultiple(false);
		templateItem.setDisabled(!updateEnabled || (isDocument() && ((GUIDocument) object).getFolder() != null
				&& ((GUIDocument) object).getFolder().getTemplateLocked() == 1));
		if (object.getTemplateId() != null)
			templateItem.setValue(object.getTemplateId().toString());

		templateItem.addChangedHandler(this::handleTemplateChangedSelection);

		if (templateChangedHandler != null)
			templateItem.addChangedHandler(templateChangedHandler);

		if (Feature.visible(Feature.TEMPLATE)) {
			standardItems.add(templateItem);

			if (!Feature.enabled(Feature.TEMPLATE)) {
				templateItem.setDisabled(true);
				templateItem.setTooltip(I18N.message("featuredisabled"));
			}
		}

		templateForm.setItems(standardItems.toArray(new FormItem[0]));

		if (allowTemplateSelection)
			addMember(templateForm);

		if (Feature.enabled(Feature.TEMPLATE))
			prepareExtendedAttributes(object.getTemplateId());
	}

	private void putCustomIdField() {
		if (isDocument()) {
			TextItem customId = ItemFactory.newTextItem("customid", ((GUIDocument) object).getCustomId());
			if (changedHandler != null)
				customId.addChangedHandler(changedHandler);
			customId.setDisabled(!updateEnabled);
			standardItems.add(customId);
		}
	}

	private void handleTemplateChangedSelection(ChangedEvent event) {
		Object templateValue = templateItem.getValue();
		if (templateValue != null && !"".equals(templateValue.toString())) {
			object.setAttributes(new GUIAttribute[0]);
			long templateId = Long.parseLong(templateValue.toString());
			prepareExtendedAttributes(templateId);
			object.setTemplateId(templateId);
		} else {
			object.setAttributes(new GUIAttribute[0]);
			prepareExtendedAttributes(null);
			object.setTemplateId(null);
		}
	}

	/**
	 * Instantiates a new DynamicForm suitable for editing extended attributes
	 * 
	 * @return a new instance of DynamicForm
	 */
	protected DynamicForm newAttributesForm() {
		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		return form;
	}

	private void refreshAttributesForm() {
		if (attributesForm != null) {
			vm.removeMembers(attributesForm);
			removeMember(attributesForm);
		}

		attributesForm = newAttributesForm();
		attributesForm.setValuesManager(vm);
		attributesForm.clearValues();
		attributesForm.clear();

		if (object.getAttributes() != null && object.getAttributes().length > 0 && extendedItems != null)
			attributesForm.setItems(extendedItems.toArray(new FormItem[0]));

		updateDependantAttributes();
		addMember(attributesForm);
	}

	/*
	 * Prepares the second form with the extended attributes
	 * 
	 * @param templateId identifier of the template
	 */
	private void prepareExtendedAttributes(final Long templateId) {
		extendedItems.clear();
		if (templateId == null) {
			refreshAttributesForm();
			return;
		}

		TemplateService.Instance.get().getAttributes(templateId, object, new AsyncCallback<GUIAttribute[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttribute[] templateAttributes) {
				// Update the object's attributes
				if (templateAttributes != null)
					updateAttributesFromTemplateDefinition(templateAttributes);

				adjustMultipleValueAttribues();

				displayAttributeItems();
			}
		});
	}

	private void updateAttributesFromTemplateDefinition(GUIAttribute[] templateAttributes) {
		for (GUIAttribute templAttr : templateAttributes) {
			GUIAttribute objAttr = object.getAttribute(templAttr.getName());
			if (objAttr != null) {
				objAttr.setEditor(templAttr.getEditor());
				objAttr.setHidden(templAttr.isHidden());
				objAttr.setReadonly(templAttr.isReadonly());
				objAttr.setLabel(templAttr.getLabel());
				objAttr.setMandatory(templAttr.isMandatory());
				objAttr.setMultiple(templAttr.isMultiple());
				objAttr.setOptions(templAttr.getOptions());
				objAttr.setSet(templAttr.getSet());
				objAttr.setSetId(templAttr.getSetId());
				objAttr.setParent(templAttr.getParent());
				objAttr.setStringValues(templAttr.getStringValues());
				objAttr.setPosition(templAttr.getPosition());
				objAttr.setType(templAttr.getType());

				if (object.getId() == 0L) {
					objAttr.setStringValue(templAttr.getStringValue());
					objAttr.setIntValue(templAttr.getIntValue());
					objAttr.setDateValue(templAttr.getDateValue());
					objAttr.setBooleanValue(templAttr.getBooleanValue());
					objAttr.setDoubleValue(templAttr.getDoubleValue());
				}
			} else {
				object.addAttribute(templAttr);
			}
		}
	}

	private void adjustMultipleValueAttribues() {
		if (object.getAttributes() != null) {
			for (GUIAttribute docAttr : object.getAttributes()) {
				if (docAttr.isMultiple()) {
					List<GUIAttribute> attrValues = object.getValues(docAttr.getName());
					if (attrValues != null)
						for (GUIAttribute val : attrValues) {
							val.setPosition(docAttr.getPosition());
							val.setHidden(docAttr.isHidden());
							val.setReadonly(docAttr.isReadonly());
							val.setMandatory(docAttr.isMandatory());
						}
				}
			}
			object.sortAttributes();
		}
	}

	private void displayAttributeItems() {
		extendedItems.clear();

		for (GUIAttribute att : object.getAttributes()) {
			if (att.isHidden())
				continue;

			FormItem item = prepareAttributeItem(att);
			if (item != null) {
				if (!updateEnabled)
					item.setDisabled(true);
				if (changedHandler != null)
					item.addChangedHandler(changedHandler);
				item.addChangedHandler(dependeciesHandler);
				extendedItems.add(item);
			}
		}

		refreshAttributesForm();
	}

	protected FormItem prepareAttributeItem(GUIAttribute att) {
		List<FormItemIcon> multiValIcons = prepareMultiValueIcons(att);

		FormItem item;
		if (att.getType() == GUIAttribute.TYPE_STRING) {
			att.setStringValue(object.getValue(att.getName()) != null ? (String) object.getValue(att.getName()) : null);
			item = ItemFactory.newStringItemForAttribute(att);
		} else if (att.getType() == GUIAttribute.TYPE_INT) {
			item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
			if (object.getValue(att.getName()) != null)
				item.setValue(object.getValue(att.getName()));
		} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
			item = prepareBooleanItem(att);
		} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
			item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
			item.setValue(object.getValue(att.getName()));
		} else if (att.getType() == GUIAttribute.TYPE_DATE) {
			item = prepareDateItem(att);
		} else if (att.getType() == GUIAttribute.TYPE_USER) {
			item = prepareUserItem(att, multiValIcons);
		} else if (att.getType() == GUIAttribute.TYPE_FOLDER) {
			item = prepareFolderItem(att, multiValIcons);
		} else {
			item = null;
		}

		prepareItemIconsAndVisibility(att, multiValIcons, item);

		return item;
	}

	private void prepareItemIconsAndVisibility(GUIAttribute att, List<FormItemIcon> multiValIcons, FormItem item) {
		if (item != null) {
			if ((att.isMultiple() || att.getParent() != null) && att.getType() != GUIAttribute.TYPE_USER
					&& att.getType() != GUIAttribute.TYPE_FOLDER)
				item.setIcons(multiValIcons.toArray(new FormItemIcon[0]));
			item.setRequired(checkMandatory && att.isMandatory());
			if (att.isReadonly())
				item.setDisabled(true);
		}
	}

	protected FormItem prepareUserItem(GUIAttribute att, List<FormItemIcon> multiValIcons) {
		FormItem item;
		item = ItemFactory.newUserSelectorForAttribute(att.getName(), att.getLabel(),
				(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null, multiValIcons);
		if (object.getValue(att.getName()) != null)
			item.setValue((object.getValue(att.getName()).toString()));
		return item;
	}

	protected FormItem prepareFolderItem(GUIAttribute att, List<FormItemIcon> multiValIcons) {
		FormItem item;
		item = ItemFactory.newFolderSelectorForAttribute(att.getName(), att.getLabel(), multiValIcons);
		FolderSelector selector = (FolderSelector) item;
		if (object.getValue(att.getName()) != null) {
			selector.setFolder(att.getIntValue(), att.getStringValue());
			item.setValue(att.getStringValue());
		}
		selector.addFolderChangeListener((GUIFolder folder) -> {
			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		return item;
	}

	protected FormItem prepareDateItem(GUIAttribute att) {
		FormItem item;
		item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
		if (object.getValue(att.getName()) != null)
			item.setValue((Date) object.getValue(att.getName()));
		item.addKeyPressHandler((KeyPressEvent event) -> {
			if ("backspace".equalsIgnoreCase(event.getKeyName()) || "delete".equalsIgnoreCase(event.getKeyName())) {
				item.clearValue();
				item.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});
		return item;
	}

	protected FormItem prepareBooleanItem(GUIAttribute att) {
		FormItem item;
		item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(),
				checkMandatory && !att.isMandatory());
		if (object.getValue(att.getName()) != null)
			item.setValue(((Boolean) object.getValue(att.getName())).booleanValue() ? "1" : "0");
		return item;
	}

	/**
	 * Prepares the additional icons to be displayed in case of a multi value
	 * attribute.
	 * 
	 * @param att the attribute to process
	 * 
	 * @return the list of additional icons
	 */
	private List<FormItemIcon> prepareMultiValueIcons(GUIAttribute att) {
		List<FormItemIcon> multiValIcons = new ArrayList<>();
		if (att.isMultiple()) {
			FormItemIcon add = new FormItemIcon();
			add.setWidth(10);
			add.setHeight(10);
			add.setSrc("[SKIN]/add.png");
			add.setPrompt(I18N.message("addvalue"));
			add.addFormItemClickHandler(event -> onValueAdd(att));

			FormItemIcon down = new FormItemIcon();
			down.setSrc("[SKIN]/down.png");
			down.setWidth(10);
			down.setHeight(10);
			down.setPrompt(I18N.message("movedown"));
			down.addFormItemClickHandler(event -> onValueShift(att, false));

			multiValIcons.add(add);
			multiValIcons.add(down);
		}

		if (att.getParent() != null && !att.getParent().isEmpty()) {
			FormItemIcon delete = new FormItemIcon();
			delete.setSrc("[SKIN]/delete.png");
			delete.setWidth(10);
			delete.setHeight(10);
			delete.setPrompt(I18N.message("remove"));
			delete.addFormItemClickHandler(event -> onValueDelete(att));

			FormItemIcon up = new FormItemIcon();
			up.setSrc("[SKIN]/up.png");
			up.setWidth(10);
			up.setHeight(10);
			up.setPrompt(I18N.message("moveup"));
			up.addFormItemClickHandler(event -> onValueShift(att, true));

			FormItemIcon down = new FormItemIcon();
			down.setSrc("[SKIN]/down.png");
			down.setWidth(10);
			down.setHeight(10);
			down.setPrompt(I18N.message("movedown"));
			down.addFormItemClickHandler(event -> onValueShift(att, false));

			multiValIcons.add(delete);
			multiValIcons.add(up);
			multiValIcons.add(down);
		}

		return multiValIcons;
	}

	public boolean validate() {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();
		vm.validate();

		if (Boolean.TRUE.equals(vm.hasErrors()))
			return false;

		if (isDocument() && allowTemplateSelection)
			((GUIDocument) object).setCustomId((String) values.get("customid"));

		validateExtendedAttributes();

		return !vm.hasErrors();
	}

	private void validateExtendedAttributes() {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();

		if (!Feature.enabled(Feature.TEMPLATE))
			return;

		if (allowTemplateSelection) {
			if (values.get(TEMPLATE) == null || values.get(TEMPLATE).toString().isEmpty())
				object.setTemplateId(null);
			else {
				object.setTemplateId(Long.parseLong(values.get(TEMPLATE).toString()));
			}
		}

		for (Map.Entry<String, Object> entry : values.entrySet()) {
			String itemName = entry.getKey();
			if (itemName.startsWith("_")) {
				String attributeName = itemName.substring(1).replace(Constants.BLANK_PLACEHOLDER, " ");

				GUIAttribute attribute = object.getAttribute(attributeName);
				if (attribute == null || attribute.isHidden())
					continue;

				validateExtendedAttribute(itemName, entry.getValue(), attributeName, attribute);
			}
		}

		if (object.getAttributes() != null)
			fixNullValues();
	}

	/*
	 * Sometimes empty fields are not included in the value map, so we should
	 * assign null value.
	 */
	private void fixNullValues() {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();

		// Check the current doc's values
		for (GUIAttribute att : object.getAttributes()) {
			if (att.isHidden())
				continue;

			boolean found = false;
			// For each one check if it was included in the form
			// values
			for (String name : values.keySet()) {
				// Get back the name of the attribute from the form
				// item's name
				String nm = name.substring(1).replace(Constants.BLANK_PLACEHOLDER, " ");
				if (nm.equals(att.getName())) {
					found = true;
					break;
				}
			}
			if (!found) {
				att.setValue(null);
			}
		}
	}

	private void validateExtendedAttribute(String itemName, Object value, String attributeName,
			GUIAttribute attribute) {
		if (value != null) {
			validateNotNulItem(itemName, value, attributeName, attribute);
		} else {
			validateNullItem(attributeName, attribute);
		}
	}

	private void validateNullItem(String attributeName, GUIAttribute attribute) {
		if (attribute != null) {
			if (attribute.getType() == GUIAttribute.TYPE_INT) {
				object.getAttribute(attributeName).setIntValue(null);
			} else if (attribute.getType() == GUIAttribute.TYPE_BOOLEAN) {
				object.getAttribute(attributeName).setBooleanValue(null);
			} else if (attribute.getType() == GUIAttribute.TYPE_DOUBLE) {
				object.getAttribute(attributeName).setDoubleValue(null);
			} else if (attribute.getType() == GUIAttribute.TYPE_DATE) {
				object.getAttribute(attributeName).setDateValue(null);
				attribute.setDateValue(null);
			} else if (attribute.getType() == GUIAttribute.TYPE_USER
					|| attribute.getType() == GUIAttribute.TYPE_FOLDER) {
				GUIAttribute at = object.getAttribute(attributeName);
				at.setIntValue(null);
				at.setStringValue(null);
				at.setType(attribute.getType());
			} else {
				object.setValue(attributeName, (String) null);
			}
		}
	}

	private void validateNotNulItem(String itemName, Object value, String attributeName, GUIAttribute attribute) {
		if (attribute.getType() == GUIAttribute.TYPE_USER) {
			validateUser(itemName, value, attributeName);
		} else if (attribute.getType() == GUIAttribute.TYPE_FOLDER) {
			validateFolder(itemName, attributeName);
		} else if (attribute.getType() == GUIAttribute.TYPE_BOOLEAN) {
			validateBoolean(value, attributeName);
		} else {
			object.setValue(attributeName, value);
			InputValues.saveInput(itemName, value);
		}
	}

	private void validateBoolean(Object value, String attributeName) {
		if (!(value == null || "".equals(value.toString().trim())))
			object.setValue(attributeName, "1".equals(value.toString().trim()));
		else if (object.getAttribute(attributeName) != null) {
			GUIAttribute at = object.getAttribute(attributeName);
			at.setBooleanValue(null);
			at.setType(GUIAttribute.TYPE_BOOLEAN);
		}
	}

	private void validateFolder(String itemName, String attributeName) {
		FolderSelector folderItem = (FolderSelector) attributesForm.getItem(itemName);
		GUIFolder selectedFolder = folderItem.getFolder();
		if (selectedFolder != null) {
			object.setValue(attributeName, selectedFolder);
		} else {
			GUIAttribute at = object.getAttribute(attributeName);
			at.setIntValue(null);
			at.setStringValue(null);
			at.setType(GUIAttribute.TYPE_FOLDER);
		}
	}

	private void validateUser(String itemName, Object value, String attributeName) {
		SelectItem userItem = (SelectItem) attributesForm.getItem(itemName);
		if (userItem.getValue() != null && !"".equals(userItem.getValue())) {
			ListGridRecord sel = userItem.getSelectedRecord();

			// Prepare a dummy user to set as
			// attribute value
			GUIUser dummy = new GUIUser();
			dummy.setId(Long.parseLong(value.toString()));
			dummy.setUsername(sel.getAttributeAsString("username"));
			dummy.setFirstName(sel.getAttributeAsString("firstName"));
			dummy.setName(sel.getAttributeAsString("name"));
			object.setValue(attributeName, dummy);

			GUIAttribute at = object.getAttribute(attributeName);
			at.setStringValue(dummy.getUsername());
			at.setUsername(dummy.getUsername());
		} else {
			GUIAttribute at = object.getAttribute(attributeName);
			at.setIntValue(null);
			at.setStringValue(null);
			at.setType(GUIAttribute.TYPE_USER);
		}
	}

	private void onValueDelete(GUIAttribute att) {
		// Copy the current filled values into the attributes
		validateExtendedAttributes();

		// Remove the attribute and re-display
		object.removeAttribute(att.getName());
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	private void onValueAdd(GUIAttribute att) {
		copyValuesToObject();

		// Add the new attribute and redisplay
		object.addAttributeValue(att.getName());
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	private void onValueShift(GUIAttribute att, boolean up) {
		copyValuesToObject();
		object.shiftValue(att.getName(), up);
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	/**
	 * Copies the actual form's values to the object without validating.
	 */
	private void copyValuesToObject() {
		@SuppressWarnings("unchecked")
		Map<String, Object> valuesMap = attributesForm.getValues();

		for (Map.Entry<String, Object> entry : valuesMap.entrySet()) {
			String name = entry.getKey();
			if (name.startsWith("_")) {
				String nm = name.substring(1).replace(Constants.BLANK_PLACEHOLDER, " ");
				Object val = entry.getValue();
				FormItem item = attributesForm.getItem(name);

				GUIAttribute attribute = object.getAttribute(nm);
				int originalType = attribute.getType();

				if (item instanceof UserSelector) {
					copyUserValue(item, attribute);
				} else if (item instanceof FolderSelector) {
					copyFolderValue(item, attribute);
				} else if (attribute.getType() == GUIAttribute.TYPE_BOOLEAN) {
					object.setValue(nm, val != null && ("1".equals(val.toString()) || "yes".equals(val.toString())));
				} else {
					object.setValue(nm, val);
				}

				attribute.setType(originalType);
			}
		}
	}

	private void copyFolderValue(FormItem item, GUIAttribute attribute) {
		FolderSelector userSelector = (FolderSelector) item;
		GUIFolder folder = userSelector.getFolder();
		if (folder != null) {
			attribute.setIntValue(folder.getId());
			attribute.setStringValue(folder.getName());
		} else {
			attribute.setIntValue(null);
			attribute.setStringValue(null);
		}
	}

	private void copyUserValue(FormItem item, GUIAttribute attribute) {
		UserSelector userSelector = (UserSelector) item;
		GUIUser user = userSelector.getUser();
		if (user != null) {
			attribute.setIntValue(user.getId());
			attribute.setStringValue(user.getUsername());
		} else {
			attribute.setIntValue(null);
			attribute.setStringValue(null);
		}
	}

	private boolean isDocument() {
		return object instanceof GUIDocument;
	}

	public GUIExtensibleObject getObject() {
		return object;
	}

	public void onErrors(ServerValidationError[] errors) {
		for (ServerValidationError error : errors) {
			if (error.getAttribute().isEmpty())
				continue;
			vm.setFieldErrors(ItemFactory.itemNameForAttribute(error.getAttribute()), error.getDescription(), true);
		}
	}

	/**
	 * Used when the form gets displayed the first time to update the options
	 * lists according on the dependencies
	 */
	private void updateDependantAttributes() {
		for (GUIAttribute att : object.getAttributes()) {
			String dependsOn = att.getDependsOn();
			if (dependsOn != null && !dependsOn.isEmpty()) {
				FormItem item = vm.getItem(ItemFactory.itemNameForAttribute(att.getName()));
				if (item instanceof SelectItem select) {
					select.setPickListFilterCriteriaFunction(itemContext -> {
						String category = vm.getValueAsString(ItemFactory.itemNameForAttribute(dependsOn));
						return new Criteria("category", category);
					});
				} else if (item instanceof ComboBoxItem combo) {
					combo.setPickListFilterCriteriaFunction(itemContext -> {
						String category = vm.getValueAsString(ItemFactory.itemNameForAttribute(dependsOn));
						return new Criteria("category", category);
					});
				}
			}
		}
	}

	/**
	 * An handler to clear the options lists according to the dependencies
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.8.2
	 */
	class ClearDependantAttributes implements ChangedHandler {

		@Override
		public void onChanged(ChangedEvent event) {
			for (GUIAttribute att : object.getAttributes()) {
				String dependsOn = att.getDependsOn();
				if (dependsOn != null && !dependsOn.isEmpty()) {
					FormItem item = vm.getItem(ItemFactory.itemNameForAttribute(att.getName()));
					FormItem editedItem = event.getItem();
					String editedAttributeName = editedItem.getName().substring(1).replace(Constants.BLANK_PLACEHOLDER,
							" ");

					if (dependsOn.equals(editedAttributeName) && event.getItem() != null
							&& !event.getItem().equals(item)) {
						/*
						 * Clear the currently selected item so the user can
						 * choose an option from the new list
						 */
						item.clearValue();
					}
				}
			}
		}
	}
}