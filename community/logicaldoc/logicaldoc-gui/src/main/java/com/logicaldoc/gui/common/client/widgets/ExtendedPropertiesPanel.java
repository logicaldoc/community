package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.InputValues;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * A panel for editing extended attributes of an extensible object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ExtendedPropertiesPanel extends HLayout {

	private GUIExtensibleObject object;

	private ChangedHandler changedHandler;

	private ChangedHandler templateChangedHandler;

	private boolean updateEnabled = false;

	private boolean checkMandatory = false;

	private boolean allowTemplateSelection = true;

	private DynamicForm templateForm = new DynamicForm();

	private DynamicForm attributesForm = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private List<FormItem> standardItems = new ArrayList<FormItem>();

	private SelectItem templateItem = null;

	private List<FormItem> extendedItems = new ArrayList<FormItem>();

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

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				adaptForms();
			}
		});
	}

	public ExtendedPropertiesPanel(GUIExtensibleObject object, ChangedHandler changedHandler, boolean updateEnabled,
			boolean checkMandatory, boolean allowTemplateSelection) {
		this(object, changedHandler, null, updateEnabled, checkMandatory, allowTemplateSelection);
	}

	private void adaptForms() {
		if (templateItem != null && templateItem.getValue() != null) {
			int maxExtCols = ((int) getWidth() - 500) / 160; // 160 = length of
																// an item
			int maxExtRows = (int) getHeight() / 46; // 46 = height of an item
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

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);
		extendedItems.clear();

		if (templateForm != null)
			templateForm.destroy();

		if (contains(templateForm))
			removeChild(templateForm);
		templateForm = new DynamicForm();
		templateForm.setValuesManager(vm);
		templateForm.setTitleOrientation(TitleOrientation.TOP);
		templateForm.setNumCols(1);
		standardItems.clear();

		if (isDocument()) {
			TextItem customId = ItemFactory.newTextItem("customid", "customid", ((GUIDocument) object).getCustomId());
			if (changedHandler != null)
				customId.addChangedHandler(changedHandler);
			customId.setDisabled(!updateEnabled);
			standardItems.add(customId);
		}

		templateItem = ItemFactory.newTemplateSelector(true, null);
		if (changedHandler != null)
			templateItem.addChangedHandler(changedHandler);
		templateItem.setMultiple(false);
		try {
			templateItem.setDisabled(!updateEnabled || (isDocument() && ((GUIDocument) object).getFolder() != null
					&& ((GUIDocument) object).getFolder().getTemplateLocked() == 1));
		} catch (Throwable t) {
			SC.warn(t.getMessage());
		}
		if (object.getTemplateId() != null)
			templateItem.setValue(object.getTemplateId().toString());

		templateItem.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (templateItem.getValue() != null && !"".equals(templateItem.getValue().toString())) {
					object.setAttributes(new GUIAttribute[0]);
					long templateId = Long.parseLong(event.getValue().toString());
					prepareExtendedAttributes(templateId);
					object.setTemplateId(templateId);
				} else {
					object.setAttributes(new GUIAttribute[0]);
					prepareExtendedAttributes(null);
					object.setTemplateId(null);
				}
			}
		});

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

	private void refreshAttributesForm() {
		if (attributesForm != null) {
			vm.removeMembers(attributesForm);
			removeMember(attributesForm);
		}

		attributesForm = new DynamicForm();
		attributesForm.setValuesManager(vm);
		attributesForm.setTitleOrientation(TitleOrientation.TOP);
		attributesForm.clearValues();
		attributesForm.clear();

		if (object.getAttributes() != null && object.getAttributes().length > 0)
			attributesForm.setItems(extendedItems.toArray(new FormItem[0]));

		addMember(attributesForm);
	}

	/*
	 * Prepare the second form for the extended attributes
	 * 
	 * @param templateId identifier of the template
	 */
	private void prepareExtendedAttributes(final Long templateId) {
		extendedItems.clear();
		if (templateId == null) {
			refreshAttributesForm();
			return;
		}

		DocumentService.Instance.get().getAttributes(templateId, new AsyncCallback<GUIAttribute[]>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttribute[] result) {
				// Update the object's attributes
				if (result != null)
					for (GUIAttribute templAttr : result) {
						GUIAttribute objAttr = object.getAttribute(templAttr.getName());
						if (objAttr != null) {
							objAttr.setEditor(templAttr.getEditor());
							objAttr.setHidden(templAttr.isHidden());
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
						} else {
							object.addAttribute(templAttr);
						}
					}

				if (object.getAttributes() != null) {
					for (GUIAttribute docAttr : object.getAttributes()) {
						if (docAttr.isMultiple()) {
							List<GUIAttribute> attrValues = object.getValues(docAttr.getName());
							if (attrValues != null)
								for (GUIAttribute val : attrValues) {
									val.setPosition(docAttr.getPosition());
									val.setHidden(docAttr.isHidden());
									val.setMandatory(docAttr.isMandatory());
								}
						}
					}

					object.sortAttributes();
				}

				displayAttributeItems();
			}
		});
	}

	private void displayAttributeItems() {
		extendedItems.clear();

		for (GUIAttribute att : object.getAttributes()) {
			if (att.isHidden())
				continue;

			FormItem item = prepareAttributeItem(att);
			if (item != null) {
				item.setDisabled(!updateEnabled);
				if (changedHandler != null)
					item.addChangedHandler(changedHandler);
				extendedItems.add(item);
			}
		}

		refreshAttributesForm();
	}

	private FormItem prepareAttributeItem(GUIAttribute att) {
		List<FormItemIcon> multiValIcons = prepareMultiValueIcons(att);

		FormItem item;
		if (att.getType() == GUIAttribute.TYPE_STRING) {
			att.setStringValue(object.getValue(att.getName()) != null ? (String) object.getValue(att.getName()) : null);
			item = ItemFactory.newStringItemForAttribute(att);
		} else if (att.getType() == GUIAttribute.TYPE_INT) {
			item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
			if (object.getValue(att.getName()) != null)
				item.setValue((Long) object.getValue(att.getName()));
		} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
			item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(),
					checkMandatory ? !att.isMandatory() : true);
			if (object.getValue(att.getName()) != null)
				item.setValue(((Boolean) object.getValue(att.getName())).booleanValue() ? "1" : "0");
		} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
			item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
			item.setValue((Double) object.getValue(att.getName()));
		} else if (att.getType() == GUIAttribute.TYPE_DATE) {
			item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
			if (object.getValue(att.getName()) != null)
				item.setValue((Date) object.getValue(att.getName()));
			item.addKeyPressHandler(new KeyPressHandler() {
				@Override
				public void onKeyPress(KeyPressEvent event) {
					if ("backspace".equals(event.getKeyName().toLowerCase())
							|| "delete".equals(event.getKeyName().toLowerCase())) {
						item.clearValue();
						item.setValue((Date) null);
						changedHandler.onChanged(null);
					} else {
						changedHandler.onChanged(null);
					}
				}
			});
		} else if (att.getType() == GUIAttribute.TYPE_USER) {
			item = ItemFactory.newUserSelectorForAttribute(att.getName(), att.getLabel(),
					(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null,
					multiValIcons);
			if (object.getValue(att.getName()) != null)
				item.setValue((object.getValue(att.getName()).toString()));
		} else if (att.getType() == GUIAttribute.TYPE_FOLDER) {
			item = ItemFactory.newFolderSelectorForAttribute(att.getName(), att.getLabel(),
					checkMandatory ? !att.isMandatory() : true, multiValIcons);
			FolderSelector selector = (FolderSelector) item;
			if (object.getValue(att.getName()) != null) {
				selector.setFolder(att.getIntValue(), att.getStringValue());
				item.setValue(att.getStringValue());
			}
			selector.addFolderChangeListener(new FolderChangeListener() {

				@Override
				public void onChanged(GUIFolder folder) {
					if (changedHandler != null)
						changedHandler.onChanged(null);
				}
			});
		} else {
			item = null;
		}

		if (item != null) {
			if ((att.isMultiple() || att.getParent() != null) && att.getType() != GUIAttribute.TYPE_USER
					&& att.getType() != GUIAttribute.TYPE_FOLDER)
				item.setIcons(multiValIcons.toArray(new FormItemIcon[0]));
			item.setRequired(checkMandatory ? att.isMandatory() : false);
		}

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
		List<FormItemIcon> multiValIcons = new ArrayList<FormItemIcon>();
		if (att.isMultiple()) {
			FormItemIcon add = new FormItemIcon();
			add.setWidth(10);
			add.setHeight(10);
			add.setSrc("[SKIN]/add.png");
			add.setPrompt(I18N.message("addvalue"));
			add.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					onValueAdd(att);
				}
			});

			FormItemIcon down = new FormItemIcon();
			down.setSrc("[SKIN]/down.png");
			down.setWidth(10);
			down.setHeight(10);
			down.setPrompt(I18N.message("movedown"));
			down.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					onValueShift(att, false);
				}
			});

			multiValIcons.add(add);
			multiValIcons.add(down);
		}

		if (att.getParent() != null && !att.getParent().isEmpty()) {
			FormItemIcon delete = new FormItemIcon();
			delete.setSrc("[SKIN]/delete.png");
			delete.setWidth(10);
			delete.setHeight(10);
			delete.setPrompt(I18N.message("remove"));
			delete.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					onValueDelete(att);
				}
			});

			FormItemIcon up = new FormItemIcon();
			up.setSrc("[SKIN]/up.png");
			up.setWidth(10);
			up.setHeight(10);
			up.setPrompt(I18N.message("moveup"));
			up.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					onValueShift(att, true);
				}
			});

			FormItemIcon down = new FormItemIcon();
			down.setSrc("[SKIN]/down.png");
			down.setWidth(10);
			down.setHeight(10);
			down.setPrompt(I18N.message("movedown"));
			down.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					onValueShift(att, false);
				}
			});

			multiValIcons.add(delete);
			multiValIcons.add(up);
			multiValIcons.add(down);
		}

		return multiValIcons;
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();

		if (!vm.hasErrors()) {
			if (isDocument() && allowTemplateSelection)
				((GUIDocument) object).setCustomId((String) values.get("customid"));

			if (Feature.enabled(Feature.TEMPLATE)) {
				if (allowTemplateSelection) {
					if (values.get("template") == null || values.get("template").toString().isEmpty())
						object.setTemplateId(null);
					else {
						object.setTemplateId(Long.parseLong(values.get("template").toString()));
					}
				}

				for (String name : values.keySet()) {
					try {
						if (name.startsWith("_")) {
							Object val = values.get(name);
							String nm = name.substring(1).replaceAll(Constants.BLANK_PLACEHOLDER, " ");

							GUIAttribute att = object.getAttribute(nm);
							if (att == null || att.isHidden())
								continue;

							if (val != null) {
								if (att.getType() == GUIAttribute.TYPE_USER) {
									SelectItem userItem = (SelectItem) attributesForm.getItem(name);
									if (userItem.getValue() != null && !"".equals(userItem.getValue())) {
										ListGridRecord sel = userItem.getSelectedRecord();

										// Prepare a dummy user to set as
										// attribute value
										GUIUser dummy = new GUIUser();
										dummy.setId(Long.parseLong(val.toString()));
										dummy.setUsername(sel.getAttributeAsString("username"));
										dummy.setFirstName(sel.getAttributeAsString("firstName"));
										dummy.setName(sel.getAttributeAsString("name"));
										object.setValue(nm, dummy);

										GUIAttribute at = object.getAttribute(nm);
										at.setStringValue(dummy.getUsername());
										at.setUsername(dummy.getUsername());
									} else {
										GUIAttribute at = object.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_USER);
									}
								} else if (att.getType() == GUIAttribute.TYPE_FOLDER) {
									FolderSelector folderItem = (FolderSelector) attributesForm.getItem(name);
									GUIFolder selectedFolder = folderItem.getFolder();
									if (selectedFolder != null) {
										object.setValue(nm, selectedFolder);
									} else {
										GUIAttribute at = object.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_FOLDER);
									}
								} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
									if (!(val == null || "".equals(val.toString().trim())))
										object.setValue(nm, "1".equals(val.toString().trim()) ? true : false);
									else if (object.getAttribute(nm) != null) {
										GUIAttribute at = object.getAttribute(nm);
										at.setBooleanValue(null);
										at.setType(GUIAttribute.TYPE_BOOLEAN);
									}
								} else {
									object.setValue(nm, val);
									InputValues.saveInput(name, val);
								}
							} else {
								if (att != null) {
									if (att.getType() == GUIAttribute.TYPE_INT) {
										object.getAttribute(nm).setIntValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
										object.getAttribute(nm).setBooleanValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
										object.getAttribute(nm).setDoubleValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DATE) {
										object.getAttribute(nm).setDateValue(null);
										att.setDateValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_USER
											|| att.getType() == GUIAttribute.TYPE_FOLDER) {
										GUIAttribute at = object.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(att.getType());
									} else {
										object.setValue(nm, (String) null);
									}
								}
							}
						}
					} catch (Throwable t) {
					}
				}

				/*
				 * Sometimes empty fields are not included in the value map, so
				 * we should assign null value.
				 */
				if (object.getAttributes() != null)
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
							String nm = name.substring(1).replaceAll(Constants.BLANK_PLACEHOLDER, " ");
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
		}
		return !vm.hasErrors();
	}

	private void onValueDelete(GUIAttribute att) {
		object.removeAttribute(att.getName());
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	private void onValueAdd(GUIAttribute att) {
		copyValuesToObject(att);

		// Add the new attribute and redisplay
		object.addAttributeValue(att.getName());
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	private void onValueShift(GUIAttribute att, boolean up) {
		copyValuesToObject(att);
		object.shiftValue(att.getName(), up);
		displayAttributeItems();
		changedHandler.onChanged(null);
	}

	/**
	 * Copies the actual form's values to the object without validating.
	 */
	private void copyValuesToObject(GUIAttribute att) {
		@SuppressWarnings("unchecked")
		Map<String, Object> valuesMap = (Map<String, Object>) attributesForm.getValues();
		for (String name : valuesMap.keySet()) {
			if (name.startsWith("_")) {
				String nm = name.substring(1).replaceAll(Constants.BLANK_PLACEHOLDER, " ");
				Object val = valuesMap.get(name);
				FormItem item = attributesForm.getItem(name);
				if (item instanceof UserSelector) {
					GUIAttribute attr = object.getAttribute(nm);
					UserSelector userSelector = (UserSelector) item;
					GUIUser user = userSelector.getUser();
					attr.setIntValue(user.getId());
					attr.setStringValue(user.getUsername());
					attr.setType(att.getType());
				} else if (item instanceof FolderSelector) {
					GUIAttribute attr = object.getAttribute(nm);
					FolderSelector userSelector = (FolderSelector) item;
					GUIFolder folder = userSelector.getFolder();
					attr.setIntValue(folder.getId());
					attr.setStringValue(folder.getName());
					attr.setType(att.getType());
				} else {
					object.setValue(nm, val);
				}
			}
		}
	}

	private boolean isDocument() {
		return object instanceof GUIDocument;
	}

	public GUIExtensibleObject getObject() {
		return object;
	}
}