package com.logicaldoc.gui.frontend.client.document;

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
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.EditorExitEvent;
import com.smartgwt.client.widgets.form.fields.events.EditorExitHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ExtendedPropertiesPanel extends DocumentDetailTab {
	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIAttribute[] currentExtAttributes = null;

	private List<FormItem> standardItems = new ArrayList<FormItem>();

	private SelectItem templateItem = null;

	private List<FormItem> extendedItems = new ArrayList<FormItem>();

	public ExtendedPropertiesPanel(GUIDocument document, ChangedHandler changedHandler) {
		super(document, changedHandler);
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

	private void adaptForms() {
		if (templateItem.getValue() != null) {
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

			form2.setNumCols(maxExtCols);
		}
	}

	private GUIAttribute getExtendedAttribute(String name) {
		if (currentExtAttributes != null)
			for (GUIAttribute extAttr : currentExtAttributes)
				if (extAttr.getName().equals(name))
					return extAttr;
		return null;
	}

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);
		extendedItems.clear();

		if (form1 != null)
			form1.destroy();

		if (contains(form1))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setNumCols(1);
		standardItems.clear();

		TextItem customId = ItemFactory.newTextItem("customid", "customid", document.getCustomId());
		customId.addChangedHandler(changedHandler);
		customId.setDisabled(!updateEnabled);

		templateItem = ItemFactory.newTemplateSelector(true, null);
		templateItem.addChangedHandler(changedHandler);
		templateItem.setMultiple(false);
		templateItem.setDisabled(!updateEnabled || document.getFolder().getTemplateLocked() == 1);
		if (document.getTemplateId() != null)
			templateItem.setValue(document.getTemplateId().toString());

		templateItem.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (templateItem.getValue() != null && !"".equals(templateItem.getValue().toString())) {
					document.setAttributes(new GUIAttribute[0]);
					prepareExtendedAttributes(new Long(event.getValue().toString()));
				} else {
					document.setAttributes(new GUIAttribute[0]);
					prepareExtendedAttributes(null);
				}
			}
		});

		standardItems.add(customId);

		if (Feature.visible(Feature.TEMPLATE)) {
			standardItems.add(templateItem);

			if (!Feature.enabled(Feature.TEMPLATE)) {
				templateItem.setDisabled(true);
				templateItem.setTooltip(I18N.message("featuredisabled"));
			}
		}

		form1.setItems(standardItems.toArray(new FormItem[0]));
		addMember(form1);

		if (Feature.enabled(Feature.TEMPLATE))
			prepareExtendedAttributes(document.getTemplateId());
	}

	/*
	 * Prepare the second form for the extended attributes
	 */
	private void prepareExtendedAttributes(final Long templateId) {
		if (form2 != null) {
			vm.removeMembers(form2);
			removeMember(form2);
		}

		form2 = new DynamicForm();
		form2.setValuesManager(vm);
		form2.setTitleOrientation(TitleOrientation.TOP);
		form2.clearValues();
		form2.clear();
		addMember(form2);

		if (templateId == null)
			return;

		DocumentService.Instance.get().getAttributes(templateId, new AsyncCallback<GUIAttribute[]>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttribute[] result) {
				currentExtAttributes = result;
				extendedItems.clear();

				for (GUIAttribute att : result) {
					final FormItem item;
					if (att.getType() == GUIAttribute.TYPE_STRING) {
						item = ItemFactory.newStringItemForAttribute(att);
						if (document.getValue(att.getName()) != null)
							item.setValue((String) document.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_INT) {
						item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
						if (document.getValue(att.getName()) != null)
							item.setValue((Long) document.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
						item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(),
								!att.isMandatory());
						if (document.getValue(att.getName()) != null)
							item.setValue(((Boolean) document.getValue(att.getName())).booleanValue() ? "1" : "0");
					} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
						item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
						item.setValue((Double) document.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_DATE) {
						item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
						if (document.getValue(att.getName()) != null)
							item.setValue((Date) document.getValue(att.getName()));
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
								(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null);
						if (document.getValue(att.getName()) != null)
							item.setValue((document.getValue(att.getName()).toString()));
					} else {
						item = null;
					}

					if (item != null) {
						item.setRequired(att.isMandatory());
						item.setDisabled(!updateEnabled);
						if (att.getType() == GUIAttribute.TYPE_STRING || att.getType() == GUIAttribute.TYPE_INT
								|| att.getType() == GUIAttribute.TYPE_DOUBLE) {
							/*
							 * Due to a bug in the skin, when you edit the field
							 * if the save panel appears, the cursor goes to the
							 * end of the text or to the next item. So we use
							 * the ExitorExit event, it's safer.
							 */
							item.addEditorExitHandler(new EditorExitHandler() {

								@Override
								public void onEditorExit(EditorExitEvent event) {
									changedHandler.onChanged(null);
								}
							});
						} else
							item.addChangedHandler(changedHandler);
						extendedItems.add(item);
					}
				}

				form2.setItems(extendedItems.toArray(new FormItem[0]));
			}

		});
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();

		if (!vm.hasErrors()) {
			document.setCustomId((String) values.get("customid"));
			if (Feature.enabled(Feature.TEMPLATE)) {
				if (values.get("template") == null || values.get("template").toString().isEmpty())
					document.setTemplateId(null);
				else {
					document.setTemplateId(Long.parseLong(values.get("template").toString()));
				}
				for (String name : values.keySet()) {
					try {
						if (name.startsWith("_")) {
							Object val = values.get(name);
							String nm = name.substring(1).replaceAll(Constants.BLANK_PLACEHOLDER, " ");
							GUIAttribute att = getExtendedAttribute(nm);
							if (att == null)
								continue;

							if (val != null) {
								if (att.getType() == GUIAttribute.TYPE_USER) {
									SelectItem userItem = (SelectItem) form2.getItem(name);
									if (userItem.getValue() != null && !"".equals(userItem.getValue())) {
										ListGridRecord sel = userItem.getSelectedRecord();

										// Prepare a dummy user to set as
										// attribute
										// value
										GUIUser dummy = new GUIUser();
										dummy.setId(Long.parseLong(val.toString()));
										dummy.setFirstName(sel.getAttributeAsString("firstName"));
										dummy.setName(sel.getAttributeAsString("name"));
										document.setValue(nm, dummy);
									} else {
										GUIAttribute at = document.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_USER);
									}
								} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
									if (!(val == null || "".equals(val.toString().trim())))
										document.setValue(nm, "1".equals(val.toString().trim()) ? true : false);
									else if (document.getAttribute(nm) != null) {
										GUIAttribute at = document.getAttribute(nm);
										at.setBooleanValue(null);
										at.setType(GUIAttribute.TYPE_BOOLEAN);
									}
								} else {
									document.setValue(nm, val);
									InputValues.saveInput(name, val);
								}
							} else {
								if (att != null) {
									if (att.getType() == GUIAttribute.TYPE_INT) {
										document.getAttribute(nm).setIntValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
										document.getAttribute(nm).setBooleanValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
										document.getAttribute(nm).setDoubleValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DATE) {
										document.getAttribute(nm).setDateValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_USER) {
										GUIAttribute at = document.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_USER);
									} else {
										document.setValue(nm, (String) null);
									}
								}
							}
						}
					} catch (Throwable t) {

					}
				}
			}
		}
		return !vm.hasErrors();
	}
}