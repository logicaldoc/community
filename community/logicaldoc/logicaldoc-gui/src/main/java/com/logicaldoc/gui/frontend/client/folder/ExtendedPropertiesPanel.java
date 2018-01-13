package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.InputValues;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Shows folder's optional template metadata
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ExtendedPropertiesPanel extends FolderDetailTab {
	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIAttribute[] currentExtAttributes = null;

	private boolean update = false;

	private SelectItem templateItem = null;

	private List<FormItem> extendedItems = new ArrayList<FormItem>();

	public ExtendedPropertiesPanel(GUIFolder folder, ChangedHandler changedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);
		update = folder.hasPermission(Constants.PERMISSION_RENAME);
		refresh();

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				adaptForms();
			}
		});
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
		form1.setWidth(200);
		form1.setNumCols(1);
		form1.setValuesManager(vm);
		form2.setBorder("1px solid red");
		form1.setTitleOrientation(TitleOrientation.TOP);

		List<FormItem> items = new ArrayList<FormItem>();

		templateItem = ItemFactory.newTemplateSelector(true, null);
		templateItem.addChangedHandler(changedHandler);
		templateItem.setMultiple(false);
		templateItem.setDisabled(!update);
		templateItem.setEndRow(true);
		if (folder.getTemplateId() != null)
			templateItem.setValue(folder.getTemplateId().toString());
		templateItem.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (templateItem.getValue() != null && !"".equals(templateItem.getValue().toString())) {
					folder.setAttributes(new GUIAttribute[0]);
					prepareExtendedAttributes(new Long(event.getValue().toString()));
				} else {
					folder.setAttributes(new GUIAttribute[0]);
					prepareExtendedAttributes(null);
				}
			}
		});

		RadioGroupItem locked = ItemFactory.newBooleanSelector("locked", "templatelocked");
		locked.setValue(folder.getTemplateLocked() == 1 ? "yes" : "no");
		locked.addChangedHandler(changedHandler);
		locked.setEndRow(true);

		ButtonItem applyMetadata = new ButtonItem(I18N.message("applytosubfolders"));
		applyMetadata.setAutoFit(true);
		applyMetadata.setEndRow(true);
		applyMetadata.setDisabled(!update);
		applyMetadata.setColSpan(1);
		applyMetadata.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				FolderService.Instance.get().applyMetadata(folder.getId(), new AsyncCallback<Void>() {

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

		if (Feature.visible(Feature.TEMPLATE)) {
			items.add(templateItem);
			items.add(locked);
			items.add(applyMetadata);
			if (!Feature.enabled(Feature.TEMPLATE)) {
				templateItem.setDisabled(true);
				templateItem.setTooltip(I18N.message("featuredisabled"));
			}
		}

		form1.setItems(items.toArray(new FormItem[0]));
		addMember(form1);

		if (Feature.enabled(Feature.TEMPLATE))
			prepareExtendedAttributes(folder.getTemplateId());
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
						if (folder.getValue(att.getName()) != null)
							item.setValue((String) folder.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_INT) {
						item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
						if (folder.getValue(att.getName()) != null)
							item.setValue((Long) folder.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
						item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(), true);
						if (folder.getValue(att.getName()) != null)
							item.setValue(((Boolean) folder.getValue(att.getName())).booleanValue() ? "1" : "0");
					} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
						item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
						if (folder.getValue(att.getName()) != null)
							item.setValue((Double) folder.getValue(att.getName()));
					} else if (att.getType() == GUIAttribute.TYPE_DATE) {
						item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
						if (folder.getValue(att.getName()) != null)
							item.setValue((Date) folder.getValue(att.getName()));
						item.addChangedHandler(changedHandler);
						item.addKeyPressHandler(new KeyPressHandler() {
							@Override
							public void onKeyPress(KeyPressEvent event) {
								if ("backspace".equals(event.getKeyName().toLowerCase())
										|| "delete".equals(event.getKeyName().toLowerCase())) {
									((DateItem) item).clearValue();
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
						if (folder.getValue(att.getName()) != null)
							item.setValue((folder.getValue(att.getName()).toString()));
						item.addChangedHandler(changedHandler);
						item.setDisabled(!update);
						item.setRequired(false);
					} else {
						item = null;
					}

					if (item != null) {
						item.addChangedHandler(changedHandler);
						item.setDisabled(!update);
						item.setRequired(false);
						extendedItems.add(item);
					}
				}
				form2.setItems(extendedItems.toArray(new FormItem[0]));
			}
		});
	}

	private GUIAttribute getExtendedAttribute(String name) {
		if (currentExtAttributes != null)
			for (GUIAttribute extAttr : currentExtAttributes)
				if (extAttr.getName().equals(name))
					return extAttr;
		return null;
	}

	private void adaptForms() {
		if (templateItem.getValue() != null) {
			int maxExtCols = ((int) getWidth() - 200) / 160; // 160 = length of
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

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {

			if (Feature.enabled(Feature.TEMPLATE)) {
				if (values.get("template") == null || "".equals(values.get("template"))) {
					folder.setTemplateId(null);
					folder.setTemplateLocked(0);
				} else {
					folder.setTemplateId(Long.parseLong(values.get("template").toString()));
					folder.setTemplateLocked("yes".equals(values.get("locked")) ? 1 : 0);
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
										folder.setValue(nm, dummy);
									} else {
										GUIAttribute at = folder.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_USER);
									}
								} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
									if (!(val == null || "".equals(val.toString().trim())))
										folder.setValue(nm, "1".equals(val.toString().trim()) ? true : false);
									else if (folder.getAttribute(nm) != null) {
										GUIAttribute at = folder.getAttribute(nm);
										at.setBooleanValue(null);
										at.setType(GUIAttribute.TYPE_BOOLEAN);
									}
								} else {
									folder.setValue(nm, val);
									InputValues.saveInput(name, val);
								}
							} else {
								if (att != null) {
									if (att.getType() == GUIAttribute.TYPE_INT) {
										folder.getAttribute(nm).setIntValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
										folder.getAttribute(nm).setBooleanValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
										folder.getAttribute(nm).setDoubleValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_DATE) {
										folder.getAttribute(nm).setDateValue(null);
									} else if (att.getType() == GUIAttribute.TYPE_USER) {
										GUIAttribute at = folder.getAttribute(nm);
										at.setIntValue(null);
										at.setStringValue(null);
										at.setType(GUIAttribute.TYPE_USER);
									} else {
										folder.setValue(nm, "");
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